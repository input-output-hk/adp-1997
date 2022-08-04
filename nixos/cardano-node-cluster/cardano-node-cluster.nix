{ config
, lib
, pkgs
, inputs
, ... }:

with lib;
let
  stateDirBase = "/var/lib/";
  stateDir = "/var/lib/cardano-node";
  runDirBase = "/run/";

  cardanoNodePkgs = inputs.cardano-node.packages."${pkgs.system}";
  cardanoNodeSrc = "${inputs.cardano-node.sourceInfo}";
  mkFilesScript = "${cardanoNodeSrc}/scripts/byron-to-alonzo/mkfiles.sh";

  mkFilesSh = workingDir: pkgs.runCommandLocal "mkFiles" {
    buildInputs = [ pkgs.makeWrapper pkgs.jq ];
  } ''
    mkdir -p $out/bin

    cp ${mkFilesScript} $out/bin/mkfiles-wrapped.sh
    patch $out/bin/mkfiles-wrapped.sh < ${./mkfiles.patch}
    substituteInPlace $out/bin/mkfiles-wrapped.sh \
      --subst-var-by CARDANO_NODE_SRC "${cardanoNodeSrc}" \
      --subst-var-by WORKING_DIR "${workingDir}"

    makeWrapper $out/bin/mkfiles-wrapped.sh $out/bin/mkfiles.sh \
      --prefix PATH : "${lib.makeBinPath [ pkgs.bash cardanoNodePkgs.cardano-node cardanoNodePkgs.cardano-cli pkgs.jq ]}"
  '';

  registerAllScript = pkgs.writeShellScriptBin "register-all" ''
  set -euxo pipefail

  export CARDANO_NODE_SOCKET_PATH=${stateDir}/state/node-bft1/node.sock

  GENESIS_TX_IN=$(cardano-cli genesis initial-txin \
    --testnet-magic 42 \
    --verification-key-file "${stateDir}/state/shelley/utxo-keys/utxo1.vkey")

  cardano-cli transaction build-raw \
    --fee 0 \
    --tx-in "''${GENESIS_TX_IN}" \
    --tx-out "$(cat ${stateDir}/state/addresses/user1.addr)+900000000000" \
    --certificate-file ${stateDir}/state/addresses/pool-owner1-stake.reg.cert \
    --certificate-file ${stateDir}/state/node-pool1/registration.cert \
    --certificate-file ${stateDir}/state/addresses/user1-stake.reg.cert \
    --certificate-file ${stateDir}/state/addresses/user1-stake.deleg.cert \
    --out-file ${stateDir}/state/tx1.txbody

  export FEE=$(cardano-cli transaction calculate-min-fee \
    --testnet-magic 42 \
    --tx-body-file ${stateDir}/state/tx1.txbody \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 4 \
    --genesis ${stateDir}/state/shelley/genesis.json | awk '{ print $1 }')

  cardano-cli transaction build-raw \
    --fee $FEE \
    --tx-in "''${GENESIS_TX_IN}" \
    --tx-out "$(cat ${stateDir}/state/addresses/user1.addr)+$(expr 900000000000 - $FEE)" \
    --certificate-file ${stateDir}/state/addresses/pool-owner1-stake.reg.cert \
    --certificate-file ${stateDir}/state/node-pool1/registration.cert \
    --certificate-file ${stateDir}/state/addresses/user1-stake.reg.cert \
    --certificate-file ${stateDir}/state/addresses/user1-stake.deleg.cert \
    --out-file ${stateDir}/state/tx1.txbody

  cardano-cli transaction sign \
    --signing-key-file ${stateDir}/state/shelley/utxo-keys/utxo1.skey \
    --signing-key-file ${stateDir}/state/addresses/user1-stake.skey \
    --signing-key-file ${stateDir}/state/node-pool1/owner.skey \
    --signing-key-file ${stateDir}/state/node-pool1/shelley/operator.skey \
    --testnet-magic 42 \
    --tx-body-file ${stateDir}/state/tx1.txbody \
    --out-file ${stateDir}/state/tx1.tx

  cardano-cli transaction submit --testnet-magic 42 --tx-file ${stateDir}/state/tx1.tx
  '';
in
{
  options = {};

  config = ( lib.mkMerge [
    {
      users.groups.cardano-node.gid = 10016;
      users.users.cardano-node = {
        description = "cardano-node node daemon user";
        uid = 10016;
        group = "cardano-node";
        isSystemUser = true;
      };
    }
    {
      systemd.services.cardano-node-cluster-setup = {
        description = "Startup script";
        enable  = true;
        wants = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
        script = ''
          cd ${cardanoNodeSrc}
          ${mkFilesSh stateDir}/bin/mkfiles.sh alonzo
        '';
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
          User = "cardano-node";
          Group = "cardano-node";
          WorkingDirectory = stateDir;
          StateDirectory =  lib.removePrefix stateDirBase stateDir;
        };
      };

      systemd.services.cardano-node-cluster-bft1 = {
        description = "BFT node 1";
        enable  = true;
        requires = [ "cardano-node-cluster-setup.service" ];
        after = [ "cardano-node-cluster-setup.service" ];
        wantedBy = [ "multi-user.target" ];
        path = [ pkgs.bash cardanoNodePkgs.cardano-node ];
        serviceConfig = {
          User = "cardano-node";
          Group = "cardano-node";
          ExecStart = "${stateDir}/state/run/node-bft1.sh";
          WorkingDirectory = "${stateDir}/state";
          StateDirectory = lib.removePrefix stateDirBase stateDir;
        };
      };

      systemd.services.cardano-node-cluster-bft2 = {
        description = "BFT node 2";
        enable  = true;
        requires = [ "cardano-node-cluster-setup.service" ];
        after = [ "cardano-node-cluster-setup.service" ];
        wantedBy = [ "multi-user.target" ];
        path = [ pkgs.bash cardanoNodePkgs.cardano-node ];
        serviceConfig = {
          User = "cardano-node";
          Group = "cardano-node";
          ExecStart = "${stateDir}/state/run/node-bft2.sh";
          WorkingDirectory = "${stateDir}/state";
          StateDirectory = lib.removePrefix stateDirBase stateDir;
        };
      };

      systemd.services.cardano-node-cluster-pool1 = {
        description = "Pool node 1";
        enable  = true;
        requires = [ "cardano-node-cluster-setup.service" "cardano-node-cluster-bft1.service" "cardano-node-cluster-bft2.service" ];
        after = [ "cardano-node-cluster-setup.service" "cardano-node-cluster-bft1.service" "cardano-node-cluster-bft2.service" ];
        wantedBy = [ "multi-user.target" ];
        path = [ pkgs.bash cardanoNodePkgs.cardano-node cardanoNodePkgs.cardano-cli pkgs.gawk ];
        script = ''
          echo "Waiting for sockets to appear"
          sleep 30
          ${registerAllScript}/bin/register-all
          ${stateDir}/state/run/node-pool1.sh
        '';
        serviceConfig = {
          User = "cardano-node";
          Group = "cardano-node";
          WorkingDirectory = "${stateDir}/state";
          StateDirectory = lib.removePrefix stateDirBase stateDir;
        };
      };
    }
    {
      assertions = [];
    }
  ]);
}
