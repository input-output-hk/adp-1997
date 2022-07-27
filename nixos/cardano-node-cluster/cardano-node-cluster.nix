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
    buildInputs = [ pkgs.makeWrapper ];
  } ''
    mkdir -p $out/bin

    cp ${mkFilesScript} $out/bin/mkfiles-wrapped.sh
    substituteInPlace $out/bin/mkfiles-wrapped.sh \
      --replace 'ROOT=example' 'ROOT=${workingDir}/state' \
      --replace '../configuration' '${cardanoNodeSrc}/configuration' \
      --replace 'echo "EnableLogMetrics: False" >> ''${ROOT}/configuration.yaml' \
                'chmod +w ''${ROOT}/configuration.yaml; echo "EnableLogMetrics: False" >> ''${ROOT}/configuration.yaml'

    makeWrapper $out/bin/mkfiles-wrapped.sh $out/bin/mkfiles.sh \
      --prefix PATH : "${lib.makeBinPath [ pkgs.bash cardanoNodePkgs.cardano-node cardanoNodePkgs.cardano-cli ]}"
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
      # oneshot service start allows to easily control all instances at once.
      systemd.services.cardano-node-cluster = {
        description = "Startup script";
        enable  = true;
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
          User = "cardano-node";
          Group = "cardano-node";
          ExecStart = "${pkgs.bash}/bin/sh -c 'cd ${cardanoNodeSrc}; ${mkFilesSh stateDir}/bin/mkfiles.sh alonzo'";
          WorkingDirectory = stateDir;
          StateDirectory =  lib.removePrefix stateDirBase stateDir;
        };
      };
    }
    {
      assertions = [];
    }
  ]);
}
