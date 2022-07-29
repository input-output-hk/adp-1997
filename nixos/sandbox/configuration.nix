{ modulesPath, config, pkgs, lib, inputs, system, ... }:

let
  cardanoNodePackages = inputs.cardano-node.packages."${pkgs.system}";
  plutusProgs = inputs.self.packages."${pkgs.system}".always-succeed;
in
{
  imports = [
    ../cardano-node-cluster/cardano-node-cluster.nix
  ];

  environment.systemPackages = [
    # Make cardano-testnet and required packages available on machine
    cardanoNodePackages.cardano-cli
    cardanoNodePackages.cardano-node
    cardanoNodePackages.cardano-node-chairman
    cardanoNodePackages.cardano-testnet
    # "/nix/store/19022nw7i67k3r1yakxpll0abf9a8fn8-source"
    pkgs.git
    pkgs.tree
    pkgs.jq
    (pkgs.writeShellScriptBin "start-cluster" (builtins.readFile ../../script.sh))
    plutusProgs
  ];

  environment.interactiveShellInit = ''
    export CARDANO_NODE_SRC=${inputs.cardano-node.sourceInfo}
    export CARDANO_NODE_SOCKET_PATH=/var/lib/cardano-node/state/node-bft1/node.sock
  '';

  # For flakes
  nix = {
    # package = pkgs.nix_2_7;
    extraOptions = ''
      builders = @/etc/nix/machines
      builders-use-substitutes = true
      experimental-features = nix-command flakes
    '';
  };

  # For cached builds
  nix.binaryCaches = [
    "https://cache.nixos.org"
    "https://cache.iog.io"
  ];
  nix.binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];

  # TODO remove
  users.users.root.initialPassword = "";
}
