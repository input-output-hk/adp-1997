{
  description = "Infrastructure for exploring ADP-1997";

  inputs = {
    cardano-node.url = "github:input-output-hk/cardano-node?ref=refs/tags/1.35.2";
    nix.url = "github:NixOS/nix/master";
    nixpkgs.follows = "haskellNix/nixpkgs-2111";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, haskellNix, flake-utils, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
      in
      {
        legacyPackages = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
        };

        devShell = pkgs.mkShell {
          CARDANO_NODE_SRC = "${inputs.cardano-node.sourceInfo}";

          nativeBuildInputs = [
            pkgs.lsof
            pkgs.tree
            pkgs.jq
            inputs.cardano-node.packages."${system}".cardano-cli
            inputs.cardano-node.packages."${system}".cardano-node
          ];
        };
      }) //
    {
      nixosConfigurations =
        let
          nixosSystem = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem;
        in {
          sandbox = nixosSystem (import ./nixos/sandbox inputs);
        };

      nixosModules = {
        cardano-node-cluster = (import ./nixos/cardano-node-cluster inputs);
      };
    };
}
