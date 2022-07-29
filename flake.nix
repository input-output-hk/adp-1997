{
  description = "Infrastructure for exploring ADP-1997";

  inputs = {
    cardano-node.url = "github:input-output-hk/cardano-node?ref=refs/tags/1.35.2";
    nix.url = "github:NixOS/nix/master";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, haskellNix, flake-utils, iohkNix, ... } @ inputs:
    let
      inherit (nixpkgs) lib;
      inherit (lib) head systems mapAttrs recursiveUpdate mkDefault
        getAttrs optionalAttrs nameValuePair attrNames;
      inherit (flake-utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith collectExes;

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.utils
        iohkNix.overlays.crypto
        (final: prev: {
          gitrev = self.rev or "dirty";
          commonLib = lib // iohkNix.lib;
        })
        (import ./nix/build-tools.nix)
        (import ./nix/lib.nix)
      ];
    in
    flake-utils.lib.eachDefaultSystem (system:
      rec {
        legacyPackages = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        pkgs = legacyPackages;

        project = (import ./nix/haskell.nix pkgs.haskell-nix);

        flake = project.flake {};
        packages = collectExes flake.packages;

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
