######################################################################
# Overlay containing extra Haskell packages which we build with
# Haskell.nix, but which aren't part of our project's package set
# themselves.
#
# These are e.g. build tools for developer usage.
#
# NOTE: Updating versions of Haskell tools
#
# Modify the tool version number to a different Hackage version.
# If you have chosen a recent version, you may also need to
# advance the "index-state" variable to include the upload date
# of your package.
#
# When increasing the "index-state" variable, it's likely that
# you will also need to update Hackage.nix to get a recent
# Hackage package index.
#
#   nix flake lock --update-input haskellNix
#
# After changing tool versions, you can update the generated
# files which are cached in ./nix/materialized. Run this
# command, follow the instructions shown, then commit the
# updated files.
#
#   ./nix/regenerate.sh
#
######################################################################

pkgs: super: let
  tools = {
    cabal-install.exe               = "cabal";
    cabal-install.version           = "3.6.0.0";
    haskell-language-server = {
      version = "1.5.0.0";
      modules = [{ reinstallableLibGhc = false; }];
    };
    hie-bios = {};
    hoogle.version                  = "5.0.18.1";
    hlint.version                   = "3.3.1";
    lentil.version                  = "1.5.2.0";
    stylish-haskell.version         = "0.11.0.3";
    weeder.version                  = "2.1.3";
    fourmolu = {
      version                = "0.4.0.0";

      # cabalProjectLocal = ''
      # constraints: Cabal >= 3.6.0.0
      # '';
    };
  };

  # Use cabal.project as the source of GHC version and Hackage index-state.
  inherit (pkgs.customLib.cabalProjectIndexState ../cabal.project)
    index-state compiler-nix-name;

  hsPkgs = pkgs.lib.mapAttrs mkTool tools;

  mkTool = name: args: pkgs.haskell-nix.hackage-package ({
    inherit name index-state compiler-nix-name;
  } // builtins.removeAttrs args ["exe"]);

  # Get the actual tool executables from the haskell packages.
  mapExes = pkgs.lib.mapAttrs (name: hsPkg: hsPkg.components.exes.${tools.${name}.exe or name});

in {
  haskell-build-tools = pkgs.recurseIntoAttrs
    ((super.haskell-build-tools or {})
      // mapExes hsPkgs
      // {
        haskell-language-server-wrapper = pkgs.runCommandNoCC "haskell-language-server-wrapper" {} ''
          mkdir -p $out/bin
          hls=${hsPkgs.haskell-language-server.components.exes.haskell-language-server}
          ln -s $hls/bin/haskell-language-server $out/bin/haskell-language-server-wrapper
        '';
      });

  # These overrides are picked up by cabalWrapped in iohk-nix
  cabal = pkgs.haskell-build-tools.cabal-install;
  cabal-install = pkgs.haskell-build-tools.cabal-install;
}
