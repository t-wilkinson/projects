{ nixpkgs ? import ./nixos-19-09.nix }:
let
  overlay = self: super: {
    myHaskellPackages =
      super.haskell.packages.ghc865.override (old: {
        overrides = self.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            project = self.haskell.lib.dontCheck (hself.callCabal2nix "project" /home/trey/dev/sites/all/backend {});
            ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
            ghcWithPackages = hself.ghc.withPackages;
            ## Overridden Package here!
            req = hself.callHackageDirect {
              pkg = "req";
              ver = "3.1.0";
              sha256 = "0rwbdmp2g74yq7fa77fywsgm0gaxcbpz9m6g57l5lmralg95cdxh";
            } {};
          });
      });
  };

  pkgs = import nixpkgs {
    overlays = [overlay];
  };

  drv = pkgs.myHaskellPackages.callCabal2nix "nix-haskell" ./nix-haskell.cabal {};

  ghcidSrc = pkgs.fetchFromGitHub {
    owner = "ndmitchell";
    repo = "ghcid";
    rev = "e3a65cd986805948687d9450717efe00ff01e3b5";
    sha256 = "1xhyl7m3yaka552j8ipvmrbfixgb8x0i33k2166rkiv7kij0rhsz";
  };

  ormoluSrc = pkgs.fetchFromGitHub {
    owner = "tweag";
    repo = "ormolu";
    rev = "d8c3e553139892c651e5efd548d95a61479fa800";
    sha256 = "1l8ai8pc373iasa3s2lpl0ddxhjpa8qbzaar1vs92pxzykw5xs7x";
  };

  ghc-lib-parser = pkgs.haskell.lib.dontCheck
          (pkgs.haskellPackages.callHackageDirect
            { pkg = "ghc-lib-parser";
              ver = "8.10.1.20200523";
              sha256 = "1fnhqb9l0cg58lalrrn4ms48rnnzlyb7dqa9h2g21m9287q5y6gs";
            } {}
          );

  extra = pkgs.haskell.lib.dontCheck
          (pkgs.haskellPackages.callHackageDirect
            { pkg = "extra";
              ver = "1.6.20";
              sha256 = "0fy5bkn8fn3gcpxksffim912q0ifk057x0wmrvxpmjjpfi895jzj";
            } {}
          );

  drvWithTools = drv.env.overrideAttrs (
    old: with pkgs.myHaskellPackages; {
      nativeBuildInputs = old.nativeBuildInputs ++ [
        brittany
        (pkgs.haskell.lib.justStaticExecutables
          (pkgs.haskell.packages.ghc865.callCabal2nix "ghcid" ghcidSrc {inherit extra;}))
        (pkgs.haskell.lib.justStaticExecutables
          (pkgs.haskell.packages.ghc865.callCabal2nix "ormolu" ormoluSrc {inherit ghc-lib-parser;}))
      ];
    }
  );

in
  drvWithTools
