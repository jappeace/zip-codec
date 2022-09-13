import ./pin.nix {
  config = {

    packageOverrides = pkgs: {
        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
          packageOverrides = hpNew: hpOld:
            let
              lib = pkgs.haskell.lib;
            in
            {
            zip-codec = hpNew.callPackage ../default.nix {};

            };
        };
    };
  };
}
