import ./pin.nix {
  config = {

    packageOverrides = pkgs: {
        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
            zip-codec = hpNew.callPackage ../default.nix {};
            };
        };
    };
  };
}
