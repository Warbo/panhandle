{ haskellPackages, lib }: haskellPackages.haskellSrc2nix {
  name = "panhandle";
  src = lib.cleanSource ./.;
}
