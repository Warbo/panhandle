# Used for building and testing on build servers like Hydra
{
  nixpkgs ? import (fetchTarball {
    name   = "nixpkgs1909";
    url    = https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz;
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  }) {}
}:
with {
  pkgs = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      panhandle = self.callPackage (nixpkgs.callPackage ./. {}) {};
    };
  };
};
pkgs.panhandle
