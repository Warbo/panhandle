with builtins;
with rec {
  nixpkgsSrc = fetchTarball {
    name   = "nixpkgs1909";
    url    = https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz;
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  };

  # TODO: Point this at IOHK's repo once i686 support is included
  # See https://github.com/angerman/old-ghc-nix/pull/4
  haskellNixSrc = fetchTarball {
    name   = "haskell-nix";
    url    = https://github.com/Warbo/haskell.nix/archive/499a761.tar.gz;
    sha256 = "1pnkywswfa71hgc2c3g2cijfk9nysbpyh6jjh455h810n4yhs522";
  };

  pkgs = import nixpkgsSrc { overlays = import "${haskellNixSrc}/overlays"; };
};
pkgs.haskell-nix.cabalProject {
  src         = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  ghc         = pkgs.buildPackages.pkgs.haskell-nix.compiler.ghc865;
  index-state = "2020-01-11T00:00:00Z";

  # Avoid 'Neither the Haskell package set or the Nixpkgs package set contain
  # the package: alex (build tool dependency).'
  # The issue AFAIK seems to be that "bootstrap" packages (used by GHC) can
  # either be fixed, to avoid excessive rebuilding; or reinstallable, to allow
  # overrides, etc. There are some packages which are the wrong way around
  # somewhere in the dependencies of lazysmallcheck2012 (not sure which, maybe
  # terminfo), so we specify them explicitly here. Found this via trial and
  # error, thanks to https://github.com/input-output-hk/haskell.nix/issues/221
  modules = [
    { reinstallableLibGhc = true; }
    {
      nonReinstallablePkgs = [
        "array"
        "array"
        "base"
        "binary"
        "bytestring"
        "Cabal"
        "containers"
        "deepseq"
        "directory"
        "filepath"
        "ghc"
        "ghc-boot"
        "ghc-boot"
        "ghc-boot-th"
        "ghc-compact"
        "ghc-heap"
        "ghc-prim"
        "ghc-prim"
        "ghci"
        "ghcjs-prim"
        "ghcjs-th"
        "haskeline"
        "hpc"
        "integer-gmp"
        "integer-simple"
        "mtl"
        "parsec"
        "pretty"
        "process"
        "rts"
        "stm"
        "template-haskell"
        "terminfo"
        "text"
        "time"
        "transformers"
        "unix"
        "Win32"
        "xhtml"
      ];
    }
  ];
}
