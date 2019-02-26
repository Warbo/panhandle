# Used for building and testing on build servers like Hydra
with builtins;
with import ./nixpkgs.nix;
with lib;
with {
  nixpkgsVersion = fileContents (path + "/.version");
  ghcVersion     = haskellPackages.ghc.version;
};
{
  "nixpkgs${nixpkgsVersion}-ghc${ghcVersion}-panhandle" =
    haskellPackages.panhandle;

  "tests" = runCommand "panhandle-tests"
    {
      buildInputs = [
        cabal-install
        haskellPackages.pandoc

        # We include panhandle, since this will guarantee our dependencies
        (haskellPackages.ghcWithPackages (h: [ h.panhandle ]))
      ];
      dir         = filterSource
        (path: _:
          with { f = baseNameOf path; };
          !(hasSuffix ".nix" f || elem f [
            ".git" ".gitignore" ".issues" "dist" "dist-newstyle"
            "README.md" "result"
          ]))
        ./.;
    }
    ''
      export HOME="$PWD"
      cp -r "$dir" dir
      chmod 777 -R dir
      cd dir
      ./test.sh
      echo pass > "$out"
    '';
}
