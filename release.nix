# Used for building and testing on build servers like Hydra
with import ./.;
panhandle.components // {
  tests = panhandle.components.tests // {
    integration = (import <nixpkgs> {}).runCommand "panhandle-integration-tests"
      {
        buildInputs = [
          pandoc.components.exes.pandoc
          panhandle.components.exes.panhandle
        ];
      }
      ''
        bash "${./test.sh}"
        echo pass > "$out"
      '';
  };
}
