# Used for building and testing on build servers like Hydra
with import ./.;
panhandle.components.exes // {
  inherit (panhandle.components) library;
  tests = {
    # FIXME: error: attribute 'configFiles' missing, at /nix/store/5sl7sk4pq1di6ainl5p8rzs98rn6q24v-haskell.nix/builder/make-config-files.nix:75:16
    #unit        = panhandle.components.tests.tests;

    integration = (import <nixpkgs> {}).runCommand "panhandle-integration-tests"
      {
        buildInputs = [
          pandoc.components.exes.pandoc
          panhandle.components.exes.panhandle
        ];
      }
      ''
        "${./test.sh}"
        echo pass > "$out"
      '';
  };
}
