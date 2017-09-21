#!/usr/bin/env bash
set -e
set -o pipefail

command -v pandoc || {
    echo "Couldn't find pandoc binary, aborting" 1>&2
    exit 1
}

function run {
    if command -v cabal2nix
    then
        EXPR=$(cabal2nix ./.)
        nix-shell -p "(haskellPackages.callPackage ($EXPR) {})" --run 'panhandle'
    else
        cabal run panhandle
    fi
}

MARKDOWN="*foo*"
echo "Got Markdown '$MARKDOWN'"

JSON=$(echo "$MARKDOWN" | pandoc -f markdown -t json)
echo "Got JSON '$JSON'"

# shellcheck disable=SC2016
UNWRAP=$(echo '```{.unwrap}'; echo "$JSON"; echo '```')
echo "Got unwrap '$UNWRAP'"

META=$(echo "$UNWRAP" | pandoc -f markdown -t json)
echo "Got meta '$META'"

UNWRAP=$(echo "$META" | run) || {
    echo "Failed to unwrap" 1>&2
    exit 1
}
echo "Got unwrapped '$UNWRAP'"

FINAL=$(echo "$UNWRAP" | pandoc -f json -t html)
echo "Got HTML '$FINAL'"

echo "$FINAL" | grep '<em>foo</em>' || {
    echo "Didn't unwrap *foo* in '$FINAL'" 1>&2
    exit 1
}
