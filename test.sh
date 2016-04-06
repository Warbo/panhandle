#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash pandoc

hsConfig

MARKDOWN="*foo*"
echo "Got Markdown '$MARKDOWN'"

JSON=$(echo "$MARKDOWN" | pandoc -f markdown -t json)
echo "Got JSON '$JSON'"

# shellcheck disable=SC2016
UNWRAP=$(echo '```{.unwrap}'; echo "$JSON"; echo '```')
echo "Got unwrap '$UNWRAP'"

META=$(echo "$UNWRAP" | pandoc -f markdown -t json)
echo "Got meta '$META'"

UNWRAP=$(echo "$META" | cabal run -v0) || {
    echo "Failed to unwrap" >> /dev/stderr
    exit 1
}
echo "Got unwrapped '$UNWRAP'"

FINAL=$(echo "$UNWRAP" | pandoc -f json -t html)
echo "Got HTML '$FINAL'"

echo "$FINAL" | grep '<em>foo</em>' || {
    echo "Didn't unwrap *foo* in '$FINAL'" >> /dev/stderr
    exit 1
}
