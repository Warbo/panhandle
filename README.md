# PanHandle #

This software is released into the Public Domain
 -- Chris Warburton <chriswarbo@gmail.com>, 2014-09-30

## Usage ##

You'll need some way to run Haskell. Check your package manager or go to
https://www.haskell.org/platform/ to get a compiler or a `runhaskell`
interpreter.

You'll also need Pandoc available as a library, which you can get from your
package manager or with `cabal install pandoc`, and will probably want the
`pandoc` command available too.

To use PanHandle, invoke it as a Pandoc "filter", like this:

`pandoc --filter ./panhandle input_file > output_file`

## Intro ##

PanHandle is a simple Haskell script using PanDoc. It allows code blocks and
lines in PanDoc-compatible documents, eg. Markdown, to be "unwrapped" and become
part of the overall document.

Any code blocks or lines with an "unwrap" class will have their contents parsed
using Pandoc, then spliced into the document (inside a Div or Span). The content
must be in "pandoc-json" format, which you can get by passing the `-t json`
option to Pandoc.

For example, this Markdown list:

     - A Markdown
     - List

Converts to this JSON:

```
[{"unMeta":{}},[{"t":"BulletList","c":[[{"t":"Plain","c":[{"t":"Str","c":"A"},{"t":"Space","c":[]},{"t":"Str","c":"Markdown"}]}],[{"t":"Plain","c":[{"t":"Str","c":"List"}]}]]}]]
```

Which we can splice into a document:

    Mumble mumble

    ```{.unwrap}
    [{"unMeta":{}},[{"t":"BulletList","c":[[{"t":"Plain","c":[{"t":"Str","c":"A"},{"t":"Space","c":[]},{"t":"Str","c":"Markdown"}]}],[{"t":"Plain","c":[{"t":"Str","c":"List"}]}]]}]]
    ```

    Groan groan

To give:

    Mumble mumble

     - A Markdown
     - List

    Groan groan

## Usage Notes ##

### Wrappers ###

Code blocks will become Pandoc-native Div blocks, code lines will become
Pandoc-native Span elements. These may affect rendering in some formats, eg.
LaTeX, and may also affect post-processors, eg. other Pandoc scripts and CSS.

### Attributes ###

All of the code block's attributes except for the "unwrap" class will be applied
to the resulting Div or Span:

````
```{#foo .bar .upper .unwrap baz="quux" something="nice"}
[{"unMeta":{}},[{"t":"Para","c":[{"t":"Str","c":"Some"},{"t":"Space","c":[]},{"t":"Emph","c":[{"t":"Str","c":"emphasised"}]},{"t":"Space","c":[]},{"t":"Str","c":"text"}]}]]
```
````

Will become:

```
<div id="foo" class="bar upper" baz="quux" something="nice">
Some *emphasised* text
</div>
```

### Replacement Order ###

PanHandle operates top-down, so blocks can be nested. For example:

    Level 1A

    `````{.unwrap}
    Level 2A

    ```{.unwrap}
    Level 3A
    ```

    ```
    Level 3B
    ```

    Level 2B
    `````

    Level 1B

Will become:

    Level 1A

    Level 2A

    Level 3A

    ```
    Level 3B
    ```

    Level 2B

    Level 1B

All code blocks are unwrapped first, then all inline code.

### Inline Snippets ###

Here's an example of PanHandle working on inline code snippets:

    I hope the following is `[{"unMeta":{}},[{"t":"Para","c":[{"t":"Emph","c":[{"t":"Str","c":"emphasised"}]}]}]]`{.unwrap}.

Will become:

    I hope the following is *emphasised*.

### No Straddling ###

PanHandle operates by splicing syntax trees together, *not* via text
replacement.

One consequence is that formatting cannot 'straddle two levels' of a document.
As an example, if we put asterisks inside and outside a code snippet (the JSON
corresponds to `be* emphasised`):

    This will *not `[{"unMeta":{}},[{"t":"Para","c":[{"t":"Str","c":"be*"},{"t":"Space","c":[]},{"t":"Str","c":"emphasised"}]}]]`{.unwrap}.

They will remain as asterisks:

    This will \*not be\* emphasised.

They *will not* join together for emphasis, like this:

    This will *not be* emphasised.

### PanPipe ###

PanHandle may be useful to you as a standalone script, but it was originally
created to augment PanPipe. PanPipe can send the contents of code blocks to the
stdin of a UNIX shell command, and dump the stdout back into the block.

PanHandle allows these results to escape their blocks and become part of the
document.