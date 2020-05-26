# `zettelkasten-mode` for Emacs

`zettelkasten-mode` is an implementation of a Zettelkasten software in
Emacs.

## What is a Zettelkasten?

A Zettelkasten is a note-taking system created by a German
sociologist, in which you create notes in a flat structure and through
links in your notes, a graph structure naturally emerges. It's similar
to techniques such as mind-mapping but doesn't require you to
pre-establish a hierarchy when creating your notes.

## Comparison to other similar Emacs packages

### Zetteldeft

Zetteldeft is another zettelkasten package written by [Elias
Storms](https://www.eliasstorms.net/) for Emacs. It also uses deft for
searching through your notes, but it makes several decisions I'm not a
fan of:

  + Zetteldeft "takes over" your usage of deft, meaning it's
    impossible without significant hacking to have a separate
    deft-directory if you want to.
	 - `zettelkasten-mode` instead chooses to wrap deft-mode calls to
       avoid dirtying the global deft installation, but is capable of
       intelligently unwrapping if you do choose to set your
       deft-directory to the same value as your
       zettelkasten-directory.
  + It supports all plain-text formats, which is great if you need
    that, but also means it offers no integration with the formats it
    supports.
	 - In contrast, `zettelkasten-mode` deeply integrates with
       `org-mode` and chooses to forgo interoperability with other
       markup formats.
