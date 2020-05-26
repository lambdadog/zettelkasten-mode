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
searching through notes, but unlike `zettelkasten-mode` it requires
your Zettelkasten to "take over" your usage of deft.

While this isn't an outright harmful assumption, I found myself
hacking around its usage of deft frequently and implementing my own
code around it for things like making sure autosaves would happen, if
I opened a note in my Zettelkasten without opening it through deft,
etc. Much of this package evolved from taking all of my hacks and
"ripping" zetteldeft out of them, as it were.

By default, `zettelkasten-mode` wraps its deft calls to avoid
interfering with deft-mode from a user perspective, but if you do wish
to use them together, it intelligently unwraps the calls as an
optimization.
