# Decks

A **D**SL and **E**ditor for **C**reating **S**lides.

## Project structure

|            |                                                        |
| ---------- | ------------------------------------------------------ |
| `cli/`     | Decks command-line interface (Haskell)                 |
| `scratch/` | Rough notes and drafts of ideas                        |
| `scripts/` | Helper scripts for setting things up                   |
| `vim/`     | Vim plugin for syntax highlighting for `*.decks` files |
| `web/`     | Web-based user interface for the WYSIWYG editor        |

## About

### Motivation

Creating presentations is a task that most computer scientists will undertake at
some point during their careers. There are typically two approaches to this
task:

1. Writing PDF slides with LaTeX's `beamer` package.
2. Making slides with a [WYSIWYG](https://en.wikipedia.org/wiki/WYSIWYG) editor.

Decks combines the advantages of both the above approaches, and consists of:

- A domain-specific language (DSL) is used for writing animated presentations;
  and
- An accompanying WYSIWYG editor.

As such, the slides can be edited both via code as well as through a graphical
interface. Edits made via the DSL will be reflected in the graphical interface;
similarly edits made in the WYSIWYG will cause the DSL code to update.

### License

This project is licensed under the
[MIT License](https://choosealicense.com/licenses/mit/).
