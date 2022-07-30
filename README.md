# Decks

A **D**SL and **E**ditor for **C**reating **S**lides.

Submitted with a detailed report as part of my final year project at the
University of Warwick (CS310).

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

### Aims

The aim of the project is to demonstrate the feasibility of a system combining
both textual and graphical approaches to writing presentations. As such, it is
more of a minimum-viable product, and is missing some features typical in other
presentation systems.

### License

This project is licensed under the
[MIT License](https://choosealicense.com/licenses/mit/).
