# Experiments: thoughts

## A) and B)

Handling variables:

- Have a "declare" section then a "draw" section? Pascal-like?
  - No: this can get messy when saving variables across multiple files/slides?
- Just allow X = Y sort of syntax in file without it being in a special section?

Syntax:

- Properties declared in curly-braced, Lua-like or even Haskell-like (setting
  properties of data)
- Use #'s for headings?
  - Then how can properties be applied to it?
  - Experiment a and b syntax inconsistent / janky
- Percent for comments?

## C)

What's going on:

- L1: Variable bindings must use "let"
- L3: We declare two types of heading (more consistent than janky markdown
  thing)
- L13: We declare a type of line with "let" but this doesn't get drawn until L19
  when we just state it
- L20: We draw a version of lineX with properties changed. It gets drawn because
  no "let"

Potential improvements:

- Text { } idea seems not fitting
- How about grouping elements?
- How about elements having the same ID so they can be correctly animated?
- Comments?

## D)

- Text block has content equals something - this is more consistent, as now
  everything in braces is a K-V pair
  - Lua-like syntax for multiple strings?
  - `Text` should represent only textbox as shapes can have text on them?
- Hash for comment?
- Speaker notes are separate data type?
  - By getting "drawn" it just is saved as a note
  - If declared using "let" but never stated, then it never would get included

Todo:

- Animations?
- How are individual slides defined?
- I'm not sure I like the Lua-like multi-line string thing
- What about defining bold, italic, bold-italic, or monospace text?
  - This is easy in markdown
  - Have a way to incorporate Markdown syntax?

## E)

- Maybe parentheses for Markdown-style text?

## F)

- Trying more Markdown-esque syntax

Is it better just to go for Markdown syntax, with "imported" (S)CSS and JS files
to add styling and SVG shape definitions?
