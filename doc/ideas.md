# Ideas

This document summarises the core features of Decks and potential,
lower-priority features.

## Potential features

### DSL

Must:

- Content:
  - [ ] Text
  - [ ] Shapes
  - [ ] Declare transitions / animations
  - [ ] Grouping slide elements (e.g. grouping bullet points or array of shapes)
  - [ ] Implicit (auto-generated) or explicit (defined) IDs for each slide
        element (to control animations between slides)
  - [ ] Split up presentation across multiple files
  - [ ] Variables/constants
  - [ ] Speaker notes
- Visual:
  - [ ] Different slide types (title, content, section heading, blank, etc.)
  - [ ] A default theme (background, fonts for heading/body, transitions)
  - [ ] Ability to override theme on specific slides
  - [ ] Applying properties to text/shapes (e.g. font, size, colour)

Could:

- [ ] Evaluate mathematical expressions
- [ ] `for`-loops for generating multiple copies of something with calculated
      properties like paddings/margins (SASS-esque)
- [ ] Refer to attributes of other elements (e.g. `textbox.height`)

### GUI

Must:

- [ ] Preview slide contents
- [ ] Moving slide elements (dragging)
- [ ] Resizing and rotating slide elements
- [ ] Use localhost

Should:

- [ ] Play through slideshow from any slide

Could:

- [ ] Inputting textboxes
- [ ] Icons to add new elements onto the slide
