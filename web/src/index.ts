//------------------------------------------------------------------------------
// Variables and init
//------------------------------------------------------------------------------

const BACKEND_URL = "http://localhost:8081/decks";

/**
 * Editing mode
 */
let editorEditable = false;

/**
 * Initializes the Decks UI
 */
function initDecks() {
  revealInit();
  editorLoadContent();
}

//------------------------------------------------------------------------------
// Reveal helpers
//------------------------------------------------------------------------------

function revealInit() {
  Reveal.initialize({
    // Disables the default reveal.js slide layout (scaling and centering)
    // so that you can use custom CSS layout
    // NOTE: Typings not up-to-date so some fields flagged as error.
    // See https://github.com/DefinitelyTyped/DefinitelyTyped/pull/57863
    // @ts-ignore
    disableLayout: true,
  });
  console.log("Reveal initialized");
}

function revealRefresh(currSlide: { h: number; v: number }) {
  Reveal.sync();
  Reveal.slide(currSlide.h, currSlide.v);
}

//------------------------------------------------------------------------------
// Decks editor
//------------------------------------------------------------------------------

/**
 * Replaces the HTML presentation with the one loaded from the backend.
 */
function editorLoadContent() {
  console.log("Loading content");
  const currSlide = Reveal.getIndices();

  fetch(BACKEND_URL)
    .then((response) => response.json())
    .then((data) => {
      const contentContainer = document.getElementById("editor-content");
      if (!contentContainer) {
        console.error("No content container found");
        return;
      }

      contentContainer.innerHTML = data;
      revealRefresh(currSlide);
      editorSetupEditClicks();
    })
    .catch((error) => console.error("Error " + error.message));
}

/**
 * Adds onclick events for all elements, to make them editable on click.
 */
function editorSetupEditClicks() {
  console.log("Setting up clicks");
  const container = document.querySelector("#editor-content")!;

  // Limit elements that can be edited using two criteria:
  //  - must be from the DSL, i.e. contain `data-decks-start` attribute
  //  - must be part of the slide deck, i.e. be inside `#editor-content`
  const elements: NodeListOf<HTMLElement> = container.querySelectorAll(
    "#editor-content *[data-decks-start]"
  );

  elements.forEach((el: HTMLElement) => {
    el.onclick = function () {
      el.setAttribute("contentEditable", "true");
      editorUpdateMode(true);
    };

    el.onblur = function () {
      el.setAttribute("contentEditable", "false");
      editorUpdateMode(false);
    };
  });
}

function editorUpdateMode(editable: boolean) {
  editorEditable = editable;
  const statusTxt = document.getElementById("status-mode")!;
  statusTxt.textContent = editable ? "Editing mode" : "Previewing mode";
}

/**
 * Outputs the presentation HTML
 */
function editorSave() {
  const content = document.getElementById("editor-content")?.innerHTML;
  console.log(content);
}

//------------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------------

initDecks();
