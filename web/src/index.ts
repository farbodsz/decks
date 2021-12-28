const BACKEND_URL = "http://localhost:8081/decks";

/**
 * Editing mode
 */
let editorEditable = false;

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
    })
    .catch((error) => console.error("Error " + error.message));
}

/**
 * Toggles the editing mode (edit/preview) and updates UI elements reflecting
 * this.
 */
function editorToggleEdit() {
  editorEditable = !editorEditable;

  document
    .querySelectorAll("section")
    .forEach((el) =>
      el.setAttribute("contentEditable", editorEditable ? "true" : "false")
    );

  const editButton = document.getElementById("button-edit");
  if (editButton) editButton.textContent = editorEditable ? "Lock" : "Edit";

  const statusTxt = document.getElementById("status-mode");
  if (statusTxt)
    statusTxt.textContent = editorEditable ? "Editing mode" : "Previewing mode";
}

/**
 * Outputs the presentation HTML
 */
function editorSave() {
  const content = document.getElementById("editor-content")?.innerHTML;
  console.log(content);
}

revealInit();
