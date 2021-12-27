Reveal.initialize({});
console.log("Reveal initialized");

/**
 * Editing mode
 */
let editorEditable = false;

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
  const content = document.querySelector(".reveal .slides")?.innerHTML;
  console.log(content);
}
