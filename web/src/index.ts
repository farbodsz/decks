//------------------------------------------------------------------------------
// Types
//------------------------------------------------------------------------------

/**
 * Return type of Reveal.getIndices().
 */
interface SlideIndices {
  h: number;
  v: number;
}

//------------------------------------------------------------------------------
// Variables and init
//------------------------------------------------------------------------------

const HOST = "localhost:8081/decks";

/**
 * Editing mode
 */
let editorEditable = false;

/**
 * Slide indices for the slide currently being viewed.
 */
let currSlide: SlideIndices = { h: 0, v: 0 };

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
  console.log("[reveal] Reveal initialized");
}

function revealRefresh() {
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
  console.log("[editor] Loading content");
  currSlide = Reveal.getIndices();

  // Setup WebSocket
  console.log("[ws] Creating WebSocket...");
  const webSocket = new WebSocket("ws://" + HOST);

  webSocket.onopen = function () {
    console.log("[ws] WebSocket connection opened");
    webSocket.onmessage = function (event: MessageEvent<any>) {
      console.log("[ws] Received message");

      // Remove quotations and line breaks
      let received = event.data.trim();
      var cleanedContent = received.replace(/\\"/g, '"');
      cleanedContent = received.substring(1, received.length - 3);

      editorSetContent(cleanedContent);
    };
  };

  webSocket.onclose = function (event: CloseEvent) {
    console.log("[ws] WebSocket connection closed");
    console.log(event.code);
    console.log(event.reason);
  };

  webSocket.onerror = function (event: Event) {
    console.log("[ws] WebSocket error");
    console.log(event);
  };
}

/**
 * Sets the presentation data to the editor content GUI.
 *
 * This should be invoked the first time the presentation content is read from
 * the backend.
 */
function editorSetContent(data: string) {
  const contentContainer = document.getElementById("editor-content");
  if (!contentContainer) {
    console.error("No content container found");
    return;
  }

  contentContainer.innerHTML = data;
  revealRefresh();
  editorSetupEditClicks();
}

/**
 * Adds onclick events for all elements, to make them editable on click.
 */
function editorSetupEditClicks() {
  console.log("[editor] Setting up clicks");
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
  currSlide = Reveal.getIndices();
  editorEditable = editable;
  const statusTxt = document.getElementById("status-mode")!;
  statusTxt.textContent = editable ? "Editing mode" : "Previewing mode";
}

/**
 * Outputs the presentation HTML
 */
function editorSave() {
  const content = document.getElementById("editor-content")?.innerHTML;
  console.log(content); // TODO:
}

//------------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------------

initDecks();
