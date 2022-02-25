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

/**
 * Notification sent from the frontend to the backend.
 */
interface DecksNotification {
  notifType: DecksNotificationType;
  notifSrc: DecksSrcRange;
  notifNewVal: string;
}

type DecksNotificationType = "NotifTextChanged" | "NotifTextInserted";

interface DecksSrcRange {
  rangeStart: DecksSourcePos;
  rangeEnd: DecksSourcePos;
}

interface DecksSourcePos {
  path: string;
  line: number;
  col: number;
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
 * WebSocket connection.
 */
let webSocket: WebSocket | null;

/**
 * Notification representing the current edit made to the presentation, pending
 * synchronisation with the backend. This may be null, if no edits have been
 * made since the presentation was last synced with the backend.
 */
let currNotification: DecksNotification | null;

/**
 * Initializes the Decks UI
 */
function initDecks() {
  revealInit();
  editorSetupConnection();
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
  console.info("[reveal] Reveal initialized");
}

function revealRefresh() {
  Reveal.sync();
  Reveal.slide(currSlide.h, currSlide.v);
}

//------------------------------------------------------------------------------
// Decks editor
//------------------------------------------------------------------------------

/**
 * Creates a connection to the backend via WebSocket.
 *
 * When a WebSocket message is received, the HTML presentation content is
 * updated.
 */
function editorSetupConnection() {
  console.info("[editor] Loading content");
  currSlide = Reveal.getIndices();

  // Setup WebSocket
  console.info("[ws] Creating WebSocket...");
  webSocket = new WebSocket("ws://" + HOST);

  webSocket.onopen = function () {
    console.info("[ws] WebSocket connection opened");
    webSocket!.onmessage = function (event: MessageEvent<any>) {
      console.info("[ws] Received message");

      // Remove quotations and line breaks
      let received = event.data.trim();
      var cleanedContent = received.replace(/\\"/g, '"');
      cleanedContent = received.substring(1, received.length - 3);

      editorSetContent(cleanedContent);
    };
  };

  webSocket.onclose = function (event: CloseEvent) {
    console.info("[ws] WebSocket connection closed");
    console.info(event);
  };

  webSocket.onerror = function (event: Event) {
    console.error("[ws] WebSocket error");
    console.error(event);
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
  const container = document.querySelector("#editor-content")!;

  // Limit elements that can be edited using two criteria:
  //  - must be from the DSL, i.e. contain `data-decks-start` attribute
  //  - must be part of the slide deck, i.e. be inside `#editor-content`
  const elements: NodeListOf<HTMLElement> = container.querySelectorAll(
    "#editor-content span[data-decks-start]"
  );

  elements.forEach((el: HTMLElement) => {
    el.onclick = function () {
      el.setAttribute("contentEditable", "true");
      editorUpdateMode(true);
    };

    el.onblur = function () {
      el.setAttribute("contentEditable", "false");
      editorUpdateMode(false);
      editorSendNotification(currNotification);
    };

    el.oninput = function (_: Event) {
      const range = elGetDecksRange(el);
      if (range == null) {
        return;
      }

      // Update the current notification when the input has changed
      currNotification = {
        notifType: "NotifTextChanged",
        notifSrc: range,
        notifNewVal: br2nl(el.innerText),
      };
    };
  });
}

function elGetDecksRange(el: Element): DecksSrcRange | null {
  const start = el.getAttribute("data-decks-start");
  const end = el.getAttribute("data-decks-end");

  if (start == null || end == null) {
    console.error("[editor] No Decks range attributes found on this element.");
    return null;
  }

  const parseColonSep = (str: string): number[] => {
    str = str.substring(2, str.length - 2);
    const [sstart, send] = str.split(":");
    return [parseInt(sstart), parseInt(send)];
  };

  const [startl, startc] = parseColonSep(start);
  const [endl, endc] = parseColonSep(end);

  return {
    rangeStart: {
      path: "", // TODO
      line: startl,
      col: startc,
    },
    rangeEnd: {
      path: "", // TODO
      line: endl,
      col: endc,
    },
  };
}

function br2nl(str: string): string {
  return str.replace(/<br\s*\/?>/gm, "\n");
}

function editorUpdateMode(editable: boolean) {
  currSlide = Reveal.getIndices();
  editorEditable = editable;
  const statusTxt = document.getElementById("status-mode")!;
  statusTxt.textContent = editable ? "Editing mode" : "Previewing mode";
}

/**
 * Inserts a textbox.
 */
function editorAddTextbox() {
  // Insert onto current slide
  const contentContainer = document.getElementById("editor-content");

  if (!contentContainer) {
    console.error("No content container found");
    return;
  }

  const currSlide = contentContainer.querySelector("section.present");
  if (currSlide == null) return console.log("[editor] Current slide not found");

  const range = elGetDecksRange(currSlide);
  if (range == null) return;

  editorSendNotification({
    notifType: "NotifTextInserted",
    notifSrc: range,
    notifNewVal: "Insert text here",
  });
}

/**
 * Sends a Notification to the backend via the WebSocket connection
 */
function editorSendNotification(notification: DecksNotification | null): void {
  if (webSocket == null) {
    return console.error(
      "[ws] Tried to send notification, but connection is null."
    );
  }

  if (notification == null || notification == undefined) {
    return console.error(
      "[editor] Tried to send notification, but notification is: " +
        notification
    );
  }

  console.info("[editor] Sending notification:");
  console.info(notification);
  webSocket.send(JSON.stringify(notification));
}

//------------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------------

initDecks();
