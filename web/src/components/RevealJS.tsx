import "reveal.js/dist/reveal.css";
import "reveal.js/dist/theme/white.css";

import React from "react";
import Reveal from "reveal.js";
import Markdown from "reveal.js/plugin/markdown/markdown.esm.js";

import styles from "./RevealJS.module.scss";

export interface RevealJSProps {
  children: React.ReactNode;
  innerContentRef: React.RefObject<HTMLDivElement>;
}

export interface RevealJSState {}

export default function RevealJS(props: RevealJSProps) {
  React.useEffect(() => {
    const deck = new Reveal({
      // Display presentation control arrows
      controls: true,

      // Don't show a presentation progress bar
      progress: false,

      // // Disable keyboard shortcuts for navigation
      // keyboard: false,

      // Disable the default reveal.js slide layout (scaling and centering)
      // so that you can use custom CSS layout
      disableLayout: true,

      // Flags if the presentation is running in an embedded mode,
      // i.e. contained within a limited portion of the screen
      embedded: true,

      // Don't show help when question mark pressed
      help: false,

      // Plugins
      plugins: [Markdown],
    });

    deck.initialize();
    console.log("Initialized Reveal.js");
  });

  return (
    <div
      className={`reveal ${styles.container}`}
      contentEditable
      suppressContentEditableWarning={true}
    >
      <div ref={props.innerContentRef} className="slides">
        {props.children}
      </div>
    </div>
  );
}
