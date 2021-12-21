import "reveal.js/dist/reveal.css";
import "reveal.js/dist/theme/white.css";

import React from "react";
import Reveal from "reveal.js";

export interface RevealJSProps {
  children: React.ReactNode;
  innerContentRef: React.RefObject<HTMLDivElement>;
}

export interface RevealJSState {}

export default function RevealJS(props: RevealJSProps) {
  React.useEffect(() => {
    const deck = new Reveal();
    deck.initialize();
    console.log("Initialized Reveal.js");
  });

  return (
    <div
      className="reveal"
      style={{
        height: "800px",
        width: "800px",
        margin: "auto",
      }}
      contentEditable
      suppressContentEditableWarning={true}
    >
      <div ref={props.innerContentRef} className="slides">
        {props.children}
      </div>
    </div>
  );
}
