import "reveal.js/dist/reveal.css";
import "reveal.js/dist/theme/white.css";

import React from "react";
import Reveal from "reveal.js";

export interface RevealJSProps {
  children: React.ReactNode;
}

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
      <div className="slides">{props.children}</div>
    </div>
  );
}
