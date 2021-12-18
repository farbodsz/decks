import React from "react";

const BACKEND_URL = "http://localhost:8081/decks";

export function ContentPreview() {
  // Store the current presentation preview
  const [html, setHtml] = React.useState<string | null>(null);

  fetch(BACKEND_URL)
    .then((response) => response.json())
    .then((data) => {
      console.log(data);
      setHtml(data);
    })
    .catch((error) => console.error("Error " + error.message));

  // TODO: doesn't load the styles and scripts from the HTML
  return <div dangerouslySetInnerHTML={{ __html: html ? html : "" }} />;
}
