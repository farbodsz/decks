import { Button } from "@mui/material";
import React from "react";
import ReactHtmlParser from "react-html-parser";

const BACKEND_URL = "http://localhost:8081/decks";

interface ContentPreviewProps {}

interface ContentPreviewState {
  contentHtml: string;
}

export default class ContentPreview extends React.Component<
  ContentPreviewProps,
  ContentPreviewState
> {
  private contentDivRef = React.createRef<HTMLDivElement>();

  constructor(props: ContentPreviewProps) {
    super(props);
    this.state = {
      contentHtml: "Loading...",
    };

    this.saveContent = this.saveContent.bind(this);
  }

  componentDidMount() {
    fetch(BACKEND_URL)
      .then((response) => response.json())
      .then((data) => {
        this.setState({ contentHtml: data ? data : "null" });
      })
      .catch((error) => console.error("Error " + error.message));
  }

  saveContent(event: React.MouseEvent<HTMLElement>) {
    console.log(this.contentDivRef.current?.innerHTML);
  }

  render() {
    // TODO: doesn't load the styles and scripts from the HTML
    // TODO: "Warning: A component is `contentEditable` and contains `children`
    // managed by React. It is now your responsibility to guarantee that none of
    // those nodes are unexpectedly modified or duplicated."
    return (
      <div>
        <Button onClick={this.saveContent}>Save</Button>
        <div ref={this.contentDivRef} contentEditable>
          {ReactHtmlParser(this.state.contentHtml)}
        </div>
      </div>
    );
  }
}
