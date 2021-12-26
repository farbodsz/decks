import React from "react";
import ReactHtmlParser from "react-html-parser";

import styles from "./ContentPreview.module.scss";
import RevealJS from "./RevealJS";

const BACKEND_URL = "http://localhost:8081/decks";

interface ContentPreviewProps {
  contentRef: React.RefObject<HTMLDivElement>;
}

interface ContentPreviewState {
  contentHtml: string;
}

export default class ContentPreview extends React.Component<
  ContentPreviewProps,
  ContentPreviewState
> {
  constructor(props: ContentPreviewProps) {
    super(props);
    this.state = {
      contentHtml: "Loading...",
    };
  }

  componentDidMount() {
    fetch(BACKEND_URL)
      .then((response) => response.json())
      .then((data) => {
        this.setState({ contentHtml: data ? data : "null" });
      })
      .catch((error) => console.error("Error " + error.message));
  }

  render() {
    return (
      <div className={styles.container}>
        <RevealJS innerContentRef={this.props.contentRef}>
          {ReactHtmlParser(this.state.contentHtml)}
        </RevealJS>
      </div>
    );
  }
}
