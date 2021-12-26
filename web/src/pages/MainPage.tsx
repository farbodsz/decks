import React from "react";

import ContentPreview from "../components/ContentPreview";
import DecksAppBar from "../components/DecksAppBar";

interface MainPageProps {}
interface MainPageState {}

export default class MainPage extends React.Component<
  MainPageProps,
  MainPageState
> {
  private contentRef = React.createRef<HTMLDivElement>();

  constructor(props: MainPageProps) {
    super(props);
    this.saveContent = this.saveContent.bind(this);
  }

  saveContent(event: React.MouseEvent<HTMLElement>) {
    console.log(this.contentRef.current?.innerHTML);
  }

  render() {
    return (
      <>
        <DecksAppBar
          onSave={(event: React.MouseEvent<HTMLButtonElement>) => {
            console.log(this.contentRef.current?.innerHTML);
          }}
        />
        <ContentPreview contentRef={this.contentRef} />
      </>
    );
  }
}
