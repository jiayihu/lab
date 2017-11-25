import React from 'react';
import { ReactTinyDOM } from './renderer/tiny-dom';

class HelloWorld extends React.Component {
  render() {
    return <h1>Hello world</h1>;
  }
}

ReactTinyDOM.render(<HelloWorld />, document.querySelector('.root'));
