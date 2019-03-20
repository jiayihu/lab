import HyperHTMLElement from 'hyperhtml-element';
import classnames from 'classnames';
import style from './Button.css';

export class Button extends HyperHTMLElement {
  kind: 'primary' | 'secondary' | 'link';

  static get observedAttributes(): Array<keyof Button> {
    return ['kind'];
  }

  constructor() {
    super();

    this.attachShadow({ mode: 'open' });
  }

  attributeChangedCallback() {
    this.render();
  }

  render() {
    const className = classnames('button', {
      [`button--${this.kind}`]: !!this.kind,
    });

    return this.html`
      <style>${style}</style>
      <button class=${className}></button>
    `;
  }
}

Button.define('mr-button');
