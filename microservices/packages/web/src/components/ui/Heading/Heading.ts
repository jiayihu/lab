import { MicroReadElement } from '../../../lib/MicroReadsElement';
import style from './Heading.css';

export class Heading extends MicroReadElement {
  level: number;

  static get observedAttributes(): Array<keyof Heading> {
    return ['level'];
  }

  constructor() {
    super();

    this.attachShadow({ mode: 'open' });
  }

  attributeChangedCallback() {
    this.render();
  }

  render() {
    const el = document.createElement(`h${this.level}`);
    el.innerHTML = `<slot></slot>`;

    return this.html`
      <style>${style}</style>
      ${el}
    `;
  }
}

Heading.define('mr-heading');
