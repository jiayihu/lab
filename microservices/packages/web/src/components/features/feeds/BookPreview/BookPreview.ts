import style from './BookPreview.css';
import { MicroReadElement } from '../../../../lib/MicroReadsElement';
import { TBook } from '../../../../domain/book';

export class BookPreview extends MicroReadElement {
  book: TBook;

  static get observedAttributes(): Array<keyof BookPreview> {
    return ['book'];
  }

  constructor() {
    super();

    this.attachShadow({ mode: 'open' });
  }

  attributeChangedCallback() {
    this.render();
  }

  render() {
    return this.html`
      <style>${style}</style>
      <div class="book-preview">
        <img src=${this.book.cover} alt=${this.book.title} />
        <div>
          <p>${this.book.title}</p>
          <p>by ${this.book.author}</p>
          <p>${this.book.summary}</p>
        </div>
      </div>
    `;
  }
}

BookPreview.define('mr-bookpreview');
