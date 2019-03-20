import HyperHTMLElement from 'hyperhtml-element';
import { TFeedWantsToRead } from '../../../../domain/feeds';

export class FeedWantsToRead extends HyperHTMLElement {
  feed: TFeedWantsToRead;

  static get observedAttributes(): Array<keyof FeedWantsToRead> {
    return ['feed'];
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
      <mr-feed user=${this.feed.user} date=${this.feed.date} likes=${this.feed.likes}>
        <span slot="action">wants to read</span>
        <mr-bookpreview book=${this.feed.book} slot="content" />
      </mr-feed>
    `;
  }
}

FeedWantsToRead.define('mr-feedwantstoread');
