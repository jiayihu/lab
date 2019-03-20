import HyperHTMLElement from 'hyperhtml-element';
import { DateTime } from 'luxon';
import { TUser } from '../../../../domain/users';
import { TLike } from '../../../../domain/feeds';

export class Feed extends HyperHTMLElement {
  user: TUser;
  date: string;
  likes: TLike[];

  static get observedAttributes(): Array<keyof Feed> {
    return ['user', 'date', 'likes'];
  }

  constructor() {
    super();

    this.attachShadow({ mode: 'open' });
  }

  attributeChangedCallback() {
    this.render();
  }

  render() {
    const elapsedDate = DateTime.fromISO(this.date).toLocaleString(DateTime.DATETIME_SHORT);

    return this.html`
      <div class="feed">
        <div class="header">
          <span>
            <mr-routerlink>${this.user.name}</mr-routerlink>
            <slot name="action"></slot>
          </span>
          <span>${elapsedDate}</span>
        </div>
        <div class="content">
          <slot name="content"></slot>
          <p>
            <mr-button kind="link">Like</mr-button>
            <mr-button kind="link">Comment</mr-button>
          </p>
        </div>

      </div>
    `;
  }
}

Feed.define('mr-feed');
