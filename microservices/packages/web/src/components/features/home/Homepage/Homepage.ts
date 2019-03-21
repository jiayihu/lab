import style from './Homepage.css';
import { ConnectedHyperElement } from '../../../../lib/connect';
import { RootState } from '../../../../redux/reducers';
import { TFeed } from '../../../../domain/feeds';
import { getFeeds } from '../../../../services/feeds.service';
import { addFeeds } from '../../../../redux/actions/feeds.action';
import { wire } from 'hyperhtml';

export class Homepage extends ConnectedHyperElement {
  feeds: TFeed[] = [];

  static get observedAttributes(): Array<keyof Homepage> {
    return ['feeds'];
  }

  constructor() {
    super();

    this.attachShadow({ mode: 'open' });
  }

  connectedCallback() {
    super.connectedCallback();

    this.render();

    getFeeds().then(feeds => this.dispatch(addFeeds(feeds)));
  }

  stateChanged(state: RootState) {
    this.feeds = state.feeds;
    this.render();
  }

  attributeChangedCallback() {
    this.render();
  }

  render() {
    return this.html`
      <style>${style}</style>
      <div class="homepage">
        <h1>Hello</h1>
        <div class="feeds">
          ${this.feeds.map(feed => wire(feed)`<mr-feedwantstoread feed=${feed} />`)}
        </div>
      </div>
    `;
  }
}

Homepage.define('mr-homepage');
