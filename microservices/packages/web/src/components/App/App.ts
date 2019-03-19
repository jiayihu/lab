import HyperHTMLElement from 'hyperhtml-element';
import hyperApp from 'hyperhtml-app';

import style from './App.css';

type State = {};

const { bind } = HyperHTMLElement;

export class App extends HyperHTMLElement<State> {
  get defaultState(): State {
    return {};
  }

  private routerOutletEl: HTMLElement = document.createElement('div');
  private router = new hyperApp();
  private renderRoute = bind(this.routerOutletEl);

  constructor() {
    super();

    this.attachShadow({ mode: 'open' });
  }

  private resetScroll() {
    window.scrollTo(0, 0);
  }

  private handleHomeRoute() {
    this.renderRoute`<h1>Homepage</h1>`;
    this.resetScroll();
  }

  private configureRoutes() {
    this.router.get('/', this.handleHomeRoute);
  }

  private getCurrentRoute(): string {
    return window.location.pathname.replace('index.html', '');
  }

  connectedCallback() {
    this.configureRoutes();

    // Trigger route handler for the current path
    this.router.navigate(this.getCurrentRoute());
  }

  render() {
    return this.html`
      <style>${style}</style>

      <div>
        <nav>Nav</nav>

        <div class="container">
          ${this.routerOutletEl}
        </div>
      </div>
    `;
  }
}

App.define('mr-app');
