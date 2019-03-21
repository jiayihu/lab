import style from './App.css';
import { MicroReadElement } from '../../lib/MicroReadsElement';
import { context } from '../../lib/context';

type State = {};

const { bind } = MicroReadElement;

export class App extends MicroReadElement<State> {
  get defaultState(): State {
    return {};
  }

  private routerOutletEl: HTMLElement = document.createElement('div');
  private router = context.get('router');
  private renderRoute = bind(this.routerOutletEl);

  constructor() {
    super();

    this.attachShadow({ mode: 'open' });
  }

  private resetScroll() {
    window.scrollTo(0, 0);
  }

  private handleHomeRoute() {
    this.renderRoute`<mr-homepage />`;
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
