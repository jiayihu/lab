import { MicroReadElement } from '../../../../lib/MicroReadsElement';
import { context } from '../../../../lib/context';

export class RouterLink extends MicroReadElement {
  route: string;
  router = context.get('router');

  static get observedAttributes(): Array<keyof RouterLink> {
    return ['route'];
  }

  constructor() {
    super();

    this.attachShadow({ mode: 'open' });
  }

  handleClick(event: MouseEvent) {
    event.preventDefault();
    this.router.navigate(this.route);
  }

  render() {
    return this.html`
      <a href=${this.route} onclick=${this.handleClick}><slot></slot></a>
    `;
  }
}

RouterLink.define('mr-routerlink');
