import { Button } from './Button';

describe('<mr-button>', () => {
  it('renders', () => {
    const element = new Button();
    element.render();
    const button = element.shadowRoot.querySelector('button');

    expect(button).not.toBe(null);
  });

  it('renders correct className', () => {
    const element = new Button();
    element.setAttribute('kind', 'primary');
    element.render();
    const button = element.shadowRoot.querySelector('button');

    expect(button.classList.contains('button--primary')).toBe(true);
  });
});
