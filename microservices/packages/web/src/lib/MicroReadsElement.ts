import HyperHTMLElement from 'hyperhtml-element';

export class MicroReadElement<State = {}> extends HyperHTMLElement<State> {
  static define(name: string, options?: ElementDefinitionOptions) {
    HyperHTMLElement.define.call(this, name, options);

    const observedAttributes = this.observedAttributes || [];
    const proto = this.prototype;

    observedAttributes.forEach(name => {
      Object.defineProperty(proto, name, {
        configurable: true,
        get() {
          return this[`_${name}`] || super[name];
        },
        set(value) {
          if (typeof value === 'object' && value !== null) {
            this[`_${name}`] = value;
          } else {
            super[name] = value;
          }
        },
      });
    });
  }
}
