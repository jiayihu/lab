import { Store, Unsubscribe, Action } from 'redux';
import { context } from './context';
import HyperHTMLElement from 'hyperhtml-element';

type Constructor<T> = new (...args: any[]) => T;

interface CustomElement {
  connectedCallback?(): void;
  disconnectedCallback?(): void;
  readonly isConnected: boolean;
}

/**
 * Allows to connect a Web Component to Redux
 * Implementation is taken from @link https://github.com/Polymer/pwa-helpers
 */
export const connect = <S>(store: Store<S>) => <T extends Constructor<CustomElement>>(
  baseElement: T,
) =>
  class extends baseElement {
    _storeUnsubscribe!: Unsubscribe;

    connectedCallback() {
      if (super.connectedCallback) {
        super.connectedCallback();
      }

      this._storeUnsubscribe = store.subscribe(() => this.stateChanged(store.getState()));
      this.stateChanged(store.getState());
    }

    disconnectedCallback() {
      this._storeUnsubscribe();

      if (super.disconnectedCallback) {
        super.disconnectedCallback();
      }
    }

    /**
     * The `stateChanged(state)` method will be called when the state is updated.
     */
    stateChanged(_state: S) {}

    dispatch(action: Action) {
      store.dispatch(action);
    }
  };

export const ConnectedHyperElement = connect(context.get('store'))(HyperHTMLElement);
