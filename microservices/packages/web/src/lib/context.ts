import { Container } from './Container';
import hyperApp, { HyperHTMLApplication } from 'hyperhtml-app';
import { Store } from 'redux';
import { store } from '../redux/store';

type Context = {
  router: HyperHTMLApplication;
  store: Store;
};

const context = new Container<Context>();

context.bind('router', new hyperApp());
context.bind('store', store);

export { context };
