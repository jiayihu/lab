import { createStore, compose, applyMiddleware } from 'redux';
import { rootReducer } from './reducers';

const composeEnhancers = (window as any)['__REDUX_DEVTOOLS_EXTENSION_COMPOSE__'] || compose;

export const store = createStore(rootReducer, composeEnhancers(applyMiddleware()));
