import { TFeed } from '../../domain/feeds';
import { Action } from '../actions';

export type FeedsState = TFeed[];

export const feedsReducer = (state: FeedsState = [], action: Action): FeedsState => {
  switch (action.type) {
    case 'ADD_FEEDS':
      return [...state, ...action.payload.feeds];
    default:
      return state;
  }
};
