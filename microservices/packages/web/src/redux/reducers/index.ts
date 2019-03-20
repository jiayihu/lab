import { FeedsState, feedsReducer } from './feeds.reducer';
import { combineReducers } from 'redux';

export type RootState = {
  feeds: FeedsState;
};

export const rootReducer = combineReducers<RootState>({
  feeds: feedsReducer,
});
