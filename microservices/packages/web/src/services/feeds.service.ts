import { TFeed } from '../domain/feeds';
import request from './api';

export const getFeeds = (): Promise<TFeed[]> => {
  return request('social/feeds');
};
