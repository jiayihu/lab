import { TFeed } from '../../domain/feeds';

export const addFeeds = (feeds: TFeed[]) => ({
  type: 'ADD_FEEDS' as 'ADD_FEEDS',
  payload: { feeds },
});

export type FeedsAction = ReturnType<typeof addFeeds>;
