import { Feed } from '../../domain/feed.model';

export class AddFeedCommand {
  type = 'ADD_FEED_COMMAND';
  constructor(public payload: Feed) {}
}

export type FeedCommand = AddFeedCommand;
