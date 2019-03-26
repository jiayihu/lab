import { Feed } from 'src/feeds/domain/feed.model';

export class AddFeedCommand {
  public type = 'ADD_FEED_COMMAND';
  constructor(public payload: Feed) {}
}

export type FeedCommand = AddFeedCommand;
