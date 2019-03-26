import { Feed } from 'src/feeds/domain/feed.model';

export class FeedAddedEvent {
  public type = 'ADDED_FEED_EVENT';

  constructor(public payload: Feed) {}
}
