import { TFeed } from 'src/feeds/interfaces/feeds.doc';

export class FeedAddedEvent {
  public type = 'ADDED_FEED_EVENT';

  constructor(public payload: TFeed) {}
}
