import { Feed } from '../domain/feed.model';

export class FeedAddedEvent {
  type = 'FEED_ADDED_EVENT';

  constructor(public payload: Feed) {}
}

export class FeedRemovedEvent {
  type = 'FEED_REMOVED_EVENT';

  constructor(public payload: { feedId: string }) {}
}

export class FeedApprovedEvent {
  type = 'FEED_APPROVED_EVENT';

  constructor(public payload: { feedId: string }) {}
}
