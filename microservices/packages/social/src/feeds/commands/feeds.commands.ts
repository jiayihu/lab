import { FeedState } from '../domain/feed.model';

export class AddFeedCommand {
  type = 'ADD_FEED_COMMAND';
  constructor(public payload: FeedState) {}
}

export class RemoveFeedCommand {
  type = 'REMOVE_FEED_COMMAND';
  constructor(public payload: { feedId: string }) {}
}

export class ApproveFeedCommand {
  type = 'APPROVE_FEED_COMMAND';
  constructor(public payload: { feedId: string }) {}
}

export type FeedCommand = AddFeedCommand | RemoveFeedCommand | ApproveFeedCommand;
