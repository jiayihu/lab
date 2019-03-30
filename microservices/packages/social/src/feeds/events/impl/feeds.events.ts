import { Feed } from '../../domain/feed.model';
import hyperid from 'hyperid';

const generateId = hyperid();

export class FeedAddedEvent {
  type = 'FEED_ADDED_EVENT';
  id = generateId();

  constructor(public payload: Feed) {}
}
