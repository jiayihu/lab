import { AggregateRoot } from '@nestjs/cqrs';
import { FeedAddedEvent } from '../events/impl/feed-added.event';

export class Feed extends AggregateRoot {
  constructor(
    public userId: string,
    public date: string,
    public type: 'WantsToRead' | 'Reading',
    public bookId: string,
  ) {
    super();
  }

  create() {
    this.apply(new FeedAddedEvent(this));
  }
}
