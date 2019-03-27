import { EventsHandler, IEventHandler } from '@nestjs/cqrs';
import { FeedAddedEvent } from '../impl/feed-added.event';
import { FeedsRepository } from 'src/feeds/repository/feeds.repository';

@EventsHandler(FeedAddedEvent)
export class FeedAddedHandler implements IEventHandler<FeedAddedEvent> {
  constructor(private repository: FeedsRepository) {}

  handle(event: FeedAddedEvent) {
    /**
     * @TODO populate userId and bookId
     */
    this.repository.create(event.payload);
  }
}
