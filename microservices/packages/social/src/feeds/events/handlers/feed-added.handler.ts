import { EventsHandler, IEventHandler } from '@nestjs/cqrs';
import { FeedAddedEvent } from '../impl/feed-added.event';

@EventsHandler(FeedAddedEvent)
export class FeedAddedHandler implements IEventHandler<FeedAddedEvent> {
  handle(_: FeedAddedEvent) {
    console.log('FeedAddedEvent...');
  }
}
