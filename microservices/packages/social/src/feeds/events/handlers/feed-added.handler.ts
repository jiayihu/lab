import { EventsHandler, IEventHandler } from '@nestjs/cqrs';
import { FeedAddedEvent } from '../impl/feeds.events';

@EventsHandler(FeedAddedEvent)
export class FeedAddedHandler implements IEventHandler<FeedAddedEvent> {
  constructor() {}

  handle(event: FeedAddedEvent) {
    console.log('FeedAddedEvent', event);
  }
}
