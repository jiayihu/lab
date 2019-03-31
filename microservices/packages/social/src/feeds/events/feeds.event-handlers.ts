import { EventsHandler, IEventHandler } from '@nestjs/cqrs';
import { FeedAddedEvent, FeedRemovedEvent, FeedApprovedEvent } from './feeds.events';

@EventsHandler(FeedAddedEvent)
export class FeedAddedHandler implements IEventHandler<FeedAddedEvent> {
  constructor() {}

  handle(event: FeedAddedEvent) {
    console.log('FeedAddedEvent', event);
  }
}

@EventsHandler(FeedRemovedEvent)
export class FeedRemovedHandler implements IEventHandler<FeedRemovedEvent> {
  constructor() {}

  handle(event: FeedRemovedEvent) {
    console.log('FeedRemovedEvent', event);
  }
}

@EventsHandler(FeedApprovedEvent)
export class FeedApprovedHandler implements IEventHandler<FeedApprovedEvent> {
  constructor() {}

  handle(event: FeedApprovedEvent) {
    console.log('FeedApprovedEvent', event);
  }
}

export const EventHandlers = [FeedAddedHandler, FeedRemovedHandler, FeedApprovedHandler];
