import { EventsHandler, IEventHandler } from '@nestjs/cqrs';
import { UserAddedEvent, UserValidatedEvent } from './users.events';

@EventsHandler(UserAddedEvent)
export class UserAddedHandler implements IEventHandler<UserAddedEvent> {
  constructor() {}

  handle(event: UserAddedEvent) {
    console.log('UserAddedEvent', event);
  }
}

@EventsHandler(UserValidatedEvent)
export class UserValidatedHandler implements IEventHandler<UserValidatedEvent> {
  constructor() {}

  handle(event: UserValidatedEvent) {
    console.log('UserValidatedEvent', event);
  }
}

export const EventHandlers = [UserAddedHandler, UserValidatedHandler];
