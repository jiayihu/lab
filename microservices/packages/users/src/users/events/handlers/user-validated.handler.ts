import { EventsHandler, IEventHandler } from '@nestjs/cqrs';
import { UserValidatedEvent } from '../impl/users.event';

@EventsHandler(UserValidatedEvent)
export class UserValidatedHandler implements IEventHandler<UserValidatedEvent> {
  constructor() {}

  handle(event: UserValidatedEvent) {
    console.log('UserValidatedEvent', event);
  }
}
