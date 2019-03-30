import { EventsHandler, IEventHandler } from '@nestjs/cqrs';
import { UserAddedEvent } from '../impl/users.event';

@EventsHandler(UserAddedEvent)
export class UserAddedHandler implements IEventHandler<UserAddedEvent> {
  constructor() {}

  handle(event: UserAddedEvent) {
    console.log('UserAddedEvent', event);
  }
}
