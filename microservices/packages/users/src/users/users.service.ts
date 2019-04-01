import { Injectable, Inject } from '@nestjs/common';
import { EventBus } from '@nestjs/cqrs';
import { User, createUser } from './domain/user.model';
import { UsersRepository } from './repository/users.repository';
import { UserAddedEvent, UserValidatedEvent, UserEvent } from './events/users.events';
import { EventPublisher } from '@microreads/core/event-publisher';
import { EventSubscriber } from '@microreads/core/event-subscriber';

@Injectable()
export class UsersService {
  constructor(
    private repository: UsersRepository,
    private eventBus: EventBus,
    @Inject('USERS_EVENTS_PUB') private usersEventsPub: EventPublisher,
    @Inject('USERS_EVENTS_SUB') private usersEventsSub: EventSubscriber<UserEvent>,
  ) {
    this.usersEventsSub.subscribe(event => {
      switch (event.type) {
        case 'USER_ADDED_EVENT':
          return this.eventBus.publish(new UserAddedEvent(event.payload));
        case 'USER_VALIDATED_EVENT':
          return this.eventBus.publish(new UserValidatedEvent(event.payload));
        default:
          return this.eventBus.publish(event);
      }
    });
  }

  addUser(dto: User): Promise<User> {
    const user = createUser('', dto.name, dto.picture);
    return this.repository.create(user).then(x => {
      this.usersEventsPub.publish(new UserAddedEvent(x));
      return x;
    });
  }

  validateUser(userId: string): Promise<boolean> {
    return this.repository
      .findOne(userId)
      .then(user => user !== null)
      .then(isValid => {
        const event = new UserValidatedEvent({ userId, isValid });
        this.usersEventsPub.publish(event);

        return isValid;
      });
  }
}
