import { Injectable, Inject } from '@nestjs/common';
import { EventBus } from '@nestjs/cqrs';
import { User, createUser } from './domain/user.model';
import { UsersRepository } from './repository/users.repository';
import { UserAddedEvent, UserValidatedEvent } from './events/users.events';
import { EventPublisher } from '@microreads/core/event-publisher';

@Injectable()
export class UsersService {
  constructor(
    private repository: UsersRepository,
    private eventBus: EventBus,
    @Inject('USERS_EVENTS') private usersEvents: EventPublisher,
  ) {}

  addUser(dto: User): Promise<User> {
    const user = createUser('', dto.name, dto.picture);
    return this.repository.create(user).then(x => {
      this.eventBus.publish(new UserAddedEvent(x));
      return x;
    });
  }

  getUsers(): Promise<User[]> {
    return this.repository.find();
  }

  validateUser(userId: string): Promise<boolean> {
    return this.repository
      .findOne(userId)
      .then(user => user !== null)
      .then(isValid => {
        const event = new UserValidatedEvent({ userId, isValid });
        this.eventBus.publish(event);
        this.usersEvents.publish(event);

        return isValid;
      });
  }
}
