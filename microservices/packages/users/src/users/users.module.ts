import { Module, Provider } from '@nestjs/common';
import { MongooseModule } from '@nestjs/mongoose';
import { UsersController } from './users.controller';
import { UsersService } from './users.service';
import { userSchema } from './schemas/user.schema';
import { CommandHandlers } from './commands/users.command-handlers';
import { EventHandlers } from './events/users.event-handlers';
import { CqrsModule } from '@nestjs/cqrs';
import { QueryHandlers } from './queries/users.query-handlers';
import { UsersRepository } from './repository/users.repository';
import { EventSubscriber } from '@microreads/core/event-subscriber';
import { EventPublisher } from '@microreads/core/event-publisher';

const usersEventsSub: Provider = {
  provide: 'USERS_EVENTS_SUB',
  useValue: new EventSubscriber({
    urls: [process.env.RABBITMQ || 'amqp://localhost:5672'],
    queue: 'users_events',
    queueOptions: {},
  }),
};

const usersEventsPub: Provider = {
  provide: 'USERS_EVENTS_PUB',
  useValue: new EventPublisher({
    urls: [process.env.RABBITMQ || 'amqp://localhost:5672'],
    queue: 'users_events',
    queueOptions: {},
  }),
};

@Module({
  imports: [CqrsModule, MongooseModule.forFeature([{ name: 'User', schema: userSchema }])],
  controllers: [UsersController],
  providers: [
    usersEventsSub,
    usersEventsPub,
    UsersService,
    UsersRepository,
    ...CommandHandlers,
    ...EventHandlers,
    ...QueryHandlers,
  ],
  exports: [UsersService],
})
export class UsersModule {}
