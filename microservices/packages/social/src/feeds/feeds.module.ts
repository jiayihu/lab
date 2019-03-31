import { Module, Provider } from '@nestjs/common';
import { MongooseModule } from '@nestjs/mongoose';
import { FeedsController } from './feeds.controller';
import { FeedsService } from './feeds.service';
import { feedSchema } from './schemas/feed.schema';
import { CommandHandlers } from './commands/feeds.command-handlers';
import { EventHandlers } from './events/feeds.event-handlers';
import { CqrsModule } from '@nestjs/cqrs';
import { QueryHandlers } from './queries/feeds.query-handlers';
import { FeedsRepository } from './repository/feeds.repository';
import { ClientsModule, Transport } from '@nestjs/microservices';
import { Sagas } from './sagas';
import { EventSubscriber } from '@microreads/core/event-subscriber';

const usersEventsProvider: Provider = {
  provide: 'USERS_EVENTS',
  useValue: new EventSubscriber({
    urls: [process.env.RABBITMQ || 'amqp://localhost:5672'],
    queue: 'users_commands',
    queueOptions: {},
  }),
};

@Module({
  imports: [
    CqrsModule,
    MongooseModule.forFeature([{ name: 'Feed', schema: feedSchema }]),
    ClientsModule.register([
      {
        name: 'USER_SERVICE',
        transport: Transport.RMQ,
        options: {
          urls: [process.env.RABBITMQ || 'amqp://localhost:5672'],
          queue: 'users_commands',
          queueOptions: {},
        },
      },
    ]),
  ],
  controllers: [FeedsController],
  providers: [
    usersEventsProvider,
    FeedsService,
    FeedsRepository,
    ...Sagas,
    ...CommandHandlers,
    ...EventHandlers,
    ...QueryHandlers,
  ],
  exports: [FeedsService],
})
export class FeedsModule {}
