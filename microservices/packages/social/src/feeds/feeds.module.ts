import { Module } from '@nestjs/common';
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

@Module({
  imports: [
    CqrsModule,
    MongooseModule.forFeature([{ name: 'Feed', schema: feedSchema }]),
    ClientsModule.register([
      {
        name: 'USER_SERVICE',
        transport: Transport.RMQ,
        options: {
          urls: [`amqp://localhost:5672`],
          queue: 'users_queue',
          queueOptions: {},
        },
      },
    ]),
  ],
  controllers: [FeedsController],
  providers: [
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
