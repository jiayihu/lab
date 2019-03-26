import { Module } from '@nestjs/common';
import { MongooseModule } from '@nestjs/mongoose';
import { FeedsController } from './feeds.controller';
import { FeedsService } from './feeds.service';
import { feedSchema } from './schemas/feed.schema';
import { CommandHandlers } from './commands/handlers';
import { EventHandlers } from './events/handlers';
import { CqrsModule } from '@nestjs/cqrs';
import { QueryHandlers } from './queries/handlers';
import { FeedsRepository } from './repository/feeds.repository';

@Module({
  imports: [CqrsModule, MongooseModule.forFeature([{ name: 'Feed', schema: feedSchema }])],
  controllers: [FeedsController],
  providers: [
    FeedsService,
    FeedsRepository,
    ...CommandHandlers,
    ...EventHandlers,
    ...QueryHandlers,
  ],
  exports: [FeedsService],
})
export class FeedsModule {}
