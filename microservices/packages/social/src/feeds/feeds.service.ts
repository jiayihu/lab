import { Injectable } from '@nestjs/common';
import { CommandBus, QueryBus } from '@nestjs/cqrs';
import { FeedDoc } from './interfaces/feeds.doc';
import { AddFeedCommand, AddFeedDTO } from './commands/impl/add-feed.command';
import { GetFeedsQuery } from './queries/impl/get-feeds.query';

@Injectable()
export class FeedsService {
  constructor(private commandBus: CommandBus, private queryBus: QueryBus) {}

  addFeed(dto: AddFeedDTO): Promise<void> {
    return this.commandBus.execute(new AddFeedCommand(dto));
  }

  getFeeds(): Promise<FeedDoc[]> {
    return this.queryBus.execute(new GetFeedsQuery());
  }
}
