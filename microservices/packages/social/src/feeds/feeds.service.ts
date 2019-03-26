import { Injectable } from '@nestjs/common';
import { CommandBus, QueryBus } from '@nestjs/cqrs';
import { AddFeedCommand } from './commands/impl/add-feed.command';
import { GetFeedsQuery } from './queries/impl/get-feeds.query';
import { Feed } from './domain/feed.model';

@Injectable()
export class FeedsService {
  constructor(private commandBus: CommandBus, private queryBus: QueryBus) {}

  addFeed(dto: Feed): Promise<void> {
    return this.commandBus.execute(new AddFeedCommand(dto));
  }

  getFeeds(): Promise<Feed[]> {
    return this.queryBus.execute(new GetFeedsQuery());
  }
}
