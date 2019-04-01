import { Controller, Get, Post, Body } from '@nestjs/common';
import { FeedsService } from './feeds.service';
import { FeedState, Feed } from './domain/feed.model';
import { QueryBus } from '@nestjs/cqrs';
import { GetFeedsQuery } from './queries/feeds.queries';

@Controller('feeds')
export class FeedsController {
  constructor(private readonly feedsService: FeedsService, private readonly queryBus: QueryBus) {}

  @Post()
  addFeed(@Body() dto: FeedState): Promise<FeedState> {
    return this.feedsService.addFeed(dto);
  }

  @Get()
  getFeeds(): Promise<Feed[]> {
    return this.queryBus.execute(new GetFeedsQuery());
  }
}
