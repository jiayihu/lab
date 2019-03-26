import { Controller, Get, Post, Body } from '@nestjs/common';
import { FeedsService } from './feeds.service';
import { FeedDoc } from './interfaces/feeds.doc';
import { AddFeedDTO } from './commands/impl/add-feed.command';

@Controller('feeds')
export class FeedsController {
  constructor(private readonly feedsService: FeedsService) {}

  @Post()
  addFeed(@Body() dto: AddFeedDTO): Promise<void> {
    return this.feedsService.addFeed(dto);
  }

  @Get()
  getFeeds(): Promise<FeedDoc[]> {
    return this.feedsService.getFeeds();
  }
}
