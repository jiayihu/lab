import { Controller, Get, Post, Body } from '@nestjs/common';
import { FeedsService } from './feeds.service';
import { Feed } from './domain/feed.model';

@Controller('feeds')
export class FeedsController {
  constructor(private readonly feedsService: FeedsService) {}

  @Post()
  addFeed(@Body() dto: Feed): Promise<Feed> {
    return this.feedsService.addFeed(dto);
  }

  @Get()
  getFeeds(): Promise<Feed[]> {
    return this.feedsService.getFeeds();
  }
}
