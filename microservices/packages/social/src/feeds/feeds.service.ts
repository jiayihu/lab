import { Injectable } from '@nestjs/common';
import { EventBus } from '@nestjs/cqrs';
import { Feed, createFeed } from './domain/feed.model';
import { FeedsRepository } from './repository/feeds.repository';
import { FeedAddedEvent } from './events/impl/feeds.events';

@Injectable()
export class FeedsService {
  constructor(private repository: FeedsRepository, private eventBus: EventBus) {}

  addFeed(dto: Feed): Promise<Feed> {
    const feed = createFeed(dto.userId, dto.date, dto.type, dto.bookId);

    return this.repository.create(feed).then(x => {
      this.eventBus.publish(new FeedAddedEvent(feed));
      return x;
    });
  }

  getFeeds(): Promise<Feed[]> {
    return this.repository.find();
  }
}
