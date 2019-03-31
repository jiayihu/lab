import { Injectable } from '@nestjs/common';
import { EventBus } from '@nestjs/cqrs';
import { Feed, createFeed } from './domain/feed.model';
import { FeedsRepository } from './repository/feeds.repository';
import { FeedAddedEvent, FeedApprovedEvent, FeedRemovedEvent } from './events/feeds.events';

@Injectable()
export class FeedsService {
  constructor(private repository: FeedsRepository, private eventBus: EventBus) {}

  addFeed(dto: Feed): Promise<Feed> {
    const feed = createFeed('', dto.userId, dto.date, dto.type, dto.bookId);

    return this.repository.create(feed).then(x => {
      this.eventBus.publish(new FeedAddedEvent(x));
      return x;
    });
  }

  removeFeed(feedId: string): Promise<void> {
    return this.repository.delete(feedId).then(() => {
      this.eventBus.publish(new FeedRemovedEvent({ feedId }));
    });
  }

  getFeeds(): Promise<Feed[]> {
    return this.repository.find();
  }

  approveFeed(feedId: string): Promise<void> {
    this.eventBus.publish(new FeedApprovedEvent({ feedId }));

    return Promise.resolve();
  }
}
