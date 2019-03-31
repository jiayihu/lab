import { Injectable } from '@nestjs/common';
import { EventBus } from '@nestjs/cqrs';
import { Feed, createFeed, approveFeed } from './domain/feed.model';
import { FeedsRepository } from './repository/feeds.repository';
import { FeedAddedEvent, FeedApprovedEvent, FeedRemovedEvent } from './events/feeds.events';

@Injectable()
export class FeedsService {
  constructor(private repository: FeedsRepository, private eventBus: EventBus) {}

  addFeed(dto: Feed): Promise<Feed> {
    const feed = createFeed('', 'Pending', dto.userId, dto.date, dto.type, dto.bookId);

    return this.repository.create(feed).then(x => {
      this.eventBus.publish(new FeedAddedEvent(x));
      return x;
    });
  }

  removeFeed(feedId: string): Promise<Feed> {
    return this.repository.delete(feedId).then(feed => {
      this.eventBus.publish(new FeedRemovedEvent({ feedId }));

      return feed;
    });
  }

  getFeeds(): Promise<Feed[]> {
    return this.repository.find();
  }

  approveFeed(feedId: string): Promise<Feed> {
    return this.repository
      .findOne(feedId)
      .then(feed => {
        const approvedFeed = approveFeed(feed);
        console.log(approvedFeed);

        return this.repository.update(approvedFeed);
      })
      .then(updatedFeed => {
        this.eventBus.publish(new FeedApprovedEvent({ feedId }));

        return updatedFeed;
      });
  }
}
