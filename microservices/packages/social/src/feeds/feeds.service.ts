import { Injectable, Inject } from '@nestjs/common';
import { EventBus, IEvent } from '@nestjs/cqrs';
import { FeedState, createFeedState, approveFeed } from './domain/feed.model';
import { FeedStateRepository } from './repository/feed-state.repository';
import { FeedAddedEvent, FeedApprovedEvent, FeedRemovedEvent } from './events/feeds.events';
import { EventSubscriber } from '@microreads/core/event-subscriber';

@Injectable()
export class FeedsService {
  constructor(
    private repository: FeedStateRepository,
    private eventBus: EventBus,
    @Inject('USERS_EVENTS_SUB') private usersEventsSub: EventSubscriber<IEvent>,
  ) {
    this.usersEventsSub.subscribe(event => this.eventBus.publish(event));
  }

  addFeed(dto: FeedState): Promise<FeedState> {
    const feed = createFeedState('', 'Pending', dto.userId, dto.date, dto.type, dto.bookId);

    return this.repository.create(feed).then(x => {
      this.eventBus.publish(new FeedAddedEvent(x));
      return x;
    });
  }

  removeFeed(feedId: string): Promise<FeedState> {
    return this.repository.delete(feedId).then(feed => {
      this.eventBus.publish(new FeedRemovedEvent({ feedId }));

      return feed;
    });
  }

  approveFeed(feedId: string): Promise<FeedState> {
    return this.repository
      .findOne(feedId)
      .then(feed => {
        const approvedFeed = approveFeed(feed);

        return this.repository.update(approvedFeed);
      })
      .then(updatedFeed => {
        this.eventBus.publish(new FeedApprovedEvent({ feedId }));

        return updatedFeed;
      });
  }
}
