import { Inject } from '@nestjs/common';
import { EventsHandler, IEventHandler } from '@nestjs/cqrs';
import { FeedAddedEvent, FeedRemovedEvent, FeedApprovedEvent } from './feeds.events';
import { ClientProxy } from '@nestjs/microservices';
import { map, switchMap, tap } from 'rxjs/operators';
import { createFeed } from '../domain/feed.model';
import { from } from 'rxjs';
import { FeedsRepository } from '../repository/feeds.repository';

@EventsHandler(FeedAddedEvent)
export class FeedAddedHandler implements IEventHandler<FeedAddedEvent> {
  constructor(
    @Inject('USER_SERVICE') private userService: ClientProxy,
    private repository: FeedsRepository,
  ) {}

  handle(event: FeedAddedEvent) {
    const feedState = event.payload;

    return this.userService
      .send('query', {
        type: 'GET_USER_QUERY',
        payload: { userId: feedState.userId },
      })
      .pipe(
        map(user =>
          createFeed(feedState.id, user, feedState.date, feedState.type, feedState.bookId),
        ),
        switchMap(feed => from(this.repository.create(feed))),
      )
      .subscribe(() => {});
  }
}

@EventsHandler(FeedRemovedEvent)
export class FeedRemovedHandler implements IEventHandler<FeedRemovedEvent> {
  constructor() {}

  handle(event: FeedRemovedEvent) {
    console.log('FeedRemovedEvent', event);
  }
}

@EventsHandler(FeedApprovedEvent)
export class FeedApprovedHandler implements IEventHandler<FeedApprovedEvent> {
  constructor() {}

  handle(event: FeedApprovedEvent) {
    console.log('FeedApprovedEvent', event);
  }
}

export const EventHandlers = [FeedAddedHandler, FeedRemovedHandler, FeedApprovedHandler];
