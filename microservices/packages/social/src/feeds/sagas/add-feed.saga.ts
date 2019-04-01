import { Injectable, Inject } from '@nestjs/common';
import { Saga, IEvent, ICommand, ofType } from '@nestjs/cqrs';
import { Observable } from 'rxjs';
import { tap, switchMap, map, filter, take } from 'rxjs/operators/';
import { FeedAddedEvent } from '../events/feeds.events';
import { ClientProxy } from '@nestjs/microservices';
import { ApproveFeedCommand, RemoveFeedCommand } from '../commands/feeds.commands';

@Injectable()
export class AddFeedSaga {
  constructor(@Inject('USER_SERVICE') private userService: ClientProxy) {}

  @Saga()
  addedFeed = (events$: Observable<IEvent>): Observable<ICommand> => {
    return events$.pipe(
      ofType(FeedAddedEvent),
      switchMap(event => {
        const feed = event.payload;

        return this.userService
          .send('command', {
            type: 'VALIDATE_USER_COMMAND',
            payload: { userId: feed.userId },
          })
          .pipe(
            map((isValid: boolean) =>
              isValid
                ? new ApproveFeedCommand({ feedId: feed.id })
                : new RemoveFeedCommand({ feedId: feed.id }),
            ),
          );
      }),
    );
  };
}
