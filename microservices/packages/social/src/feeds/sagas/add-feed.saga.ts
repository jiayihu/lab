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
        const userValidationEvent$ = events$.pipe(
          filter(
            (event: any) =>
              event.type === 'USER_VALIDATED_EVENT' && event.payload.userId === feed.userId,
          ),
          take(1),
        );

        return this.userService
          .emit('command', {
            type: 'VALIDATE_USER_COMMAND',
            payload: { userId: feed.userId },
          })
          .pipe(
            switchMap(() => userValidationEvent$),
            map(event =>
              event.payload.isValid
                ? new ApproveFeedCommand({ feedId: feed.id })
                : new RemoveFeedCommand({ feedId: feed.id }),
            ),
          );
      }),
    );
  };
}
