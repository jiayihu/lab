import { Injectable, Inject } from '@nestjs/common';
import { Saga, IEvent, ICommand, ofType } from '@nestjs/cqrs';
import { Observable } from 'rxjs';
import { delay, tap, map, switchMap } from 'rxjs/operators/';
import { FeedAddedEvent } from '../events/impl/feeds.events';
import { ClientProxy } from '@nestjs/microservices';

@Injectable()
export class AddFeedSaga {
  constructor(@Inject('USER_SERVICE') private userService: ClientProxy) {}

  @Saga()
  addedFeed = (events$: Observable<IEvent>): Observable<ICommand> => {
    return events$.pipe(
      ofType(FeedAddedEvent),
      switchMap(event =>
        this.userService.emit('command', {
          type: 'VALIDATE_USER_COMMAND',
          payload: { userId: event.payload.userId },
        }),
      ),
    );
  };
}
