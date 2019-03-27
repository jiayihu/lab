import { CommandHandler, ICommandHandler, EventBus } from '@nestjs/cqrs';
import { AddFeedCommand } from '../impl/add-feed.command';
import { FeedsRepository } from 'src/feeds/repository/feeds.repository';
import { Feed } from 'src/feeds/domain/feed.model';
import { FeedAddedEvent } from 'src/feeds/events/impl/feed-added.event';

@CommandHandler(AddFeedCommand)
export class AddFeedHandler implements ICommandHandler<AddFeedCommand> {
  constructor(private repository: FeedsRepository, private eventBus: EventBus) {}

  execute(command: AddFeedCommand) {
    const { payload } = command;

    const feed = new Feed(payload.userId, payload.date, payload.type, payload.bookId);

    this.eventBus.publish(new FeedAddedEvent(feed));

    return Promise.resolve(feed);
  }
}
