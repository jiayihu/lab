import { CommandHandler, ICommandHandler, EventPublisher } from '@nestjs/cqrs';
import { AddFeedCommand } from '../impl/add-feed.command';
import { FeedsRepository } from 'src/feeds/repository/feeds.repository';
import { Feed } from 'src/feeds/domain/feed.model';

@CommandHandler(AddFeedCommand)
export class AddFeedHandler implements ICommandHandler<AddFeedCommand> {
  constructor(private repository: FeedsRepository, private publisher: EventPublisher) {}

  execute(command: AddFeedCommand) {
    const { payload } = command;

    const feed = this.publisher.mergeObjectContext(
      new Feed(payload.userId, payload.date, payload.type, payload.bookId),
    );

    feed.create();
    feed.commit();

    return Promise.resolve(feed);
  }
}
