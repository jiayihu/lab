import { CommandHandler, ICommandHandler, EventPublisher } from '@nestjs/cqrs';
import { AddFeedCommand } from '../impl/add-feed.command';
import { FeedsRepository } from 'src/feeds/repository/feeds.repository';

@CommandHandler(AddFeedCommand)
export class AddFeedHandler implements ICommandHandler<AddFeedCommand> {
  constructor(private repository: FeedsRepository, private publisher: EventPublisher) {}

  execute(command: AddFeedCommand) {
    const { payload } = command;

    return this.repository
      .create(payload)
      .then(feed => this.publisher.mergeObjectContext(feed))
      .then(feed => {
        feed.create();
        feed.commit();

        return feed;
      });
  }
}
