import { CommandHandler, ICommandHandler } from '@nestjs/cqrs';
import { AddFeedCommand } from '../impl/feeds.commands';
import { FeedsService } from '../../feeds.service';

@CommandHandler(AddFeedCommand)
export class AddFeedHandler implements ICommandHandler<AddFeedCommand> {
  constructor(private feedsService: FeedsService) {}

  execute(command: AddFeedCommand) {
    const { payload } = command;

    return this.feedsService.addFeed(payload);
  }
}
