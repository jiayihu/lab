import { CommandHandler, ICommandHandler } from '@nestjs/cqrs';
import { AddFeedCommand, RemoveFeedCommand, ApproveFeedCommand } from './feeds.commands';
import { FeedsService } from '../feeds.service';

@CommandHandler(AddFeedCommand)
export class AddFeedHandler implements ICommandHandler<AddFeedCommand> {
  constructor(private feedsService: FeedsService) {}

  execute(command: AddFeedCommand) {
    const { payload } = command;

    return this.feedsService.addFeed(payload);
  }
}

@CommandHandler(RemoveFeedCommand)
export class RemoveFeedHandler implements ICommandHandler<RemoveFeedCommand> {
  constructor(private feedsService: FeedsService) {}

  execute(command: RemoveFeedCommand) {
    const { payload } = command;

    return this.feedsService.removeFeed(payload.feedId);
  }
}

@CommandHandler(ApproveFeedCommand)
export class ApproveFeedHandler implements ICommandHandler<ApproveFeedCommand> {
  constructor(private feedsService: FeedsService) {}

  execute(command: ApproveFeedCommand) {
    const { payload } = command;

    return this.feedsService.approveFeed(payload.feedId);
  }
}

export const CommandHandlers = [AddFeedHandler, RemoveFeedHandler, ApproveFeedHandler];
