import { CommandHandler, ICommandHandler, EventBus } from '@nestjs/cqrs';
import { InjectModel } from '@nestjs/mongoose';
import { Model } from 'mongoose';
import { FeedDoc } from 'src/feeds/interfaces/feeds.doc';
import { AddFeedCommand } from '../impl/add-feed.command';
import { FeedAddedEvent } from 'src/feeds/events/impl/feed-added.event';

@CommandHandler(AddFeedCommand)
export class AddFeedHandler implements ICommandHandler<AddFeedCommand> {
  constructor(
    @InjectModel('Feed') private feedsModel: Model<FeedDoc>,
    private eventBus: EventBus,
  ) {}

  execute(command: AddFeedCommand) {
    const { payload } = command;

    /**
     * @TODO populate userId and bookId
     */
    const feed = new this.feedsModel(payload);

    return feed.save().then(feed => {
      this.eventBus.publish(new FeedAddedEvent(feed));
      return feed;
    });
  }
}
