import { Injectable } from '@nestjs/common';
import { InjectModel } from '@nestjs/mongoose';
import { Model, Document } from 'mongoose';
import { FeedState, createFeedState } from '../domain/feed.model';

@Injectable()
export class FeedStateRepository {
  constructor(@InjectModel('FeedState') private feedODM: Model<FeedState & Document>) {}

  find(): Promise<FeedState[]> {
    return this.feedODM
      .find()
      .exec()
      .then(feeds => feeds.map(this.asFeedState));
  }
  findOne(feedId: string): Promise<FeedState | null> {
    return this.feedODM.findById(feedId).then(x => (x ? this.asFeedState(x) : null));
  }
  create(feed: FeedState): Promise<FeedState> {
    return this.feedODM.create(feed).then(this.asFeedState);
  }
  delete(feedId: string): Promise<FeedState | null> {
    return this.feedODM.findByIdAndDelete(feedId).exec();
  }
  update(feed: FeedState): Promise<FeedState> {
    return this.feedODM.findByIdAndUpdate(feed.id, feed, { new: true }).then(this.asFeedState);
  }

  private asFeedState(doc: FeedState & Document): FeedState {
    return createFeedState(
      doc._id.toString(),
      doc.state,
      doc.userId.toString(),
      doc.date,
      doc.type,
      doc.bookId.toString(),
    );
  }
}
