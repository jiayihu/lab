import { Injectable } from '@nestjs/common';
import { InjectModel } from '@nestjs/mongoose';
import { Model, Document } from 'mongoose';
import { Feed, createFeed } from '../domain/feed.model';

@Injectable()
export class FeedsRepository {
  constructor(@InjectModel('Feed') private feedODM: Model<Feed & Document>) {}

  find(): Promise<Feed[]> {
    return this.feedODM
      .find()
      .exec()
      .then(feeds => feeds.map(this.asFeed));
  }
  findOne() {}
  create(feed: Feed): Promise<Feed> {
    return this.feedODM.create(feed).then(this.asFeed);
  }
  delete(feedId: string): Promise<Feed | null> {
    return this.feedODM.findByIdAndDelete(feedId).exec();
  }
  update() {}

  private asFeed(doc: Feed & Document): Feed {
    return createFeed(doc._id, doc.userId, doc.date, doc.type, doc.bookId);
  }
}
