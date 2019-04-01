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
  findOne(feedId: string): Promise<Feed | null> {
    return this.feedODM.findById(feedId).then(x => (x ? this.asFeed(x) : null));
  }
  create(feed: Feed): Promise<Feed> {
    return this.feedODM.create(feed).then(this.asFeed);
  }
  delete(feedId: string): Promise<Feed | null> {
    return this.feedODM.findByIdAndDelete(feedId).exec();
  }
  update(feed: Feed): Promise<Feed> {
    return this.feedODM.findByIdAndUpdate(feed.id, feed, { new: true }).then(this.asFeed);
  }

  private asFeed(doc: Feed & Document): Feed {
    return createFeed(
      doc._id.toString(),
      {
        id: doc.user.id.toString(),
        name: doc.user.name,
        picture: doc.user.picture,
      },
      doc.date,
      doc.type,
      doc.bookId.toString(),
    );
  }
}
