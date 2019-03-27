import { Injectable } from '@nestjs/common';
import { InjectModel } from '@nestjs/mongoose';
import { Model, Document } from 'mongoose';
import { Feed } from '../domain/feed.model';

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
  delete() {}
  update() {}

  private asFeed(doc: Feed & Document): Feed {
    return new Feed(doc.userId, doc.date, doc.type, doc.bookId);
  }
}
