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
      .then(feeds => feeds.map(x => new Feed(x.userId, x.date, x.type, x.bookId)));
  }
  findOne() {}
  create(dto: Feed): Promise<Feed> {
    /**
     * @TODO populate userId and bookId
     */
    return this.feedODM.create(dto).then(x => new Feed(x.userId, x.date, x.type, x.bookId));
  }
  delete() {}
  update() {}
}
