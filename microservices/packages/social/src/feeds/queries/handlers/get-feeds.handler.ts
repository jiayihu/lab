import { QueryHandler, IQueryHandler } from '@nestjs/cqrs';
import { GetFeedsQuery } from '../impl/get-feeds.query';
import { InjectModel } from '@nestjs/mongoose';
import { Model } from 'mongoose';
import { FeedDoc } from 'src/feeds/interfaces/feeds.doc';

@QueryHandler(GetFeedsQuery)
export class GetFeedsHandler implements IQueryHandler<GetFeedsQuery> {
  constructor(@InjectModel('Feed') private feedModel: Model<FeedDoc>) {}

  execute(_: GetFeedsQuery) {
    return this.feedModel.find().then(feeds => feeds);
  }
}
