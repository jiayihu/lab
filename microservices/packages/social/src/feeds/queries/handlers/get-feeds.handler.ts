import { QueryHandler, IQueryHandler } from '@nestjs/cqrs';
import { GetFeedsQuery } from '../impl/feeds.queries';
import { FeedsService } from '../../feeds.service';

@QueryHandler(GetFeedsQuery)
export class GetFeedsHandler implements IQueryHandler<GetFeedsQuery> {
  constructor(private feedsService: FeedsService) {}

  execute(_: GetFeedsQuery) {
    return this.feedsService.getFeeds();
  }
}
