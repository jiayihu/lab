import { QueryHandler, IQueryHandler } from '@nestjs/cqrs';
import { GetFeedsQuery } from './feeds.queries';
import { FeedsRepository } from '../repository/feeds.repository';
import { Feed } from '../domain/feed.model';

@QueryHandler(GetFeedsQuery)
export class GetFeedsHandler implements IQueryHandler<GetFeedsQuery> {
  constructor(private repository: FeedsRepository) {}

  execute(_: GetFeedsQuery): Promise<Feed[]> {
    return this.repository.find();
  }
}

export const QueryHandlers = [GetFeedsHandler];
