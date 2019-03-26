import { QueryHandler, IQueryHandler } from '@nestjs/cqrs';
import { GetFeedsQuery } from '../impl/get-feeds.query';
import { FeedsRepository } from 'src/feeds/repository/feeds.repository';

@QueryHandler(GetFeedsQuery)
export class GetFeedsHandler implements IQueryHandler<GetFeedsQuery> {
  constructor(private repository: FeedsRepository) {}

  execute(_: GetFeedsQuery) {
    return this.repository.find();
  }
}
