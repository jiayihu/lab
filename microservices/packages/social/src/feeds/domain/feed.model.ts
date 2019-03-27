import { AggregateRoot } from '@nestjs/cqrs';

export class Feed extends AggregateRoot {
  constructor(
    public userId: string,
    public date: string,
    public type: 'WantsToRead' | 'Reading',
    public bookId: string,
  ) {
    super();
  }
}
