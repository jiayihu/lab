import { QueryHandler, IQueryHandler, IQuery } from '@nestjs/cqrs';
import { GetUsersQuery, GetUserQuery } from './users.queries';
import { UsersRepository } from '../repository/users.repository';
import { User } from '../domain/user.model';

@QueryHandler(GetUsersQuery)
export class GetUsersHandler implements IQueryHandler<GetUsersQuery> {
  constructor(private repository: UsersRepository) {}

  execute(_: GetUsersQuery): Promise<User[]> {
    return this.repository.find();
  }
}

@QueryHandler(GetUserQuery)
export class GetUserHandler implements IQueryHandler<GetUserQuery> {
  constructor(private repository: UsersRepository) {}

  execute(query: GetUserQuery): Promise<User> {
    return this.repository.findOne(query.payload.userId);
  }
}

export const QueryHandlers = [GetUsersHandler, GetUserHandler];
