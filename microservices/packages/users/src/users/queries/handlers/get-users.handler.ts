import { QueryHandler, IQueryHandler } from '@nestjs/cqrs';
import { GetUsersQuery } from '../impl/users.queries';
import { UsersService } from '../../users.service';

@QueryHandler(GetUsersQuery)
export class GetUsersHandler implements IQueryHandler<GetUsersQuery> {
  constructor(private usersService: UsersService) {}

  execute(_: GetUsersQuery) {
    return this.usersService.getUsers();
  }
}
