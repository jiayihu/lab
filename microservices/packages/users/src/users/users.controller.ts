import { Controller, Get, Post, Body, Param } from '@nestjs/common';
import { UsersService } from './users.service';
import { User } from './domain/user.model';
import { MessagePattern } from '@nestjs/microservices';
import { UserCommand, ValidateUserCommand, AddUserCommand } from './commands/users.commands';
import { CommandBus, QueryBus } from '@nestjs/cqrs';
import { GetUsersQuery, UserQuery, GetUserQuery } from './queries/users.queries';

@Controller('users')
export class UsersController {
  constructor(
    private readonly usersService: UsersService,
    private commandBus: CommandBus,
    private queryBus: QueryBus,
  ) {}

  @Post()
  addUser(@Body() dto: User): Promise<User> {
    return this.usersService.addUser(dto);
  }

  @Get()
  getUsers(): Promise<User[]> {
    return this.queryBus.execute(new GetUsersQuery());
  }

  @Get(':userId')
  getUser(@Param('userId') userId: string): Promise<User> {
    return this.queryBus.execute(new GetUserQuery({ userId }));
  }

  @MessagePattern('command')
  handleCommand(data: UserCommand): Promise<any> {
    switch (data.type) {
      case 'ADD_USER_COMMAND':
        return this.commandBus.execute(new AddUserCommand(data.payload));
      case 'VALIDATE_USER_COMMAND':
        return this.commandBus.execute(new ValidateUserCommand(data.payload));
    }
  }

  @MessagePattern('query')
  handleQuery(data: UserQuery): Promise<any> {
    switch (data.type) {
      case 'GET_USERS_QUERY':
        return this.queryBus.execute(new GetUsersQuery());
      case 'GET_USER_QUERY':
        return this.queryBus.execute(new GetUserQuery(data.payload));
    }
  }
}
