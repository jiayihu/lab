import { Controller, Get, Post, Body } from '@nestjs/common';
import { UsersService } from './users.service';
import { User } from './domain/user.model';
import { EventPattern } from '@nestjs/microservices';
import { UserCommand, ValidateUserCommand, AddUserCommand } from './commands/impl/users.commands';
import { CommandBus } from '@nestjs/cqrs';

@Controller('users')
export class UsersController {
  constructor(private readonly usersService: UsersService, private commandBus: CommandBus) {}

  @Post()
  addUser(@Body() dto: User): Promise<User> {
    return this.usersService.addUser(dto);
  }

  @Get()
  getUsers(): Promise<User[]> {
    return this.usersService.getUsers();
  }

  @EventPattern('command')
  handleCommand(data: UserCommand): void {
    switch (data.type) {
      case 'ADD_USER_COMMAND':
        this.commandBus.execute(new AddUserCommand(data.payload));
        break;
      case 'VALIDATE_USER_COMMAND':
        this.commandBus.execute(new ValidateUserCommand(data.payload));
        break;
    }
  }
}
