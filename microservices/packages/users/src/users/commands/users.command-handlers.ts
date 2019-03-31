import { CommandHandler, ICommandHandler } from '@nestjs/cqrs';
import { AddUserCommand, ValidateUserCommand } from './users.commands';
import { UsersService } from '../users.service';

@CommandHandler(AddUserCommand)
export class AddUserHandler implements ICommandHandler<AddUserCommand> {
  constructor(private usersService: UsersService) {}

  execute(command: AddUserCommand) {
    const { payload } = command;

    return this.usersService.addUser(payload);
  }
}

@CommandHandler(ValidateUserCommand)
export class ValidateUserHandler implements ICommandHandler<ValidateUserCommand> {
  constructor(private usersService: UsersService) {}

  execute(command: ValidateUserCommand) {
    const { payload } = command;

    return this.usersService.validateUser(payload.userId);
  }
}

export const CommandHandlers = [AddUserHandler, ValidateUserHandler];
