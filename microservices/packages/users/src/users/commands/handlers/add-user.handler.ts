import { CommandHandler, ICommandHandler } from '@nestjs/cqrs';
import { AddUserCommand } from '../impl/users.commands';
import { UsersService } from '../../users.service';

@CommandHandler(AddUserCommand)
export class AddUserHandler implements ICommandHandler<AddUserCommand> {
  constructor(private usersService: UsersService) {}

  execute(command: AddUserCommand) {
    const { payload } = command;

    return this.usersService.addUser(payload);
  }
}
