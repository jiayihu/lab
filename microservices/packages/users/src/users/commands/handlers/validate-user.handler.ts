import { CommandHandler, ICommandHandler } from '@nestjs/cqrs';
import { ValidateUserCommand } from '../impl/users.commands';
import { UsersService } from '../../users.service';

@CommandHandler(ValidateUserCommand)
export class ValidateUserHandler implements ICommandHandler<ValidateUserCommand> {
  constructor(private usersService: UsersService) {}

  execute(command: ValidateUserCommand) {
    const { payload } = command;

    return this.usersService.validateUser(payload.userId);
  }
}
