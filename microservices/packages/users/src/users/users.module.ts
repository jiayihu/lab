import { Module } from '@nestjs/common';
import { MongooseModule } from '@nestjs/mongoose';
import { UsersController } from './users.controller';
import { UsersService } from './users.service';
import { userSchema } from './schemas/user.schema';
import { CommandHandlers } from './commands/users.command-handlers';
import { EventHandlers } from './events/users.event-handlers';
import { CqrsModule } from '@nestjs/cqrs';
import { QueryHandlers } from './queries/users.query-handlers';
import { UsersRepository } from './repository/users.repository';

@Module({
  imports: [CqrsModule, MongooseModule.forFeature([{ name: 'User', schema: userSchema }])],
  controllers: [UsersController],
  providers: [
    UsersService,
    UsersRepository,
    ...CommandHandlers,
    ...EventHandlers,
    ...QueryHandlers,
  ],
  exports: [UsersService],
})
export class UsersModule {}
