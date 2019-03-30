import { User } from '../../domain/user.model';

export class UserAddedEvent {
  type = 'USER_ADDED_EVENT';

  constructor(public payload: User) {}
}

export class UserValidatedEvent {
  type = 'USER_VALIDATED_EVENT';

  constructor(public payload: { userId: string; isValid: boolean }) {}
}
