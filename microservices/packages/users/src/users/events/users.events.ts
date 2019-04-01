import { User } from '../domain/user.model';

export class UserAddedEvent {
  type = 'USER_ADDED_EVENT' as const;

  constructor(public payload: User) {}
}

export class UserValidatedEvent {
  type = 'USER_VALIDATED_EVENT' as const;

  constructor(public payload: { userId: string; isValid: boolean }) {}
}

export type UserEvent = UserAddedEvent | UserValidatedEvent;
