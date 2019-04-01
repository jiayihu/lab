export class GetUsersQuery {
  type = 'GET_USERS_QUERY' as const;
}

export class GetUserQuery {
  type = 'GET_USER_QUERY' as const;
  constructor(public payload: { userId: string }) {}
}

export type UserQuery = GetUsersQuery | GetUserQuery;
