export type User = {
  id: string;
  name: string;
  picture: string;
};

export function createUser(id: string, name: string, picture: string): User {
  return {
    id,
    name,
    picture,
  };
}
