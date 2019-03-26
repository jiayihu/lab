export type TUser = {
  id: string;
  name: string;
  picture: string;
};

export type TBook = {
  title: string;
  author: string;
  cover: string;
  summary: string;
};

export type TLike = {
  id: string;
  userId: string;
  userName: string;
};
