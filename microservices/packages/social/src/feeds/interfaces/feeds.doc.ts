import { Document } from 'mongoose';

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

export type TFeed = {
  user: TUser;
  date: string;
  likes: TLike[];
  type: 'WantsToRead';
  book: TBook;
};

export type FeedDoc = Document & TFeed;
