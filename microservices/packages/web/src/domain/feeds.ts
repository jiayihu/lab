import { TUser } from './users';
import { TBook } from './book';

export type TLike = {
  id: string;
  userId: string;
  userName: string;
};

export type TFeedWantsToRead = {
  user: TUser;
  date: string;
  likes: TLike[];
  type: 'WantsToRead';
  book: TBook;
};

export type TFeedRating = {
  user: TUser;
  date: string;
  likes: TLike[];
  type: 'Rating';
  book: TBook;
};

export type TFeed = TFeedWantsToRead | TFeedRating;
