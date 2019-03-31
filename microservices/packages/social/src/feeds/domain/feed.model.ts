export type User = {
  id: string;
  name: string;
  picture: string;
};

export type Book = {
  title: string;
  author: string;
  cover: string;
  summary: string;
};

export type Like = {
  id: string;
  userId: string;
  userName: string;
};

export type Feed = {
  id: string;
  userId: string;
  date: string;
  type: 'WantsToRead' | 'Reading';
  bookId: string;
};

export function createFeed(
  id: string,
  userId: string,
  date: string,
  type: 'WantsToRead' | 'Reading',
  bookId: string,
): Feed {
  return {
    id,
    userId,
    date,
    type,
    bookId,
  };
}
