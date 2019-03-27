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
  userId: string;
  date: string;
  type: 'WantsToRead' | 'Reading';
  bookId: string;
};

export function createFeed(
  userId: string,
  date: string,
  type: 'WantsToRead' | 'Reading',
  bookId: string,
): Feed {
  return {
    userId,
    date,
    type,
    bookId,
  };
}
