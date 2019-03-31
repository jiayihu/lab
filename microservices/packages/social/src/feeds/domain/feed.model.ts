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
  state: 'Pending' | 'Approved';
  userId: string;
  date: string;
  type: 'WantsToRead' | 'Reading';
  bookId: string;
};

export function createFeed(
  id: string,
  state: 'Pending' | 'Approved',
  userId: string,
  date: string,
  type: 'WantsToRead' | 'Reading',
  bookId: string,
): Feed {
  return {
    id,
    state,
    userId,
    date,
    type,
    bookId,
  };
}

export function approveFeed(feed: Feed): Feed {
  return {
    ...feed,
    state: 'Approved',
  };
}
