export type FeedState = {
  id: string;
  state: 'Pending' | 'Approved';
  userId: string;
  date: string;
  type: 'WantsToRead' | 'Reading';
  bookId: string;
};

export function createFeedState(
  id: string,
  state: 'Pending' | 'Approved',
  userId: string,
  date: string,
  type: 'WantsToRead' | 'Reading',
  bookId: string,
): FeedState {
  return {
    id,
    state,
    userId,
    date,
    type,
    bookId,
  };
}

export function approveFeed(feed: FeedState): FeedState {
  return {
    ...feed,
    state: 'Approved',
  };
}

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
  user: User;
  date: string;
  type: 'WantsToRead' | 'Reading';
  bookId: string;
};

export function createFeed(
  id: string,
  user: User,
  date: string,
  type: 'WantsToRead' | 'Reading',
  bookId: string,
): Feed {
  return {
    id,
    user,
    date,
    type,
    bookId,
  };
}
