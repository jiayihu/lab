import { Request, Response } from 'express';
import faker from 'faker';

export const generateFeed = () => ({
  user: {
    id: faker.random.uuid(),
    name: faker.name.firstName(),
    picture: faker.image.avatar(),
  },
  date: faker.date.recent(),
  likes: Array.from({ length: 4 }, () => null).map(() => ({
    id: faker.random.uuid(),
    userId: faker.random.uuid(),
    userName: faker.name.firstName(),
  })),
  type: 'Rating',
  book: {
    title: faker.name.title(),
    author: faker.name.firstName(),
    cover: faker.image.abstract(),
    summary: faker.lorem.paragraphs(3),
  },
});

export const getFeeds = (_: Request, res: Response) => {
  const feeds = Array.from({ length: 10 }, () => null).map(generateFeed);

  return res.json({ status: 'success', data: feeds });
};
