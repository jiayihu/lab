import { Schema } from 'mongoose';

export const FeedSchema = new Schema({
  user: {
    id: Schema.Types.ObjectId,
    name: String,
    picture: String,
  },
  date: Date,
  type: String,
  likes: [
    {
      id: Schema.Types.ObjectId,
      userId: Schema.Types.ObjectId,
      userName: String,
    },
  ],
  book: {
    title: String,
    author: String,
    cover: String,
    summary: String,
  },
});
