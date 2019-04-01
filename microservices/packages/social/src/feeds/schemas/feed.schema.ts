import { Schema } from 'mongoose';

const feedSchema = new Schema({
  user: {
    id: Schema.Types.ObjectId,
    name: String,
    picture: String,
  },
  date: Date,
  type: String,
  bookId: Schema.Types.ObjectId,
});

export { feedSchema };
