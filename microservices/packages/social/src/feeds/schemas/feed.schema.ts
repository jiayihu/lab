import { Schema } from 'mongoose';

const feedSchema = new Schema({
  state: String,
  userId: Schema.Types.ObjectId,
  date: Date,
  type: String,
  bookId: Schema.Types.ObjectId,
});

export { feedSchema };
