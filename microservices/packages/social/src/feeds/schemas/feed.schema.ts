import { Schema } from 'mongoose';

const feedSchema = new Schema({
  userId: Schema.Types.ObjectId,
  date: Date,
  type: String,
  bookId: Schema.Types.ObjectId,
});

export { feedSchema };
