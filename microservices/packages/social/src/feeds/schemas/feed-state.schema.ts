import { Schema } from 'mongoose';

const feedStateSchema = new Schema({
  state: String,
  userId: Schema.Types.ObjectId,
  date: Date,
  type: String,
  bookId: Schema.Types.ObjectId,
});

export { feedStateSchema };
