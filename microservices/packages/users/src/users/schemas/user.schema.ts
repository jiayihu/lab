import { Schema } from 'mongoose';

const userSchema = new Schema({
  name: String,
  picture: String,
});

export { userSchema };
