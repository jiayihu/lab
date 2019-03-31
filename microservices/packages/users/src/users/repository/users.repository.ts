import { Injectable } from '@nestjs/common';
import { InjectModel } from '@nestjs/mongoose';
import { Model, Document } from 'mongoose';
import { User, createUser } from '../domain/user.model';

@Injectable()
export class UsersRepository {
  constructor(@InjectModel('User') private userODM: Model<User & Document>) {}

  find(): Promise<User[]> {
    return this.userODM
      .find()
      .exec()
      .then(users => users.map(this.asUser));
  }
  findOne(userId: string): Promise<User | null> {
    return this.userODM.findById(userId).then(doc => (doc ? this.asUser(doc) : null));
  }
  create(user: User): Promise<User> {
    return this.userODM.create(user).then(this.asUser);
  }
  delete() {}
  update() {}

  private asUser(doc: User & Document): User {
    return createUser(doc._id.toString(), doc.name, doc.picture);
  }
}
