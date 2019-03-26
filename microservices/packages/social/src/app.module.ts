import { Module } from '@nestjs/common';
import { MongooseModule } from '@nestjs/mongoose';
import { FeedsModule } from './feeds/feeds.module';

@Module({
  imports: [
    MongooseModule.forRoot(`${process.env.MONGO || 'mongodb://localhost:27017'}/feeds`),
    FeedsModule,
  ],
  controllers: [],
  providers: [],
})
export class AppModule {}
