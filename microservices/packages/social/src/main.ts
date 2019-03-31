import { NestFactory } from '@nestjs/core';
import { Transport } from '@nestjs/microservices';
import { AppModule } from './app.module';

require('dotenv').config();

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  const microservice = app.connectMicroservice({
    transport: Transport.RMQ,
    options: {
      urls: [process.env.RABBITMQ || 'amqp://localhost:5672'],
      queue: 'social_queue',
      queueOptions: {},
    },
  });

  microservice.listen(() => console.log('Feeds microservice up & running'));

  await app.listen(process.env.PORT || 8080);
}
bootstrap();
