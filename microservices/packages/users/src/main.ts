import { NestFactory } from '@nestjs/core';
import { Transport } from '@nestjs/microservices';
import { AppModule } from './app.module';

require('dotenv').config();

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  const microservice = app.connectMicroservice({
    transport: Transport.TCP,
    options: {
      port: 4201,
    },
  });

  microservice.listen(() => console.log('Users microservice up & running'));

  await app.listen(process.env.PORT || 8080);
}
bootstrap();
