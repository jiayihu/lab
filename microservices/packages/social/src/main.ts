import { NestFactory } from '@nestjs/core';
import { Transport } from '@nestjs/microservices';
import { AppModule } from './app.module';

require('dotenv').config();

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  const microservice = app.connectMicroservice({
    transport: Transport.TCP,
    options: {
      port: Number(process.env.PORT) || 4200,
    },
  });

  microservice.listen(() => console.log('Feeds microservice up & running'));

  await app.listen(process.env.PORT || 8080);
}
bootstrap();
