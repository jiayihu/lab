import { Logger } from '@nestjs/common';
import amqp, { AmqpConnectionManager, ChannelWrapper } from 'amqp-connection-manager';
import { ConfirmChannel } from 'amqplib';
import { IEvent } from '@nestjs/cqrs';
import { RmqOptions } from '@nestjs/microservices';

export class EventPublisher {
  connection: AmqpConnectionManager;
  channel: ChannelWrapper;

  constructor(private options: RmqOptions['options']) {
    this.connection = amqp.connect(options.urls);
    this.channel = this.connection.createChannel({
      json: false,
      setup: (channel: ConfirmChannel) => {
        channel.assertExchange(options.queue, 'fanout');
      },
    });
  }

  publish(event: IEvent) {
    this.channel
      .publish(this.options.queue, '', Buffer.from(JSON.stringify(event)))
      .catch(err => Logger.error('The published event was rejected', err));
  }
}
