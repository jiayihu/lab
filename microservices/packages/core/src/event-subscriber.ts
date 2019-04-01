import amqp, { AmqpConnectionManager, ChannelWrapper } from 'amqp-connection-manager';
import { ConfirmChannel } from 'amqplib';
import { IEvent } from '@nestjs/cqrs';
import { RmqOptions } from '@nestjs/microservices';

export class EventSubscriber<T extends IEvent> {
  connection: AmqpConnectionManager;
  channel: ChannelWrapper;
  subscribers: Array<(event: T) => void> = [];

  constructor(options: RmqOptions['options']) {
    this.connection = amqp.connect(options.urls);
    this.channel = this.connection.createChannel({
      json: false,
      setup: (channel: ConfirmChannel) => {
        channel
          .assertExchange(options.queue, 'fanout')
          .then(() => channel.assertQueue('', { exclusive: true }))
          .then(q => {
            return channel.bindQueue(q.queue, options.queue, '').then(() =>
              channel.consume(q.queue, msg => {
                const event = JSON.parse(msg.content.toString());
                this.handleEvent(event);
              }),
            );
          });
      },
    });
  }

  handleEvent(event: T) {
    this.subscribers.forEach(fn => fn(event));
  }

  subscribe(fn: (event: T) => void) {
    this.subscribers = this.subscribers.concat(fn);
  }

  unsubscribe(fn: (event: T) => void) {
    this.subscribers = this.subscribers.filter(x => x !== fn);
  }
}
