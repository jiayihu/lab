import { hostname } from 'os';
import express from 'express';
import bodyParser from 'body-parser';
import dotenv from 'dotenv';
import mongoose from 'mongoose';

const app = express();

dotenv.config({ path: '.env' });

function connectWithRetry() {
  mongoose
    .connect(process.env.MONGO_URI || 'mongodb://localhost:27017', {
      autoReconnect: true,
      useNewUrlParser: false,
    })
    .then(() => {
      /** ready to use. The `mongoose.connect()` promise resolves to undefined. */
    })
    .catch(err => {
      if (err.message && err.message.match(/failed to connect to server .* on first connect/)) {
        console.error('Failed to connect to mongo on startup - retrying in 5 sec', err);
        setTimeout(connectWithRetry, 5000);
      }
    });
}

connectWithRetry();

type CatModel = mongoose.Document & {
  name: string;
};
const catSchema = new mongoose.Schema({ name: String });
const Cat = mongoose.model<CatModel>('Cat', catSchema);

app.set('port', process.env.PORT || 8080);
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));

const router = express.Router();

router.get('/themes', (_, res) => {
  return res.json({ status: 'success', data: `Hello world editor from ${hostname()}` });
});

router.get('/cats', (_, res) => {
  return Cat.find().then(cats => res.json({ status: 'success', data: cats }));
});

router.post('/cats', (_, res) => {
  const kitty = new Cat({ name: 'Zildjian' });
  return kitty.save().then(() => res.json({ status: 'success', data: 'Meow' }));
});

app.use('/', router);

const server = app.listen(app.get('port'), () => {
  console.log(`App is running at localhost:${app.get('port')} in ${app.get('env')} mode`);
  console.log('Press CTRL-C to stop\n');
});

export default server;
