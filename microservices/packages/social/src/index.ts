import express from 'express';
import bodyParser from 'body-parser';
import dotenv from 'dotenv';
import { getFeeds } from './services/feeds.service';

const app = express();

dotenv.config({ path: '.env' });

app.set('port', process.env.PORT || 8080);
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));

const router = express.Router();

router.get('/feeds', getFeeds);

app.use('/', router);

const server = app.listen(app.get('port'), () => {
  console.log(`App is running at localhost:${app.get('port')} in ${app.get('env')} mode`);
  console.log('Press CTRL-C to stop\n');
});

export default server;
