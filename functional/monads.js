const Either = RF.Either;
const Left = Either.Left;
const Right = Either.Right;
const Maybe = RF.Maybe;
const IO = RF.IO;
const Task = RF.Future;

// Exercise 1
// ==========
// Use safeProp and map/join or chain to safely get the street name when given a user

const safeProp = R.curry(function(x, o) {
  return Maybe.of(o[x]);
});
const user = {
  id: 2,
  name: 'albert',
  address: {
    street: {
      number: 22,
      name: 'Walnut St',
    },
  },
};

const ex1 = R.compose(
  R.map(R.map(safeProp('name'))),
  R.map(safeProp('street')),
  safeProp('address')
);
const ex1b = R.compose(R.chain(safeProp('name')), R.chain(safeProp('street')), safeProp('address'));

console.log(ex1(user), ex1b(user));

// Exercise 2
// ==========
// Use getFile to get the filename, remove the directory so it's just the file, then purely log it.

const __filename = '/Users/jiayi/Desktop/Repo/experiments/functional/monads.js';

const getFile = function() {
  return new IO(function() {
    return __filename;
  });
};

const pureLog = function(x) {
  return new IO(function() {
    console.log(x);
    return 'logged ' + x; // for testing w/o mocks
  });
};

const filename = R.compose(R.last(), R.split('/'));
const ex2 = R.compose(R.chain(pureLog), R.map(filename), getFile);

console.log(ex2().runIO());

// Exercise 3
// ==========
// Use getPost() then pass the post's id to getComments().

const getPost = function(i) {
  return new Task(function(rej, res) {
    setTimeout(function() {
      res({ id: i, title: 'Love them tasks' }); // THE POST
    }, 300);
  });
};

const getComments = function(i) {
  return new Task(function(rej, res) {
    setTimeout(function() {
      res([
        { post_id: i, body: 'This book should be illegal' },
        { post_id: i, body: 'Monads are like smelly shallots' },
      ]);
    }, 300);
  });
};

const ex3 = R.compose(R.chain(getComments), R.map(R.prop('id')), getPost);

ex3(13).fork(err => console.error(err), data => console.log(data));

// Exercise 4
// ==========
// Use validateEmail, addToMailingList and emailBlast to implement ex4's type signature.
// It should safely add a new subscriber to the list, then email everyone with this happy news.

//  addToMailingList :: Email -> IO [Email]
const addToMailingList = (function(list) {
  return function(email) {
    return new IO(function() {
      list.push(email);
      return list;
    });
  };
})([]);

//  emailBlast :: [Email] -> IO String
function emailBlast(list) {
  return new IO(function() {
    return 'emailed: ' + list.join(','); // for testing w/o mocks
  });
}

//  validateEmail :: Email -> Either String Email
const validateEmail = function(x) {
  return x.match(/\S+@\S+\.\S+/) ? new Right(x) : new Left('invalid email');
};

//  ex4 :: Email -> Either String (IO String)
const ex4 = R.compose(
  Either.either(R.identity, R.invoker(0, 'runIO')),
  R.map(R.chain(emailBlast)),
  R.map(addToMailingList),
  validateEmail
);

console.log(ex4('notanemail'));
console.log(ex4('sleepy@grandpa.net'));
