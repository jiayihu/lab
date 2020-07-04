const Either = RF.Either;
const Left = Either.Left;
const Right = Either.Right;
const Maybe = RF.Maybe;
const IO = RF.IO;
const Task = RF.Future;

// fib browser for test
const localStorage = {};

const liftA2 = R.liftN(2);

// Exercise 1
// ==========
// Write a function that add's two possibly null numbers together using Maybe and ap()

//  ex1 :: Number -> Number -> Maybe Number
const ex1 = function(x, y) {
  return Maybe.of(R.add)
    .ap(Maybe.of(x))
    .ap(Maybe.of(y));
};

console.log(ex1(2, 3));

// Exercise 2
// ==========
// Now write a function that takes 2 Maybe's and adds them. Use liftA2 instead of ap().

//  ex2 :: Maybe Number -> Maybe Number -> Maybe Number
const ex2 = R.curry(function(x, y) {
  return liftA2(R.add)(Maybe.of(x), Maybe.of(y));
});

console.log(ex2(2, 3));

// Exercise 3
// ==========
// Run both getPost(n) and getComments(n) then render the page with both. (the n arg is arbitrary)
const makeComments = R.reduce(function(acc, c) {
  return acc + '<li>' + c + '</li>';
}, '');
const render = R.curry(function(p, cs) {
  return '<div>' + p.title + '</div>' + makeComments(cs);
});

//  ex3 :: Task Error HTML
const ex3 = liftA2(render)(getPost(1), getComments(1));

ex3.fork(console.error, console.log);

// Exercise 4
// ==========
// Write an IO that gets both player1 and player2 from the cache and starts the game
localStorage.player1 = 'toby';
localStorage.player2 = 'sally';

const getCache = function(x) {
  return new IO(function() {
    return localStorage[x];
  });
};
const game = R.curry(function(p1, p2) {
  return p1 + ' vs ' + p2;
});

//  ex4 :: IO String
const ex4 = liftA2(game)(getCache('player1'), getCache('player2'));

console.log(ex4.runIO());

// TEST HELPERS
// =====================

function getPost(i) {
  return new Task(function(rej, res) {
    setTimeout(function() {
      res({ id: i, title: 'Love them futures' });
    }, 300);
  });
}

function getComments(i) {
  return new Task(function(rej, res) {
    setTimeout(function() {
      res(['This book should be illegal', 'Monads are like space burritos']);
    }, 300);
  });
}
