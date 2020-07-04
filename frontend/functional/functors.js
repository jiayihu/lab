(function() {
  const Identity = RF.Identity;
  const Maybe = RF.Maybe;
  const Either = RF.Either;
  const Left = Either.Left;
  const Right = Either.Right;
  const IO = RF.IO;
  const Task = RF.Future;

  // Exercise 1
  // ==========
  // Use R.add(x,y) and R.map(f,x) to make a function that increments a value inside a functor

  const ex1 = R.map(R.add(1));

  // console.log(ex1(Identity(2)));

  // Exercise 2
  // ==========
  // Use R.head to get the first element of the list
  const xs = Identity.of(['do', 'ray', 'me', 'fa', 'so', 'la', 'ti', 'do']);

  const ex2 = R.map(R.head);

  // console.log(ex2(xs));

  // Exercise 3
  // ==========
  // Use safeProp and R.head to find the first initial of the user
  const safeProp = R.curry(function(x, o) {
    return Maybe.of(o[x]);
  });

  const user = { id: 2, name: 'Albert' };

  const ex3 = R.compose(R.map(R.head), safeProp('name'));

  // console.log(ex3(user));

  // Exercise 4
  // ==========
  // Use Maybe to rewrite ex4 without an if statement

  const ex4 = function(n) {
    if (n) {
      return parseInt(n);
    }
  };

  const fex4 = R.compose(R.map(parseInt), Maybe.of);

  // console.log(fex4('4'));

  // Exercise 5
  // ==========
  // Write a function that will getPost then R.toUpper the post's title

  // getPost :: Int -> Future({id: Int, title: String})
  const getPost = function(i) {
    return new Task(function(reject, resolve) {
      setTimeout(function() {
        resolve({ id: i, title: 'Love them futures' });
      }, 300);
    });
  };

  const ex5 = R.compose(R.map(R.compose(R.toUpper, R.prop('title'))), getPost);

  // ex5().fork(err => console.error(err), data => console.log(data));

  // Exercise 6
  // ==========
  // Write a function that uses checkActive() and showWelcome() to grant access or return the error

  const showWelcome = R.compose(R.concat('Welcome '), R.prop('name'));

  const checkActive = function(user) {
    return user.active ? Right(user) : Left('Your account is not active');
  };

  const ex6 = R.compose(R.map(showWelcome), checkActive);
  // console.log(ex6({ active: false, name: 'Gary' }));
  // console.log(ex6({ active: true, name: 'Theresa' }));

  // Exercise 7
  // ==========
  // Write a validation function that checks for a length > 3. It should return Right(x) if it is greater than 3 and Left("You need > 3") otherwise

  const ex7 = function(x) {
    return x.length > 3 ? Right(x) : Left('You need > 3'); // <--- write me. (don't be pointfree)
  };

  // console.log(ex7('fpguy99'));
  // console.log(ex7('...'));

  // Exercise 8
  // ==========
  // Use ex7 above and either as a functor to save the user if they are valid or return the error message string. Remember either's two arguments must return the same type.

  const save = function(x) {
    return new IO(function() {
      console.log('SAVED USER!');
      return x + '-saved';
    });
  };

  const ex8 = R.compose(Either.either(IO.of, save), ex7);

  // console.log(ex8('fpguy99').runIO());
  // console.log(ex8('...').runIO());
})();
