// Exercise 1
//==============
// Refactor to remove all arguments by partially applying the function

const words = function(str) {
  return R.split(' ', str);
};

const fwords = R.split(' ');

// console.log(fwords('Jingle bells Batman smells'));

// Exercise 1a
//==============
// Use map to make a new words fn that works on an array of strings.

const sentences = ['Jingle bells Batman smells', 'Robin laid an egg'];

const fAllWords = R.pipe(R.map(fwords), R.flatten);

// console.log(fAllWords(sentences));

// Exercise 2
//==============
// Refactor to remove all arguments by partially applying the functions

const match = R.curry(function(pattern, string) {
  return string.match(pattern);
});

const filterQs = function(xs) {
  return R.filter(function(x) {
    return match(/q/i, x);
  }, xs);
};

const fFilterQs = R.filter(match(/q/i));

// console.log(fFilterQs(['quick', 'camels', 'quarry', 'over', 'quails']));

// Exercise 3
//==============
// Use the helper function _keepHighest to refactor max to not reference any arguments

// LEAVE BE:
const _keepHighest = function(x, y) {
  return x >= y ? x : y;
};

// REFACTOR THIS ONE:
const max = function(xs) {
  return R.reduce(
    function(acc, x) {
      return _keepHighest(acc, x);
    },
    -Infinity,
    xs
  );
};

const fMax = R.reduce(_keepHighest, -Infinity);

// console.log(max([323, 523, 554, 123, 5234]), fMax([323, 523, 554, 123, 5234]));

// Bonus 1:
// ============
// wrap array's slice to be functional and curried.
// //[1,2,3].slice(0, 2)
const slice = R.curry(function(from, to, array) {
  return array.slice(from, to);
});

// console.log(slice(1, 3)(['a', 'b', 'c']));

// Bonus 2:
// ============
// Use slice to define a function "take" that returns n elements from the beginning of an array. Make it curried.
// For ['a', 'b', 'c'] with n=2 it should return ['a', 'b']
const take = R.curry(function(end, array) {
  return slice(0, end)(array);
});

// console.log(take(2)(['a', 'b', 'c']));
