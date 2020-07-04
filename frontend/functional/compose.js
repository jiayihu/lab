// Example Data
const CARS = [
  { name: 'Ferrari FF', horsepower: 660, dollar_value: 700000, in_stock: true },
  { name: 'Spyker C12 Zagato', horsepower: 650, dollar_value: 648000, in_stock: false },
  { name: 'Jaguar XKR-S', horsepower: 550, dollar_value: 132000, in_stock: false },
  { name: 'Audi R8', horsepower: 525, dollar_value: 114200, in_stock: false },
  { name: 'Aston Martin One-77', horsepower: 750, dollar_value: 1850000, in_stock: true },
  { name: 'Pagani Huayra', horsepower: 700, dollar_value: 1300000, in_stock: false },
];

const trace = R.curry(function(tag, x) {
  console.log(tag, x);
  return x;
});

// Exercise 1:
// ============
// use R.compose() to rewrite the function below. Hint: R.prop() is curried.
const isLastInStock = function(cars) {
  const reversed_cars = R.last(cars);
  return R.prop('in_stock', reversed_cars);
};

const fIsLastInStock = R.compose(R.prop('in_stock'), R.last);

// console.log(isLastInStock(CARS), fIsLastInStock(CARS));

// Exercise 2:
// ============
// use R.compose(), R.prop() and R.head() to retrieve the name of the first car
const nameOfFirstCar = R.compose(R.prop('name'), R.head);

// console.log(nameOfFirstCar(CARS));

// Exercise 3:
// ============
// Use the helper function _average to refactor averageDollarValue as a composition
const _average = function(xs) {
  return R.reduce(R.add, 0, xs) / xs.length;
}; // <- leave be

const averageDollarValue = function(cars) {
  const dollar_values = R.map(function(c) {
    return c.dollar_value;
  }, cars);
  return _average(dollar_values);
};

const fAvarageDollarValue = R.compose(_average, R.map(R.prop('dollar_value')));

// console.log(averageDollarValue(CARS), fAvarageDollarValue(CARS));

// Exercise 4:
// ============
// Write a function: sanitizeNames() using compose that takes an array of cars and returns a list of lowercase and underscored names: e.g: sanitizeNames([{name: "Ferrari FF"}]) //=> ["ferrari_ff"].

const _underscore = R.replace(/\W+/g, '_'); //<-- leave this alone and use to sanitize

const sanitizeNames = R.compose(R.map(_underscore), R.map(R.toLower), R.map(R.prop('name')));

// console.log(sanitizeNames(CARS));

// Bonus 1:
// ============
// Refactor availablePrices with compose.

const formatMoney = function(number) {
  return number + '€';
};

const availablePrices = function(cars) {
  const available_cars = R.filter(R.prop('in_stock'), cars);
  return available_cars
    .map(function(x) {
      return formatMoney(x.dollar_value);
    })
    .join(', ');
};

const fAvailablePrices = R.compose(
  R.join(', '),
  R.map(formatMoney),
  R.map(R.prop('dollar_value')),
  R.filter(R.prop('in_stock'))
);

// console.log(availablePrices(CARS), fAvailablePrices(CARS));

// Bonus 2:
// ============
// Refactor to pointfree. Hint: you can use R.flip()

const fastestCar = function(cars) {
  const sorted = R.sortBy(function(car) {
    return car.horsepower;
  }, cars);
  const fastest = R.last(sorted);
  return fastest.name + ' is the fastest';
};

const fFastestCar = R.compose(
  R.flip(R.concat)(' is the fastest'),
  R.prop('name'),
  R.last,
  R.sortBy(R.prop('horsepower'))
);

// console.log(fastestCar(CARS), fFastestCar(CARS));
