const ITERATIONS_LENGTH = 200000;
const ITERATIONS_PER_FRAME = 100000;

function* nonBlockingReduce(iterable, transformer, initialValue, maxTime = 200) {
  let result = initialValue

  for (let [key, value] of iterable) {
    const item = value || key
    const itemIterator = transformer(result, item, key, iterable);

    yield new Promise(resolve => {
      let animToken, start;

      const step = (timestamp) => { // timestamp not used
        let iteration = itemIterator.next();

        for (let i = 0; i < ITERATIONS_PER_FRAME && !iteration.done; i++) {
          iteration = itemIterator.next();
        }

        if (!iteration.done) window.requestAnimationFrame(step);
        else {
          console.log(`item transformation is done with latest iteration`, iteration);
          result = iteration.value;
          window.cancelAnimationFrame(animToken);
          resolve();
        }
      };

      animToken = window.requestAnimationFrame(step);
    });
  }

  return result
}


function* doTask() {
  const iterable = ['c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o', 'c', 'i', 'a', 'o']
  const iterations = 10000;

  function* transformer(acc, item) {
    // trivial implementation to generate a delay and
    // make `nonBlockingReduce` split the recursion
    let x = 0
    while (x < ITERATIONS_LENGTH) {
      x = x + 1;
      yield;
    }

    // simple `reducer` function composing a string
    return `${acc}${item}`
  }

  return yield* nonBlockingReduce(iterable, transformer, '');
}

window.setTimeout(() => {
  console.log(
    `Starting doTask with ${ITERATIONS_LENGTH} iterations per item,
    split in chunks by ${ITERATIONS_PER_FRAME} iterations per frame`);
  const task = co(doTask);
  task.then((r) => {
    console.log(`Reduced value: ${r}`);
  })
}, 4000);
