function assertEq(a, b, message) {
  console.log(message, { a, b });

  if (a !== b) throw new Error(message);
}

fetch('./checkers.wasm')
  .then((response) => response.arrayBuffer())
  .then((bytes) =>
    WebAssembly.instantiate(bytes, {
      events: {
        piecemoved: (fX, fY, tX, tY) => console.log('A piece moved', { fX, fY, tX, tY }),
        piececrowned: (x, y) => console.log('A piece was crowned at', { x, y }),
      },
    }),
  )
  .then((results) => {
    const exports = results.instance.exports;

    exports.initBoard();
    console.log('Turn owner is', exports.getTurnOwner());

    exports.move(0, 2, 1, 3); // B
    exports.move(1, 5, 0, 4); // W
    exports.move(1, 3, 0, 2); // B
    exports.move(0, 4, 1, 5); // W

    console.log('At end, turn owner is ' + exports.getTurnOwner());
  });
