(function() {
  const btn = document.querySelector('.btn-catch1');

  const letters$ = Rx.Observable.of('a', 'b', 'c', 2)
    .map(letter => letter.toUpperCase());
  const caught$ = letters$.catch(error => Rx.Observable.empty());

  Rx.Observable.fromEvent(btn, 'click')
    .switchMapTo(caught$)
    .subscribe(x => console.log(x));
})();

(function() {
  const btn = document.querySelector('.btn-catch2');

  let time = 0;
  const letters$ = Rx.Observable.of('a', 'b', 'c', 2)
    .map(letter => letter.toUpperCase());
  const retry$ = letters$.catch((err, caught$) => {
    if (time < 2) {
      time += 1;
      return caught$;
    }

    return Rx.Observable.empty();
  });

  Rx.Observable.fromEvent(btn, 'click')
    .switchMapTo(retry$)
    .subscribe(x => console.log(x));
})();
