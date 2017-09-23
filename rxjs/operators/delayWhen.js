(function() {
const btn = document.querySelector('.btn-delay-when');

const numbers$ = Rx.Observable.of(0, 1, 2, 3, 4, 5);
const delayOdd$ = numbers$.delayWhen(x => {
  if (x % 2) return Rx.Observable.of(1);

  return Rx.Observable.interval(x * 1000).first();
});

Rx.Observable.fromEvent(btn, 'click')
    .switchMapTo(delayOdd$)
    .subscribe(x => console.log(x));
})();
