(function() {
  const btn = document.querySelector('.btn-merge-all');

  let counter = 0;
  const interval$ = Rx.Observable.interval(1000).take(6);

  const higherOrderObservable = Rx.Observable.fromEvent(btn, 'click')
    .map(x => ++counter)
    .map(count => interval$.startWith(`stream` + counter)); // Observable of interval Observable

  higherOrderObservable
    // Maximum number of concurrent observables considered. Remaining Observables
    // will be flattened as soon as one current Observable completes.
    .mergeAll(2)
    .subscribe(x => console.log(x));
})();
