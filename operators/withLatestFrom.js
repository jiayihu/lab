(function() {
  const fetchBtn = document.querySelector('.btn-with-latest-from1');
  const logBtn = document.querySelector('.btn-with-latest-from2');

  const users$ = Rx.Observable.fromEvent(fetchBtn, 'click')
    .throttleTime(250)
    .map(() => 'https://jsonplaceholder.typicode.com/users')
    .switchMap(endpoint => Rx.Observable.fromPromise(fetch(endpoint)))
    .switchMap(response => response.json())
    // Use .share() to call `fetch()` only once, despite the number of subscribers
    .share();

  const logClicks$ = Rx.Observable.fromEvent(logBtn, 'click');

  /**
   * .withLatestFrom() emits only when the source (logClicks$) emits, whereas
   * Observable.combineLatest() returns a stream which emits whenever any source
   * emits.
   */

  logClicks$.withLatestFrom(users$, (ev, users) => users)
    .subscribe(users => console.log('withLatestFrom:', users));

  Rx.Observable.combineLatest(users$, logClicks$, (users) => users)
    .subscribe(users => console.log('combineLatest:', users));
})();
