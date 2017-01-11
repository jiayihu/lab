(function() {
  const fetchBtn = document.querySelector('.btn-combine-latest1');
  const logBtn = document.querySelector('.btn-combine-latest2');

  const users$ = Rx.Observable.fromEvent(fetchBtn, 'click')
    .throttleTime(250)
    .map(() => 'https://jsonplaceholder.typicode.com/users')
    .switchMap(endpoint => Rx.Observable.fromPromise(fetch(endpoint)))
    .switchMap(response => response.json());

  const logClicks$ = Rx.Observable.fromEvent(logBtn, 'click');

  /**
   * Observable.combineLatest() returns a stream which emits whenever any source
   * emits.
   */

  Rx.Observable.combineLatest(users$, logClicks$, (users) => users)
    .subscribe(users => console.log('combineLatest:', users));
})();
