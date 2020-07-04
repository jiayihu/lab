(function() {
  const btn = document.querySelector('.btn-from-promise');

  const users$ = Rx.Observable.fromEvent(btn, 'click')
    .throttleTime(250)
    .map(() => 'https://jsonplaceholder.typicode.com/users')
    .switchMap(endpoint => Rx.Observable.fromPromise(fetch(endpoint)))
    .switchMap(response => response.json());

  users$.subscribe(users => console.log(users));
})();
