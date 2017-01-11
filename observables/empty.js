(function() {
  const btn = document.querySelector('.btn-empty');

  const just7$ = Rx.Observable.empty().startWith(7);
  just7$.subscribe({
    next: x => console.log(x), 
    complete: () => console.log('isEmpty() completed'),
  });

  const users$ = Rx.Observable.fromEvent(btn, 'click')
    .switchMapTo(just7$);

  users$.subscribe(x => console.log(x));
})();
