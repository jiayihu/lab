(function() {
  const btn = document.querySelector('.btn-publish-replay');

  const clicks$ = Rx.Observable.fromEvent(btn, 'click')

  /**
   * Without .refCount() we have to manually call .connect(), which is the same as
   * calling .subscribe(). Without .refCount() and .connect() the Observable 
   * won't start to execute as shared Observable and nothing happens on click.
   * With .refCount(), which stands for 'reference count', the Observable will keep
   * count of the number of subscribers. It will start execute as shared on first
   * subscribers and unsubscribe when there are none.
   * @see http://reactivex.io/rxjs/manual/overview.html#reference-counting
   */
  const source$ = clicks$
    .do(() => console.log('Source is emitting'))
    .publishReplay(1) // Replays the last n values on late subscribers
    .refCount();
  
  // source$.connect();

  source$.subscribe(() => console.log('First subscriber'));
  source$.subscribe(() => console.log('Second subscriber'));
  
  clicks$
    .take(1)
    .delay(2000)
    .do(() => source$.subscribe(() => console.log('Third late subscriber')))
    .subscribe(); // Without subscribe the observable doesn't run
})();
