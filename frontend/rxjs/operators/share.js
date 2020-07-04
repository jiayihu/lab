(function() {
  const btn = document.querySelector('.btn-share');

  const clicks$ = Rx.Observable.fromEvent(btn, 'click')

  /**
   * Without .share() every subscriber get its own stream, or copy of the Observable,
   * and 'Source is emitting' is logged 2-3 times for every click. 
   * With .share() just once because the stream is shared.
   */
  const source$ = clicks$
    .do(() => console.log('Source is emitting'))
    .share()

  source$.subscribe(() => console.log('First subscriber'));
  source$.subscribe(() => console.log('Second subscriber'));
  
  clicks$
    .take(1)
    .delay(2000)
    .do(() => source$.subscribe(() => console.log('Third subscriber')))
    .subscribe(); // Without subscribe the observable doesn't run
})();
