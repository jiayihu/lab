(function() {
  const btn = document.querySelector('.btn-merge');

  const source1$ = Rx.Observable.interval(500).startWith('streamA').take(4);
  const source2$ = Rx.Observable.interval(300).startWith('streamB').take(6);

  const allSources$ = source1$.merge(source2$);
  
  Rx.Observable.fromEvent(btn, 'click')
    .switchMapTo(allSources$)
    .subscribe(x => console.log(x));
})();
