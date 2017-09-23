(function() {
  const btn = document.querySelector('.btn-zip');

  const source1$ = Rx.Observable.of('h', 'e', 'l', 'l', 'o');
  const source2$ = Rx.Observable.interval(1000).take(5);

  const spreadHello$ = Rx.Observable.zip(source1$, source2$, (letter, second) => letter);
  
  Rx.Observable.fromEvent(btn, 'click')
    .switchMapTo(spreadHello$)
    .subscribe(x => console.log(x));
})();
