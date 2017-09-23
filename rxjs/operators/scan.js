(function() {
  const btn = document.querySelector('.btn-scan');

  const source1$ = Rx.Observable.of('h', 'e', 'l', 'l', 'o');
  const source2$ = Rx.Observable.interval(1000).take(5);

  const spreadHello$ = Rx.Observable.zip(source1$, source2$, (letter, second) => letter);
  
  Rx.Observable.fromEvent(btn, 'click')
    .switchMapTo(spreadHello$)
    // .scan() is very similar to [].reduce() and Redux can be implemented in
    // RxJS with .scan() indeed
    .scan((acc, letter) => acc + letter, '')
    .subscribe(x => console.log(x));
})();
