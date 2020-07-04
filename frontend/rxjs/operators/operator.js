(function() {
  const btn = document.querySelector('.btn-operator');

  Rx.Observable.prototype.multiplyBy = function(multiplier) {
    const source$ = this;
    return Rx.Observable.create(function(observer) {
      source$.subscribe({
        next: (x) => observer.next(x * multiplier),
        error: (err) => observer.error(err),
        complete: () => observer.complete(),
      });
    });
  }


  const source$ = Rx.Observable.from([1, 2, 3])
    .multiplyBy(10);
  
  btn.addEventListener('click', function() {
    source$.subscribe({
      next: (x) => console.log(x),
      complete: () => console.log('Completed!'),
    });
  });
})();
