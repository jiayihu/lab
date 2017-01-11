(function() {
  const btn = document.querySelector('.btn-create1');

  btn.addEventListener('click', function() {
    function subscribe(observer) {
      observer.next(1);
      observer.next(2);
      observer.next(3);
      observer.complete();
    }
    const source$ = Rx.Observable.create(subscribe);

    const observer = {
      next: function(x) { console.log(x) },
      complete: function() { console.log('Completed'); },
    };
    source$.subscribe(observer);
  })
})();

(function() {
  const createBtn = document.querySelector('.btn-create2');
  const unsubBtn = document.querySelector('.btn-unsubscribe');

  function subscribe(observer) {
    let counter = 0;
    const token = setInterval(function() {
      observer.next(++counter);
    }, 1000);

    return function() {
      clearInterval(token);
      console.log('Unsubscribed!');
    };
  }
  const source$ = Rx.Observable.create(subscribe);

  let subscription;
  createBtn.addEventListener('click', function() {
    if (!subscription) subscription = source$.subscribe(function(x) {
      console.log(x);
    });
  });

  unsubBtn.addEventListener('click', function() {
    if (subscription) subscription.unsubscribe();
  });
})();
