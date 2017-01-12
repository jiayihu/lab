(function() {
  const btn = document.querySelector('.btn-async-subject');

  btn.addEventListener('click', function() {
    const users$ = Rx.Observable.fromPromise(fetch('https://jsonplaceholder.typicode.com/users'))
    .do(() => console.log('Users requested'))
    .switchMap(response => response.json());
    
    const subject = new Rx.AsyncSubject();
    users$.subscribe(subject);

    subject.subscribe(x => console.log('First subscriber:', x));

    window.setTimeout(() => {
      subject.subscribe(x => console.log('Late subscriber', x));
    }, 2000);
  });
})();
