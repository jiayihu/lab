(function() {
  const btn = document.querySelector('.btn-replay-subject');

  const interval$ = Rx.Observable.interval(1000).take(3);

  btn.addEventListener('click', function() {
    const subject = new Rx.ReplaySubject(2);
    interval$.subscribe(subject);

    subject.subscribe(x => console.log('First subscriber:', x));
    subject.subscribe(x => console.log('Second subscriber:', x));
    subject.subscribe({ complete: () => console.log('Completed') });

    window.setTimeout(() => {
      // Values are replayed even after complete, differently from BehaviourSubject
      subject.subscribe({
        next: x => console.log('Late subscriber', x),
        complete: () => console.log('Completed'),
      });
    }, 3000);
  });
})();
