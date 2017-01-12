(function() {
  const btn = document.querySelector('.btn-subject');

  const clicks$ = Rx.Observable.fromEvent(btn, 'click')

  const source$ = clicks$
    .do(() => console.log('Source is emitting'));

  /**
   * Subjects are both Observables, with .subscribe() and operators, and Observers
   * with next(), error() and complete() methods.
   * The following object is a bare example of `const subject = new Subject()`;
   */
  const subject = {
    next(value) { this.observers.forEach(observer => observer.next(value)); },
    error() {},
    complete() {},
    observers: [],
    // Has .subscribe() method like Observables
    subscribe(observer) {
      this.observers.push(observer);
    }
  };
  // Use the subject as Observer
  source$.subscribe(subject);

  // Use the subject as Observable
  subject.subscribe({ next: () => console.log('First subscriber') });
  subject.subscribe({ next: () => console.log('Second subscriber') });
  
  // Late subscriber
  clicks$
    .take(1)
    .delay(2000)
    .do(function() {
      subject.subscribe({ next: () => console.log('Third subscriber') });

      // Manually emit values to observers with the Subject
      window.setTimeout(function() {
        console.log('Manually emitting with the subject');
        subject.next();
      }, 1000);
    })
    .subscribe(); // Without subscribe the observable doesn't run
})();
