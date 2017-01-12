(function() {
  const btn = document.querySelector('.btn-multicast');

  btn.addEventListener('click', function() {
    const source$ = Rx.Observable.interval(500)
      /**
       * There are shortcuts for each Subject type
       * .publish() = .multicast(new Rx.Subject())
       * .publishBehaviour(0) = .multicast(new Rx.BehaviourSubject(0))
       * .publishReplay(2) = .multicast(new Rx.ReplaySubject()2)
       * .publishLast() = .multicast(new Rx.AsyncSubject())
       * 
       * .share() = .publish().refCount()
       */
      .multicast(new Rx.ReplaySubject(2));
      

    /**
     * Without .connect() the multicasting Observable won't start executing.
     * It's similar to `interval$.subscribe(subject);` in ReplaySubject example.
     * @Note: the source$ will start emitting despite there are no observers yet,
     * because actually we are subscribing the inner ReplaySubject(2) to it.
     * It will also continue emitting even after the observers have unsubscribed!!
     * This is a potential memory leak.
     */
    // const sourceSub = source$.connect();
    /**
     * With .refCount() the number of observers is kept in memory
     * When 0 => 1 `source$.connect()` will be called
     * When 1 => 0 `sourceSub.unsubscribe()` will be called
     */
    const sourceSub = source$.refCount();

    // Will receive (2-3)4--5--6--7|
    window.setTimeout(() => {
      source$.subscribe(x => console.log('First subscriber:', x));
    }, 2000);

    // Will receive (45)6--7|
    window.setTimeout(() => {
      source$.subscribe(x => console.log('Late subscriber:', x));
    }, 3000);
  });
})();
