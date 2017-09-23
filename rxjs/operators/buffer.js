(function() {
/**
 * Print on console only when button is clicked twice and only twice
 */

const btn = document.querySelector('.btn-buffer');

const clicks$ = Rx.Observable.fromEvent(btn, 'click');
const doubleClicks$ = clicks$.buffer(clicks$.debounceTime(250))
  .filter(arr => arr.length === 2);

doubleClicks$.subscribe(() => console.log('Double clicked!'));
})();
