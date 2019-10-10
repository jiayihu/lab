import { Bexpr, Ass, Aexpr } from '../syntax';
import { State, substState, stateOps, isBottomState, bottomState } from './state';

export type Domain<T> = {
  le: (a: T) => (b: T) => boolean;
  bottom: T;
  top: T;
  join: (a: T) => (b: T) => T;
  meet: (a: T) => (b: T) => T;

  evalAexpr: (ass: Aexpr) => (s: State<T>) => T;
  test: (bexpr: Bexpr) => (s: State<T>) => State<T>;
  widening: (x: T) => (y: T) => T;
};

export const isBottom = <T>(domain: Domain<T>) => (x: unknown): x is Domain<T>['bottom'] => {
  return x === domain.bottom;
};

export const assign = <T>(domain: Domain<T>) => (ass: Ass) => (s: State<T>): State<T> => {
  return substState(s)(ass.name)(domain.evalAexpr(ass.aexpr)(s));
};

export const fallbackAssign = <T>(domain: Domain<T>) => (ass: Ass) => (s: State<T>): State<T> => {
  return substState(s)(ass.name)(domain.top);
};

export const fallbackTest = <T>(domain: Domain<T>) => (bexpr: Bexpr) => (s: State<T>): State<T> => {
  if (isBottomState(s)) return s;

  if (bexpr.type === 'True') return s;
  if (bexpr.type === 'False') return bottomState;
  if (bexpr.type === 'And') {
    const test1 = fallbackTest(domain)(bexpr.bexpr1)(s);
    const test2 = fallbackTest(domain)(bexpr.bexpr1)(s);

    return stateOps.meet(domain)(test1)(test2);
  }

  return s;
};
