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
  widen: (x: T) => (y: T) => T;
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

  switch (bexpr.type) {
    case 'True':
      return s;
    case 'False':
      return bottomState;
    case 'And': {
      const test1 = fallbackTest(domain)(bexpr.bexpr1)(s);
      const test2 = fallbackTest(domain)(bexpr.bexpr1)(s);

      return stateOps.meet(domain)(test1)(test2);
    }
    case 'Neg': {
      const negBexpr = bexpr.value;

      switch (negBexpr.type) {
        case 'True':
          return bottomState;
        case 'False':
          return s;
        case 'Neg':
          return fallbackTest(domain)(negBexpr.value)(s);
        case 'And':
          // Logical OR
          if (negBexpr.bexpr1.type === 'And' && negBexpr.bexpr2.type === 'And') {
            const test1 = fallbackTest(domain)(negBexpr.bexpr1)(s);
            const test2 = fallbackTest(domain)(negBexpr.bexpr1)(s);

            return stateOps.join(domain)(test1)(test2);
          }
        case 'Eq':
        case 'Le':
          return s;
      }
    }
    case 'Eq':
    case 'Le':
      return s;
  }
};

// Naive widening
export const fallbackWiden = <T>(domain: Domain<T>) => (x: T) => (y: T): T => {
  return domain.le(y)(x) ? x : domain.top;
};
