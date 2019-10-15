import { Bexpr, Ass, Aexpr, Or, Neg, And } from '../syntax';
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

export const assign = <T>(domain: Domain<T>) => (ass: Ass) => (s: State<T>): State<T> => {
  const value = domain.evalAexpr(ass.aexpr)(s);

  // Coalescing bottom values to bottom state
  if (value === domain.bottom) return bottomState;

  return substState(s)(ass.name)(value);
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
    case 'Or': {
      const test1 = fallbackTest(domain)(bexpr.bexpr1)(s);
      const test2 = fallbackTest(domain)(bexpr.bexpr1)(s);

      return stateOps.join(domain)(test1)(test2);
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
          // De Morgan !(a & b) = !a | !b
          return fallbackTest(domain)(new Or(new Neg(negBexpr.bexpr1), new Neg(negBexpr.bexpr2)))(
            s,
          );
        case 'Or':
          // De Morgan !(a | b) = !a & !b
          return fallbackTest(domain)(new And(new Neg(negBexpr.bexpr1), new Neg(negBexpr.bexpr2)))(
            s,
          );
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
