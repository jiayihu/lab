import { Z, Aexpr, Bexpr, Or, Neg, And } from '../syntax';
import { Domain } from './domain';
import { State, isBottomState, bottomState, stateOps, substState } from './state';
import { Brand } from '../utils';

type NegInf = Brand<number, 'NegInf'>;
type PosInf = Brand<number, 'PosInf'>;

class Bottom {
  type: 'Bottom' = 'Bottom';
}

export const negInf: NegInf = -Infinity as any;
export const posInf: PosInf = Infinity as any;
export const bottom = new Bottom();
export const top: Interval = [negInf, posInf];

type Bound = Z | NegInf | PosInf;

type Interval = [Bound, Bound] | Bottom;

const isNegInf = (x: Bound): x is NegInf => {
  return x === -Infinity;
};

const isPosInf = (x: Bound): x is PosInf => {
  return x === Infinity;
};

const isBottom = (x: Interval): x is Bottom => {
  return !Array.isArray(x);
};

const eq = (x: Interval) => (y: Interval): boolean => {
  if (isBottom(x)) return isBottom(y);
  if (isBottom(y)) return isBottom(x);

  const [a, b] = x;
  const [c, d] = y;

  return a === c && b === d;
};

const le = (x: Interval) => (y: Interval): boolean => {
  if (isBottom(x)) return true;
  if (isBottom(y)) return false;

  const [a, b] = x;
  const [c, d] = y;

  return a >= c && b <= d;
};

const join = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x)) return y;
  if (isBottom(y)) return x;

  const [a, b] = x;
  const [c, d] = y;

  return [Math.min(a, c), Math.max(b, d)];
};

const meet = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  if (Math.max(a, c) <= Math.min(b, d)) return [Math.max(a, c), Math.min(b, d)];

  return bottom;
};

const addIntervals = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  // Adding negInf to posInf
  if (Number.isNaN(a + c) || Number.isNaN(b + d)) return top;

  return [a + c, b + d];
};

const subIntervals = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  // Subtracting posInf to posInf
  if (Number.isNaN(a - c) || Number.isNaN(b - d)) return top;

  return [a - d, b - c];
};

const mult = (x: Bound, y: Bound): Bound => {
  if (x === 0 || y === 0) return 0;

  return x * y;
};

const multIntervals = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  const lowerBound = Math.min(mult(a, c), mult(a, d), mult(b, c), mult(b, d));
  const upperBound = Math.max(mult(a, c), mult(a, d), mult(b, c), mult(b, d));

  return [lowerBound, upperBound];
};

const div = (x: Bound, y: Bound): Bound => {
  if (isPosInf(y) || isNegInf(y)) return 0;

  return x / y;
};

const divIntervals = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  // If the divisor can be zero bottom is returned, which could lead to false alarms
  if (c <= 0 && d >= 0) return bottom;

  if (c >= 1) return [Math.min(div(a, c), div(a, d)), Math.max(div(b, c), div(b, d))];
  if (d <= -1) return [Math.min(div(b, c), div(b, d)), Math.max(div(a, c), div(a, d))];

  // Divisor is both positive and negative
  const posPart = divIntervals([a, b])(meet([c, d])([1, posInf]));
  const negPart = divIntervals([a, b])(meet([c, d])([negInf, -1]));

  return join(posPart)(negPart);
};

const evalAexpr = (expr: Aexpr) => (s: State<Interval>): Interval => {
  switch (expr.type) {
    case 'Num':
      return [expr.value, expr.value];
    case 'Var':
      return isBottomState(s) ? intervalDomain.bottom : s(expr.value);
    case 'Add': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return addIntervals(a1)(a2);
    }
    case 'Sub': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return subIntervals(a1)(a2);
    }
    case 'Mult': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return multIntervals(a1)(a2);
    }
    case 'Div': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return divIntervals(a1)(a2);
    }
  }
};

const diffIntervals = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x)) return bottom;
  if (isBottom(y)) return x;

  const [a, b] = x;
  const [c, d] = y;

  // Leave the interval intact since we cannot express intervals with a "hole"
  if (le(y)(x)) return x;

  if (d >= b) return c <= a ? bottom : [a, Math.min(b, c - 1)];

  // c <= a
  return d >= b ? bottom : [Math.max(a, d + 1), b];
};

const test = (bexpr: Bexpr) => (s: State<Interval>): State<Interval> => {
  if (isBottomState(s)) return s;

  switch (bexpr.type) {
    case 'True':
      return s;
    case 'False':
      return bottomState;
    case 'And': {
      const test1 = test(bexpr.bexpr1)(s);
      const test2 = test(bexpr.bexpr1)(s);

      return stateOps.meet(intervalDomain)(test1)(test2);
    }
    case 'Or': {
      const test1 = test(bexpr.bexpr1)(s);
      const test2 = test(bexpr.bexpr1)(s);

      return stateOps.join(intervalDomain)(test1)(test2);
    }
    case 'Neg': {
      const negBexpr = bexpr.value;

      switch (negBexpr.type) {
        case 'True':
          return bottomState;
        case 'False':
          return s;
        case 'Neg':
          return test(negBexpr.value)(s);
        case 'And':
          // De Morgan !(a & b) = !a | !b
          return test(new Or(new Neg(negBexpr.bexpr1), new Neg(negBexpr.bexpr2)))(s);
        case 'Or':
          // De Morgan !(a | b) = !a & !b
          return test(new And(new Neg(negBexpr.bexpr1), new Neg(negBexpr.bexpr2)))(s);
        case 'Eq': {
          const a1 = evalAexpr(negBexpr.aexpr1)(s);
          const a2 = evalAexpr(negBexpr.aexpr2)(s);

          // Same information
          if (eq(a1)(a2)) return bottomState;

          const met = meet(a1)(a2);

          // No information in common
          if (met === bottom) return s;

          let s1: State<Interval> = s;

          /**
           * If there is some overlapping information, it's removed from both
           * values although it could lead to some false alarms. For instance:
           * a1 := [1, 5], a2 := [4, 7]
           * => met := [2, 5]
           * => a1 := [1, 1], a2 := [2, 7] OR a1 := [1, 5], a2 := [6, 7]
           * Both are valid cases, but the test will return a stricter
           * a1 := [1, 1], a2 := [6, 7]
           */

          if (negBexpr.aexpr1.type === 'Var' && !eq(a1)(met)) {
            const v1 = diffIntervals(a1)(met);
            s1 = substState(s1)(negBexpr.aexpr1.value)(v1);
          }
          if (negBexpr.aexpr2.type === 'Var' && !eq(a2)(met)) {
            const v2 = diffIntervals(a2)(met);
            s1 = substState(s1)(negBexpr.aexpr2.value)(v2);
          }

          return s1;
        }
        case 'Le': {
          const x = evalAexpr(negBexpr.aexpr1)(s);
          const y = evalAexpr(negBexpr.aexpr2)(s);

          if (isBottom(x) || isBottom(y)) return bottomState;

          // ! x <= y  <=>  x > y  <=> y < x
          // Similar to normal Le but inverting x with y and careful with not equal
          const [a, b] = x;
          const [c, d] = y;

          if (b < c) return bottomState;

          let s1: State<Interval> = s;

          if (negBexpr.aexpr1.type === 'Var') {
            s1 = substState(s1)(negBexpr.aexpr1.value)([Math.max(a, c + 1), b]);
          }
          if (negBexpr.aexpr2.type === 'Var') {
            s1 = substState(s1)(negBexpr.aexpr2.value)([c, Math.min(b - 1, d)]);
          }

          return s1;
        }
      }

      return s;
    }
    case 'Eq': {
      const a1 = evalAexpr(bexpr.aexpr1)(s);
      const a2 = evalAexpr(bexpr.aexpr2)(s);

      const met = meet(a1)(a2);

      if (met === bottom) return bottomState;

      let s1: State<Interval> = s;

      if (bexpr.aexpr1.type === 'Var') s1 = substState(s1)(bexpr.aexpr1.value)(met);
      if (bexpr.aexpr2.type === 'Var') s1 = substState(s1)(bexpr.aexpr2.value)(met);

      return s1;
    }
    case 'Le': {
      const x = evalAexpr(bexpr.aexpr1)(s);
      const y = evalAexpr(bexpr.aexpr2)(s);

      if (isBottom(x) || isBottom(y)) return bottomState;

      const [a, b] = x;
      const [c, d] = y;

      if (a > d) return bottomState;

      let s1: State<Interval> = s;

      if (bexpr.aexpr1.type === 'Var') {
        s1 = substState(s1)(bexpr.aexpr1.value)([a, Math.min(b, d)]);
      }
      if (bexpr.aexpr2.type === 'Var') {
        s1 = substState(s1)(bexpr.aexpr2.value)([Math.max(a, c), d]);
      }

      return s1;
    }
  }
};

const widen = (x: Interval) => (y: Interval): Interval => {
  // Actually values cannot be bottom because of State definition
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  const lowerBound = a <= c ? a : negInf;
  const upperBound = b >= d ? b : posInf;

  return [lowerBound, upperBound];
};

export const intervalDomain: Domain<Interval> = {
  le,
  bottom,
  top,
  join,
  meet,

  evalAexpr,
  test,
  widen,
};
