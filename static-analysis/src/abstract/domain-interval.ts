import { Z, Aexpr } from '../syntax';
import { Domain, fallbackTest } from './domain';
import { State, isBottomState } from './state';
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

const addInts = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  // Adding negInf to posInf
  if (Number.isNaN(a + c) || Number.isNaN(b + d)) return top;

  return [a + c, b + d];
};

const subInts = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  // Subtracting posInf to posInf
  if (Number.isNaN(a - c) || Number.isNaN(b - d)) return top;

  return [a - c, b - d];
};

const mult = (x: Bound) => (y: Bound): Bound => {
  if (x === 0 || y === 0) return 0;

  return x * y;
};

const multInts = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  const lowerBound = Math.min(mult(a)(c), mult(a)(d), mult(b)(c), mult(b)(d));
  const upperBound = Math.max(mult(a)(c), mult(a)(d), mult(b)(c), mult(b)(d));

  return [lowerBound, upperBound];
};

const div = (x: Bound, y: Bound): Bound => {
  if (isPosInf(y) || isNegInf(y)) return 0;

  return x / y;
};

const divInts = (x: Interval) => (y: Interval): Interval => {
  if (isBottom(x) || isBottom(y)) return bottom;

  const [a, b] = x;
  const [c, d] = y;

  if (c === 0 || d === 0) return bottom;

  if (c >= 1) return [Math.min(div(a, c), div(a, d)), Math.max(div(b, c), div(b, d))];
  if (d <= -1) return [Math.min(div(b, c), div(b, d)), Math.max(div(a, c), div(a, d))];

  // Divisor is both positive and negative
  const posPart = divInts([a, b])(meet([c, d])([1, posInf]));
  const negPart = divInts([a, b])(meet([c, d])([negInf, -1]));

  return join(posPart)(negPart);
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

const evalAexpr = (expr: Aexpr) => (s: State<Interval>): Interval => {
  switch (expr.type) {
    case 'Num':
      return [expr.value, expr.value];
    case 'Var':
      return isBottomState(s) ? intervalDomain.bottom : s(expr.value);
    case 'Add': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return addInts(a1)(a2);
    }
    case 'Sub': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return subInts(a1)(a2);
    }
    case 'Mult': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return multInts(a1)(a2);
    }
    case 'Div': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return divInts(a1)(a2);
    }
  }
};

export const intervalDomain: Domain<Interval> = {
  le,
  bottom,
  top,
  join,
  meet,

  evalAexpr,
  get test() {
    return fallbackTest(intervalDomain);
  },
  widen,
};
