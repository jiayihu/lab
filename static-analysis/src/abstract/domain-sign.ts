import { Domain } from './domain';
import { Aexpr, Bexpr, Or, Neg, And } from '../syntax';
import { State, isBottomState, stateOps, bottomState, substState } from './state';

class Bottom {
  type: 'Bottom' = 'Bottom';
}

class LZero {
  type: 'LZero' = 'LZero';
}

class Zero {
  type: 'Zero' = 'Zero';
}

class GZero {
  type: 'GZero' = 'GZero';
}

class LeZero {
  type: 'LeZero' = 'LeZero';
}

class NotZero {
  type: 'NotZero' = 'NotZero';
}

class GeZero {
  type: 'GeZero' = 'GeZero';
}

class Top {
  type: 'Top' = 'Top';
}

export const bottom = new Bottom();
export const lZero = new LZero();
export const zero = new Zero();
export const gZero = new GZero();
export const leZero = new LeZero();
export const notZero = new NotZero();
export const geZero = new GeZero();
export const top = new Top();

type Sign = Bottom | LZero | Zero | GZero | LeZero | NotZero | GeZero | Top;

const isBottom = (x: Sign): x is Bottom => {
  return x.type == 'Bottom';
};

function index(a: Sign): number {
  switch (a.type) {
    case 'Bottom':
      return 0;
    case 'LZero':
      return 1;
    case 'Zero':
      return 2;
    case 'GZero':
      return 3;
    case 'LeZero':
      return 4;
    case 'NotZero':
      return 5;
    case 'GeZero':
      return 6;
    case 'Top':
      return 7;
  }
}

const print = (a: Sign): string => {
  return a.type;
};

const eq = (a: Sign) => (b: Sign): boolean => {
  return a === b;
};

const leTable: boolean[][] = [
  //          ⊥      < 0    0      > 0    <= 0    != 0    >= 0    T
  /* ⊥ */ [true, true, true, true, true, true, true, true],
  /* < 0 */ [false, true, false, false, true, true, false, true],
  /* 0 */ [false, false, true, false, true, false, true, true],
  /* > 0 */ [false, false, false, true, false, true, true, true],
  /* <= 0 */ [false, false, false, false, true, false, false, true],
  /* != 0 */ [false, false, false, false, false, true, false, true],
  /* >= 0 */ [false, false, false, false, false, false, true, true],
  /* T */ [false, false, false, false, false, false, false, true],
];

const le = (a: Sign) => (b: Sign): boolean => {
  return leTable[index(a)][index(b)];
};

/**
[|
[   BOT;    L0;    E0;    G0;   LE0;    N0;   GE0;   TOP |];
[    L0;    L0;   LE0;    N0;   LE0;    N0;   TOP;   TOP |];
[    E0;   LE0;    E0;   GE0;   LE0;   TOP;   GE0;   TOP |];
[    G0;    N0;   GE0;    G0;   TOP;    N0;   GE0;   TOP |];
[   LE0;   LE0;   LE0;   TOP;   LE0;   TOP;   TOP;   TOP |];
[    N0;    N0;   TOP;    N0;   TOP;    N0;   TOP;   TOP |];
[   GE0;   TOP;   GE0;   GE0;   TOP;   TOP;   GE0;   TOP |];
[   TOP;   TOP;   TOP;   TOP;   TOP;   TOP;   TOP;   TOP |]
|]
 */

const joinTable: Sign[][] = [
  //          ⊥    < 0    0    > 0    <= 0    != 0    >= 0    T
  /* ⊥ */ [bottom, lZero, zero, gZero, leZero, notZero, geZero, top],
  /* < 0 */ [lZero, lZero, leZero, notZero, leZero, notZero, top, top],
  /* 0 */ [zero, leZero, zero, geZero, leZero, top, geZero, top],
  /* > 0 */ [gZero, notZero, geZero, gZero, top, notZero, geZero, top],
  /* <= 0 */ [leZero, leZero, leZero, top, leZero, top, top, top],
  /* != 0 */ [notZero, notZero, top, notZero, top, notZero, top, top],
  /* >= 0 */ [geZero, top, geZero, geZero, top, top, geZero, top],
  /* T */ [top, top, top, top, top, top, top, top],
];

const join = (a: Sign) => (b: Sign): Sign => {
  return joinTable[index(a)][index(b)];
};

const meetTable: Sign[][] = [
  //          ⊥    < 0    0    > 0    <= 0    != 0    >= 0    T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom],
  /* < 0 */ [bottom, lZero, bottom, bottom, lZero, lZero, bottom, lZero],
  /* 0 */ [bottom, bottom, zero, bottom, zero, bottom, zero, zero],
  /* > 0 */ [bottom, bottom, bottom, gZero, bottom, gZero, gZero, gZero],
  /* <= 0 */ [bottom, lZero, zero, bottom, leZero, lZero, zero, leZero],
  /* != 0 */ [bottom, lZero, bottom, gZero, lZero, notZero, gZero, notZero],
  /* >= 0 */ [bottom, bottom, zero, gZero, zero, gZero, geZero, geZero],
  /* T */ [bottom, lZero, zero, gZero, leZero, notZero, geZero, top],
];

const meet = (a: Sign) => (b: Sign): Sign => {
  return meetTable[index(a)][index(b)];
};

const addTable: Sign[][] = [
  //          ⊥    < 0    0    > 0    <= 0    != 0    >= 0    T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom],
  /* < 0 */ [bottom, lZero, lZero, top, lZero, top, top, top],
  /* 0 */ [bottom, lZero, zero, gZero, leZero, notZero, geZero, top],
  /* > 0 */ [bottom, top, gZero, gZero, top, top, gZero, top],
  /* <= 0 */ [bottom, lZero, leZero, top, leZero, top, top, top],
  /* != 0 */ [bottom, top, notZero, top, top, top, top, top],
  /* >= 0 */ [bottom, top, geZero, geZero, top, top, geZero, top],
  /* T */ [bottom, top, top, top, top, top, top, top],
];

const subTable: Sign[][] = [
  //          ⊥    < 0    0    > 0    <= 0    != 0    >= 0    T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom],
  /* < 0 */ [bottom, top, lZero, lZero, top, top, lZero, top],
  /* 0 */ [bottom, gZero, zero, lZero, leZero, notZero, geZero, top],
  /* > 0 */ [bottom, gZero, gZero, top, gZero, top, top, top],
  /* <= 0 */ [bottom, top, leZero, lZero, top, top, leZero, top],
  /* != 0 */ [bottom, top, notZero, top, top, top, top, top],
  /* >= 0 */ [bottom, gZero, geZero, top, geZero, top, top, top],
  /* T */ [bottom, top, top, top, top, top, top, top],
];

const multTable: Sign[][] = [
  //          ⊥    < 0    0    > 0    <= 0    != 0    >= 0    T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom],
  /* < 0 */ [bottom, gZero, zero, lZero, geZero, notZero, leZero, top],
  /* 0 */ [bottom, zero, zero, zero, zero, zero, zero, zero],
  /* > 0 */ [bottom, lZero, zero, gZero, leZero, notZero, geZero, top],
  /* <= 0 */ [bottom, geZero, zero, leZero, geZero, top, leZero, top],
  /* != 0 */ [bottom, notZero, zero, notZero, top, notZero, top, top],
  /* >= 0 */ [bottom, leZero, zero, geZero, leZero, top, geZero, top],
  /* T */ [bottom, top, zero, top, top, top, top, top],
];

/**
 * If the divisor can be zero bottom is returned, which could lead to false alarms
 */
const divTable: Sign[][] = [
  //          ⊥    < 0    0    > 0    <= 0    != 0    >= 0    T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom],
  /* < 0 */ [bottom, gZero, bottom, lZero, bottom, notZero, bottom, top],
  /* 0 */ [bottom, zero, bottom, zero, bottom, zero, bottom, zero],
  /* > 0 */ [bottom, lZero, bottom, gZero, bottom, notZero, bottom, top],
  /* <= 0 */ [bottom, geZero, bottom, leZero, bottom, top, bottom, top],
  /* != 0 */ [bottom, notZero, bottom, notZero, bottom, notZero, bottom, top],
  /* >= 0 */ [bottom, leZero, bottom, geZero, bottom, top, bottom, top],
  /* T */ [bottom, top, bottom, top, bottom, top, bottom, top],
];

const testLeTable: [Sign, Sign][][] = [
  //
  /* ⊥ */ [
    [bottom, bottom], // ⊥
    [bottom, bottom], // < 0
    [bottom, bottom], // 0
    [bottom, bottom], // > 0
    [bottom, bottom], // <= 0
    [bottom, bottom], // != 0
    [bottom, bottom], // >= 0
    [bottom, bottom], // T
  ],
  /* < 0 */ [
    [bottom, bottom], // ⊥
    [lZero, lZero], // < 0
    [lZero, zero], // 0
    [lZero, gZero], // > 0
    [lZero, leZero], // <= 0
    [lZero, notZero], // != 0
    [lZero, geZero], // >= 0
    [lZero, top], // T
  ],
  /* 0 */ [
    [bottom, bottom], // ⊥
    [bottom, bottom], // < 0
    [zero, zero], // 0
    [zero, gZero], // > 0
    [zero, zero], // <= 0
    [zero, gZero], // != 0
    [zero, geZero], // >= 0
    [zero, geZero], // T
  ],
  /* > 0 */ [
    [bottom, bottom], // ⊥
    [bottom, bottom], // < 0
    [bottom, bottom], // 0
    [gZero, gZero], // > 0
    [bottom, bottom], // <= 0
    [gZero, gZero], // != 0
    [gZero, gZero], // >= 0
    [gZero, gZero], // T
  ],
  /* <= 0 */ [
    [bottom, bottom], // ⊥
    [lZero, lZero], // < 0
    [leZero, zero], // 0
    [leZero, gZero], // > 0
    [leZero, leZero], // <= 0
    [leZero, notZero], // != 0
    [leZero, geZero], // >= 0
    [leZero, top], // T
  ],
  /* != 0 */ [
    [bottom, bottom], // ⊥
    [lZero, lZero], // < 0
    [lZero, zero], // 0
    [notZero, gZero], // > 0
    [lZero, leZero], // <= 0
    [notZero, notZero], // != 0
    [notZero, geZero], // >= 0
    [notZero, top], // T
  ],
  /* >= 0 */ [
    [bottom, bottom], // ⊥
    [bottom, bottom], // < 0
    [zero, zero], // 0
    [geZero, gZero], // > 0
    [zero, zero], // <= 0
    [geZero, gZero], // != 0
    [geZero, geZero], // >= 0
    [geZero, geZero], // T
  ],
  /* T */ [
    [bottom, bottom], // ⊥
    [lZero, lZero], // < 0
    [lZero, zero], // 0
    [top, gZero], // > 0
    [leZero, leZero], // <= 0
    [top, notZero], // != 0
    [top, geZero], // >= 0
    [top, top], // T
  ],
];

// Difference of information
const diffTable: Sign[][] = [
  //          ⊥    < 0    0    > 0    <= 0    != 0    >= 0    T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom],
  /* < 0 */ [bottom, bottom, lZero, lZero, bottom, bottom, lZero, bottom],
  /* 0 */ [bottom, zero, bottom, zero, bottom, zero, bottom, bottom],
  /* > 0 */ [bottom, gZero, gZero, bottom, gZero, bottom, bottom, bottom],
  /* <= 0 */ [bottom, zero, lZero, leZero, bottom, zero, lZero, bottom],
  /* != 0 */ [bottom, gZero, notZero, lZero, gZero, bottom, lZero, bottom],
  /* >= 0 */ [bottom, geZero, gZero, zero, gZero, zero, bottom, bottom],
  /* T */ [bottom, geZero, notZero, leZero, gZero, zero, lZero, bottom],
];

const evalAexpr = (expr: Aexpr) => (s: State<Sign>): Sign => {
  switch (expr.type) {
    case 'Num':
      return expr.value === 0 ? zero : expr.value > 0 ? gZero : lZero;
    case 'Var':
      return isBottomState(s) ? signDomain.bottom : s(expr.value);
    case 'Add': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return addTable[index(a1)][index(a2)];
    }
    case 'Sub': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return subTable[index(a1)][index(a2)];
    }
    case 'Mult': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return multTable[index(a1)][index(a2)];
    }
    case 'Div': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return divTable[index(a1)][index(a2)];
    }
  }
};

const test = (bexpr: Bexpr) => (s: State<Sign>): State<Sign> => {
  if (isBottomState(s)) return s;

  switch (bexpr.type) {
    case 'True':
      return s;
    case 'False':
      return bottomState;
    case 'And': {
      const test1 = test(bexpr.bexpr1)(s);
      const test2 = test(bexpr.bexpr1)(s);

      return stateOps.meet(signDomain)(test1)(test2);
    }
    case 'Or': {
      const test1 = test(bexpr.bexpr1)(s);
      const test2 = test(bexpr.bexpr1)(s);

      return stateOps.join(signDomain)(test1)(test2);
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

          // Zero is the only case where we can be sure they both have the same information
          if (a1 === a2 && a1 === zero) return bottomState;

          const met = meet(a1)(a2);

          // No information in common
          if (met === bottom) return s;

          let s1: State<Sign> = s;

          if (negBexpr.aexpr1.type === 'Var' && a1 !== met) {
            const v1 = diffTable[index(a1)][index(met)];
            s1 = substState(s1)(negBexpr.aexpr1.value)(v1);
          }
          if (negBexpr.aexpr2.type === 'Var' && a2 !== met) {
            const v2 = diffTable[index(a1)][index(met)];
            s1 = substState(s1)(negBexpr.aexpr2.value)(v2);
          }

          return s1;
        }
        case 'Le': {
          const a1 = evalAexpr(negBexpr.aexpr1)(s);
          const a2 = evalAexpr(negBexpr.aexpr2)(s);

          // ! a <= b  <=>  a > b  <=>  (b <= a) - (a = b)
          const [v1, v2] = testLeTable[index(a2)][index(a1)];
          const met = meet(v1)(v2);

          if (v1 === bottom || v2 === bottom) return bottomState;

          let s1: State<Sign> = s;

          if (negBexpr.aexpr1.type === 'Var') {
            const v1m = isBottom(met) || v1 === met ? v1 : diffTable[index(v1)][index(met)];
            const v1final = le(v1m)(a1) ? v1m : a1;
            s1 = substState(s1)(negBexpr.aexpr1.value)(v1final);
          }

          if (negBexpr.aexpr2.type === 'Var') {
            const v2m = isBottom(met) || v2 === met ? v2 : diffTable[index(v2)][index(met)];
            const v2final = le(v2m)(a2) ? v2m : a2;
            s1 = substState(s1)(negBexpr.aexpr2.value)(v2final);
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

      let s1: State<Sign> = s;

      if (bexpr.aexpr1.type === 'Var') s1 = substState(s1)(bexpr.aexpr1.value)(met);
      if (bexpr.aexpr2.type === 'Var') s1 = substState(s1)(bexpr.aexpr2.value)(met);

      return s1;
    }
    case 'Le': {
      const a1 = evalAexpr(bexpr.aexpr1)(s);
      const a2 = evalAexpr(bexpr.aexpr2)(s);

      const [v1, v2] = testLeTable[index(a1)][index(a2)];

      if (v1 === bottom || v2 === bottom) return bottomState;

      let s1: State<Sign> = s;

      if (bexpr.aexpr1.type === 'Var') s1 = substState(s1)(bexpr.aexpr1.value)(v1);
      if (bexpr.aexpr2.type === 'Var') s1 = substState(s1)(bexpr.aexpr2.value)(v2);

      return s1;
    }
  }
};

export const signDomain: Domain<Sign> = {
  eq,
  le,
  bottom,
  top,
  join,
  meet,

  evalAexpr,
  test,
  widen: join,

  print,
};
