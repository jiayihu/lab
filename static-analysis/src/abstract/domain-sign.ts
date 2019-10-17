import { Domain, fallbackTest } from './domain';
import { Aexpr, Bexpr } from '../syntax';
import { State, isBottomState, bottomState, substState } from './state';

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
    [leZero, zero], // 0
    [top, gZero], // > 0
    [leZero, leZero], // <= 0
    [top, notZero], // != 0
    [top, geZero], // >= 0
    [top, top], // T
  ],
];

const testGTable: [Sign, Sign][][] = [
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
    [bottom, bottom], // 0
    [bottom, bottom], // > 0
    [lZero, lZero], // <= 0
    [lZero, lZero], // != 0
    [bottom, bottom], // >= 0
    [lZero, lZero], // T
  ],
  /* 0 */ [
    [bottom, bottom], // ⊥
    [zero, lZero], // < 0
    [bottom, bottom], // 0
    [bottom, bottom], // > 0
    [zero, leZero], // <= 0
    [zero, lZero], // != 0
    [bottom, bottom], // >= 0
    [zero, lZero], // T
  ],
  /* > 0 */ [
    [bottom, bottom], // ⊥
    [gZero, lZero], // < 0
    [gZero, zero], // 0
    [gZero, gZero], // > 0
    [gZero, leZero], // <= 0
    [gZero, notZero], // != 0
    [gZero, geZero], // >= 0
    [gZero, top], // T
  ],
  /* <= 0 */ [
    [bottom, bottom], // ⊥
    [leZero, lZero], // < 0
    [bottom, bottom], // 0
    [bottom, bottom], // > 0
    [leZero, lZero], // <= 0
    [leZero, lZero], // != 0
    [bottom, bottom], // >= 0
    [leZero, lZero], // T
  ],
  /* != 0 */ [
    [bottom, bottom], // ⊥
    [notZero, lZero], // < 0
    [gZero, zero], // 0
    [gZero, gZero], // > 0
    [notZero, leZero], // <= 0
    [notZero, notZero], // != 0
    [gZero, geZero], // >= 0
    [notZero, top], // T
  ],
  /* >= 0 */ [
    [bottom, bottom], // ⊥
    [geZero, lZero], // < 0
    [geZero, zero], // 0
    [gZero, gZero], // > 0
    [geZero, leZero], // <= 0
    [geZero, notZero], // != 0
    [geZero, geZero], // >= 0
    [geZero, top], // T
  ],
  /* T */ [
    [bottom, bottom], // ⊥
    [top, lZero], // < 0
    [gZero, zero], // 0
    [gZero, gZero], // > 0
    [top, leZero], // <= 0
    [top, notZero], // != 0
    [gZero, geZero], // >= 0
    [top, top], // T
  ],
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

  const fallbackSignTest = fallbackTest(signDomain);
  const substSignState = substState(signDomain);

  switch (bexpr.type) {
    case 'True':
    case 'False':
    case 'And':
    case 'Or':
      return fallbackSignTest(bexpr)(s);
    case 'Neg': {
      const negBexpr = bexpr.value;

      switch (negBexpr.type) {
        case 'True':
        case 'False':
        case 'Neg':
        case 'And':
        case 'Or':
          return fallbackSignTest(bexpr)(s);
        case 'Eq': {
          const a1 = evalAexpr(negBexpr.aexpr1)(s);
          const a2 = evalAexpr(negBexpr.aexpr2)(s);

          // Zero is the only case where we can be sure they both are the same value
          if (a1 === a2 && a1 === zero) return bottomState;

          return s;
        }
        case 'Le': {
          const a1 = evalAexpr(negBexpr.aexpr1)(s);
          const a2 = evalAexpr(negBexpr.aexpr2)(s);

          const [v1, v2] = testGTable[index(a1)][index(a2)];

          if (v1 === bottom || v2 === bottom) return bottomState;

          let s1: State<Sign> = s;

          if (negBexpr.aexpr1.type === 'Var') s1 = substSignState(s1)(negBexpr.aexpr1.value)(v1);
          if (negBexpr.aexpr2.type === 'Var') s1 = substSignState(s1)(negBexpr.aexpr2.value)(v2);

          return s1;
        }
      }

      return s;
    }
    case 'Eq':
      return fallbackSignTest(bexpr)(s);
    case 'Le': {
      const a1 = evalAexpr(bexpr.aexpr1)(s);
      const a2 = evalAexpr(bexpr.aexpr2)(s);

      const [v1, v2] = testLeTable[index(a1)][index(a2)];

      if (v1 === bottom || v2 === bottom) return bottomState;

      let s1: State<Sign> = s;

      if (bexpr.aexpr1.type === 'Var') s1 = substSignState(s1)(bexpr.aexpr1.value)(v1);
      if (bexpr.aexpr2.type === 'Var') s1 = substSignState(s1)(bexpr.aexpr2.value)(v2);

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
