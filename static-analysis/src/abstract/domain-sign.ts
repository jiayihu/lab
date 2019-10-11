import { Domain, fallbackTest } from './domain';
import { Aexpr } from '../syntax';
import { State, isBottomState } from './state';

class Bottom {
  type: 'Bottom' = 'Bottom';
}

class Zero {
  type: 'Zero' = 'Zero';
}

class LeZero {
  type: 'LeZero' = 'LeZero';
}

class GeZero {
  type: 'GeZero' = 'GeZero';
}

class Top {
  type: 'Top' = 'Top';
}

export const bottom = new Bottom();
export const zero = new Zero();
export const leZero = new LeZero();
export const geZero = new GeZero();
export const top = new Top();

type Sign = Bottom | Zero | LeZero | GeZero | Top;

function tableIndex(a: Sign): number {
  switch (a.type) {
    case 'Bottom':
      return 0;
    case 'Zero':
      return 1;
    case 'LeZero':
      return 2;
    case 'GeZero':
      return 3;
    case 'Top':
      return 4;
  }
}

const leTable: boolean[][] = [
  //          ⊥      0      <= 0   >= 0   T
  /* ⊥ */ [true, true, true, true, true],
  /* 0 */ [false, true, true, true, true],
  /* <= 0 */ [false, false, true, false, true],
  /* >= 0 */ [false, false, false, true, true],
  /* T */ [false, false, false, false, true],
];

const le = (a: Sign) => (b: Sign): boolean => {
  return leTable[tableIndex(a)][tableIndex(b)];
};

const joinTable: Sign[][] = [
  //          ⊥      0      <= 0   >= 0   T
  /* ⊥ */ [bottom, zero, leZero, geZero, top],
  /* 0 */ [zero, zero, leZero, geZero, top],
  /* <= 0 */ [leZero, leZero, leZero, top, top],
  /* >= 0 */ [geZero, geZero, top, geZero, top],
  /* T */ [top, top, top, top, top],
];

const join = (a: Sign) => (b: Sign): Sign => {
  return joinTable[tableIndex(a)][tableIndex(b)];
};

const meetTable: Sign[][] = [
  //          ⊥      0      <= 0   >= 0   T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom],
  /* 0 */ [bottom, zero, zero, zero, zero],
  /* <= 0 */ [bottom, zero, leZero, zero, leZero],
  /* >= 0 */ [bottom, zero, zero, geZero, geZero],
  /* T */ [bottom, zero, leZero, geZero, top],
];

const meet = (a: Sign) => (b: Sign): Sign => {
  return meetTable[tableIndex(a)][tableIndex(b)];
};

const addTable: Sign[][] = [
  //          ⊥      0      <= 0   >= 0   T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom],
  /* 0 */ [bottom, zero, leZero, geZero, top],
  /* <= 0 */ [bottom, leZero, leZero, top, top],
  /* >= 0 */ [bottom, geZero, top, geZero, top],
  /* T */ [bottom, top, top, top, top],
];

const subTable: Sign[][] = [
  //          ⊥      0      <= 0   >= 0   T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom],
  /* 0 */ [bottom, zero, geZero, leZero, top],
  /* <= 0 */ [bottom, leZero, top, leZero, top],
  /* >= 0 */ [bottom, geZero, geZero, top, top],
  /* T */ [bottom, top, top, top, top],
];

const multTable: Sign[][] = [
  //          ⊥      0      <= 0   >= 0   T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom],
  /* 0 */ [bottom, zero, zero, zero, top],
  /* <= 0 */ [bottom, zero, geZero, leZero, top],
  /* >= 0 */ [bottom, zero, leZero, geZero, top],
  /* T */ [bottom, top, top, top, top],
];

const divTable: Sign[][] = [
  //          ⊥      0      <= 0   >= 0   T
  /* ⊥ */ [bottom, bottom, bottom, bottom, bottom],
  /* 0 */ [bottom, bottom, zero, zero, top],
  /* <= 0 */ [bottom, bottom, geZero, leZero, top],
  /* >= 0 */ [bottom, bottom, leZero, geZero, top],
  /* T */ [bottom, bottom, top, top, top],
];

const evalAexpr = (expr: Aexpr) => (s: State<Sign>): Sign => {
  switch (expr.type) {
    case 'Num':
      return expr.value === 0 ? zero : expr.value >= 0 ? geZero : leZero;
    case 'Var':
      return isBottomState(s) ? signDomain.bottom : s(expr.value);
    case 'Add': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return addTable[tableIndex(a1)][tableIndex(a2)];
    }
    case 'Sub': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return subTable[tableIndex(a1)][tableIndex(a2)];
    }
    case 'Mult': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return multTable[tableIndex(a1)][tableIndex(a2)];
    }
    case 'Div': {
      const a1 = evalAexpr(expr.aexpr1)(s);
      const a2 = evalAexpr(expr.aexpr2)(s);

      return divTable[tableIndex(a1)][tableIndex(a2)];
    }
  }
};

export const signDomain: Domain<Sign> = {
  le,
  bottom,
  top,
  join,
  meet,

  evalAexpr,
  get test() {
    return fallbackTest(signDomain);
  },
  widen: join,
};
