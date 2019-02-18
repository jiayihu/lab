import { Z, T, Name, Aexpr, Bexpr, State, Add, Mult, Sub, Eq, Neg, And } from './language';

type EvalAexpr = (aexpr: Aexpr) => (s: State) => Z;
type EvalBexpr = (bexpr: Bexpr) => (s: State) => T;

export const evalAexpr: EvalAexpr = (expr: Aexpr) => (s: State): Z => {
  switch (expr.type) {
    case 'Num':
      return expr.value;
    case 'Var':
      return s(expr.value);
    case 'Add':
      return evalAexpr(expr.a1)(s) + evalAexpr(expr.a2)(s);
    case 'Mult':
      return evalAexpr(expr.a1)(s) * evalAexpr(expr.a2)(s);
    case 'Sub':
      return evalAexpr(expr.a1)(s) - evalAexpr(expr.a2)(s);
  }
};

export const evalBexpr: EvalBexpr = (expr: Bexpr) => (s: State): T => {
  switch (expr.type) {
    case 'True':
      return expr.value;
    case 'False':
      return expr.value;
    case 'Eq':
      return evalAexpr(expr.a1)(s) === evalAexpr(expr.a2)(s);
    case 'Le':
      return evalAexpr(expr.a1)(s) <= evalAexpr(expr.a2)(s);
    case 'Neg':
      return evalBexpr(expr.value)(s) === false;
    case 'And': {
      return evalBexpr(expr.b1)(s) === evalBexpr(expr.b2)(s);
    }
  }
};

export const freeVariablesAexpr = (expr: Aexpr): Name[] => {
  const rec = (aexpr: Aexpr, fvs: Set<Name>): Set<Name> => {
    switch (aexpr.type) {
      case 'Num':
        return fvs;
      case 'Var':
        return new Set(fvs).add(aexpr.value);
      case 'Add':
      case 'Mult':
      case 'Sub': {
        const fvA1 = rec(aexpr.a1, fvs);
        const fvA2 = rec(aexpr.a2, fvs);

        return new Set([...fvA1, ...fvA2]);
      }
    }
  };

  return Array.from(rec(expr, new Set()));
};

export const freeVariablesBexpr = (expr: Bexpr): Name[] => {
  const rec = (bexpr: Bexpr, fvs: Set<Name>): Set<Name> => {
    switch (bexpr.type) {
      case 'True':
      case 'False':
        return fvs;
      case 'Eq':
      case 'Le': {
        const fvA1 = freeVariablesAexpr(bexpr.a1);
        const fvA2 = freeVariablesAexpr(bexpr.a2);

        return new Set([...fvA1, ...fvA2]);
      }
      case 'Neg':
        return rec(bexpr.value, fvs);
      case 'And': {
        const fvB1 = rec(bexpr.b1, fvs);
        const fvB2 = rec(bexpr.b2, fvs);

        return new Set([...fvB1, ...fvB2]);
      }
    }
  };

  return Array.from(rec(expr, new Set()));
};

export const substAexpr = (a: Aexpr) => (y: Name) => (a0: Aexpr): Aexpr => {
  switch (a.type) {
    case 'Num':
      return a;
    case 'Var':
      return a.value === y ? a0 : a;
    case 'Add':
      return new Add(substAexpr(a.a1)(y)(a0), substAexpr(a.a2)(y)(a0));
    case 'Mult':
      return new Mult(substAexpr(a.a1)(y)(a0), substAexpr(a.a2)(y)(a0));
    case 'Sub':
      return new Sub(substAexpr(a.a1)(y)(a0), substAexpr(a.a2)(y)(a0));
  }
};

export const substBexpr = (b: Bexpr) => (y: Name) => (a0: Aexpr): Bexpr => {
  switch (b.type) {
    case 'True':
    case 'False':
      return b;
    case 'Eq':
    case 'Le':
      return new Eq(substAexpr(b.a1)(y)(a0), substAexpr(b.a2)(y)(a0));
    case 'Neg':
      return new Neg(substBexpr(b.value)(y)(a0));
    case 'And':
      return new And(substBexpr(b.b1)(y)(a0), substBexpr(b.b2)(y)(a0));
  }
};

export const substState = (s: State) => (y: Name) => (v: Z): State => {
  return (x: Name) => (x === y ? v : s(x));
};
