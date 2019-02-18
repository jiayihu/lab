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
  switch (expr.type) {
    case 'Num':
      return [];
    case 'Var':
      return [expr.value];
    case 'Add':
    case 'Mult':
    case 'Sub': {
      const fvA1 = freeVariablesAexpr(expr.a1);
      const fvA2 = freeVariablesAexpr(expr.a2);

      return Array.from(new Set([...fvA1, ...fvA2]));
    }
  }
};

export const freeVariablesBexpr = (expr: Bexpr): Name[] => {
  switch (expr.type) {
    case 'True':
    case 'False':
      return [];
    case 'Eq':
    case 'Le': {
      const fvA1 = freeVariablesAexpr(expr.a1);
      const fvA2 = freeVariablesAexpr(expr.a2);

      return Array.from(new Set([...fvA1, ...fvA2]));
    }
    case 'Neg':
      return freeVariablesBexpr(expr.value);
    case 'And': {
      const fvB1 = freeVariablesBexpr(expr.b1);
      const fvB2 = freeVariablesBexpr(expr.b2);

      return Array.from(new Set([...fvB1, ...fvB2]));
    }
  }
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
