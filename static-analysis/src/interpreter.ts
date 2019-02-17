import { Z, T, Aexpr, Bexpr, State } from './language';

type EvalAexpr = (aexpr: Aexpr) => (s: State) => Z;
type EvalBexpr = (bexpr: Bexpr) => (s: State) => T;

export const evalAexpr: EvalAexpr = (aexpr: Aexpr) => {
  return (s: State): Z => {
    switch (aexpr.type) {
      case 'Num':
        return aexpr.value;
      case 'Var':
        return s(aexpr.value);
      case 'Add':
        return evalAexpr(aexpr.a1)(s) + evalAexpr(aexpr.a2)(s);
      case 'Mult':
        return evalAexpr(aexpr.a1)(s) * evalAexpr(aexpr.a2)(s);
      case 'Sub':
        return evalAexpr(aexpr.a1)(s) - evalAexpr(aexpr.a2)(s);
    }
  };
};

export const evalBexpr: EvalBexpr = (bexpr: Bexpr) => {
  return (s: State): T => {
    switch (bexpr.type) {
      case 'True':
        return bexpr.value;
      case 'False':
        return bexpr.value;
      case 'Eq':
        return evalAexpr(bexpr.a1)(s) === evalAexpr(bexpr.a2)(s);
      case 'Le':
        return evalAexpr(bexpr.a1)(s) <= evalAexpr(bexpr.a2)(s);
      case 'Neg':
        return evalBexpr(bexpr.value)(s) === false;
      case 'And': {
        return evalBexpr(bexpr.b1)(s) === evalBexpr(bexpr.b2)(s);
      }
    }
  };
};
