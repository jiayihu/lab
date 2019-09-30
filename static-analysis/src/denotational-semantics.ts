import { Stm, State, T } from './syntax';
import { substState, evalAexpr, evalBexpr } from './eval';
import { identity, compose } from 'fp-ts/lib/function';

/**
 * The set of partial functions State -> State is a chain complete partially ordered set.
 * A functional takes a state (function), and performs an operation with it that
 * results in another state.
 */
type FunctionalStm = (state: State) => State;

/**
 * A thunked function returns an expression wrapped in an argument-less function.
 * This wrapping delays the evaluation of the expression until the point at which
 * the function is called.
 */
export type Thunked<T> = (s: T, ret: (s: T) => T) => T | (() => ReturnType<Thunked<T>>);

function isNextFn<T>(fn: T | (() => T)): fn is () => T {
  return typeof fn === 'function' && fn.name === 'next';
}

/**
 * A trampoline is a loop that iteratively invokes thunk-returning functions.
 * The idea is to not make the final continuation call inside the function, but
 * to exit and to return the continuation to a trampoline. That trampoline is
 * simply a loop that invokes the returned continuations. Hence, there are no
 * nested function calls and the stack won’t grow.
 * @src https://eli.thegreenplace.net/2017/on-recursion-continuations-and-trampolines/
 * @src https://en.wikipedia.org/wiki/Trampoline_(computing)
 */
export function trampoline<T>(thunk: Thunked<T>): (s: T) => T {
  return function(s: T): T {
    let result = thunk(s, identity);

    while (result && isNextFn(result)) {
      result = result();
    }

    return result;
  };
}

const cond = (p: (s: State) => T) => (g1: FunctionalStm) => (g2: FunctionalStm) => (
  state: State,
) => {
  return p(state) ? g1(state) : g2(state);
};

export const semantic = (stm: Stm) => (state: State): State => {
  switch (stm.type) {
    case 'Ass': {
      const { name, aexpr } = stm;
      return substState(state)(name)(evalAexpr(aexpr)(state));
    }
    case 'Skip':
      return state;
    case 'Comp': {
      const { stm1, stm2 } = stm;
      return compose(
        semantic(stm2),
        semantic(stm1),
      )(state);
    }
    case 'If': {
      const { bexpr, stm1, stm2 } = stm;
      return cond(evalBexpr(bexpr))(semantic(stm1))(semantic(stm2))(state);
    }
    case 'While': {
      const { bexpr, stm: whileStm } = stm;

      const KKT: Thunked<State> = (s, ret) => {
        return evalBexpr(bexpr)(s)
          ? function next() {
              const s1 = semantic(whileStm)(s);
              return KKT(s1, identity);
            }
          : ret(s);
      };

      return trampoline(KKT)(state);
    }
  }
};
