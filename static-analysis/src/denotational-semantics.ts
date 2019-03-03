import { Stm, State, T } from './language';
import { substState, evalAexpr, evalBexpr } from './eval';
import { identity, compose } from 'fp-ts/lib/function';

/**
 * The set of Functionals is a chain complete partially ordered set
 */
type Functional = (state: State) => State;

// type Combinator = (f: Combinator) => Functional;

/**
 * The least fix point of F, which is the least upper bound of the set of iterations of F
 * @param F Continous function, which preserves least upper bound of chain.
 *
 * @notes The Haskell implementation, with lazy evalutation, is just `fix F = F (fix F)`
 */
const fix = (F: (g: () => Functional) => Functional): Functional => F(() => fix(F));

const cond = (p: (s: State) => T) => (g1: Functional) => (g2: Functional) => (state: State) => {
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
      const F = (g: () => Functional): Functional => {
        const lazyF: Functional = (s: State) =>
          compose(
            g(),
            semantic(whileStm),
          )(s);
        return cond(evalBexpr(bexpr))(lazyF)(identity);
      };

      return fix(F)(state);
    }
  }
};
