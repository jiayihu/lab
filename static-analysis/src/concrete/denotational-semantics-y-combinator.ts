import { Stm, T } from '../syntax';
import { evalAexpr, evalBexpr } from './eval';
import { identity, compose } from 'fp-ts/lib/function';
import { State, substState } from './state';

/**
 * The set of partial functions State -> State is a chain complete partially ordered set.
 * A functional takes a state (function), and performs an operation with it that
 * results in another state.
 */
type FunctionalState = (state: State) => State;

/**
 * Y combinator: the least fix point of F, which is the least upper bound of the
 * set of iterations of F.
 *
 * The Haskell implementation, with lazy evalutation, is just `fix F = F (fix F)`.
 * JS implementation:
 *
 * ```js
 * F( // Calls the recursive function generator
 *  () => fix(F) // Next iteration generator, made lazy and called only if necessary
 * )
 * ```
 *
 * @param F Continous function, which preserves least upper bound of chain.
 */
const fix = (F: (g: () => FunctionalState) => FunctionalState): FunctionalState => F(() => fix(F));

const cond = (p: (s: State) => T) => (g1: FunctionalState) => (g2: FunctionalState) => (
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

      const F = (g: () => FunctionalState): FunctionalState => {
        const lazy: FunctionalState = (s: State) =>
          compose(
            g(), // Next iteration
            semantic(whileStm), // Current iteration
          )(s);
        return cond(evalBexpr(bexpr))(lazy)(identity);
      };

      return fix(F)(state);
    }
  }
};
