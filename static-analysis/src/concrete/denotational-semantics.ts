import { Stm, T } from '../syntax';
import { evalAexpr, evalBexpr } from './eval';
import { identity, compose } from 'fp-ts/lib/function';
import { State, substState } from './state';
import { trampoline, Thunker } from '../utils';

type FunctionalState = (state: State) => State;

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

      const KKT: Thunker<State> = (s, ret) => {
        return evalBexpr(bexpr)(s)
          ? function next() {
              return KKT(semantic(whileStm)(s), identity);
            }
          : ret(s);
      };

      return trampoline(KKT)(state);
    }
  }
};
