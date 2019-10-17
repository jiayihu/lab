import { Stm, Neg } from '../syntax';
import { compose } from 'fp-ts/lib/function';
import { Domain, assign } from './domain';
import { State, stateOps, bottomState } from './state';
import { printState } from './state';

export const lim = <T>(domain: Domain<T>) => (F: (state: State<T>) => State<T>): State<T> => {
  let curr: State<T> = bottomState; // F^0 := identity(bottomState)
  let next = F(bottomState); // F^1 := compose(F, identity)(bottomState)

  while (!stateOps.eq(domain)(curr)(next)) {
    curr = next;
    next = F(next);
    // console.log('-----------------------------------------------------------');
  }

  return next;
};

export const semantic = <T>(domain: Domain<T>) => (stm: Stm) => (state: State<T>): State<T> => {
  const appliedSem = semantic(domain);
  const print = printState(domain);
  print(bottomState); // Noop, just to avoid TS unused decl error

  switch (stm.type) {
    case 'Ass': {
      return assign(domain)(stm)(state);
    }
    case 'Skip':
      return state;
    case 'Comp': {
      const { stm1, stm2 } = stm;

      return compose(
        appliedSem(stm2),
        appliedSem(stm1),
      )(state);
    }
    case 'If': {
      const { bexpr, stm1, stm2 } = stm;
      const state1 = appliedSem(stm1)(domain.test(bexpr)(state));
      const state2 = appliedSem(stm2)(domain.test(new Neg(bexpr))(state));

      return stateOps.join(domain)(state1)(state2);
    }
    case 'While': {
      const { bexpr, stm: whileStm } = stm;

      const F = (x: State<T>): State<T> => {
        // console.log('x', print(x));
        const filtered = domain.test(bexpr)(x);
        // console.log('filtered', print(filtered));
        const appliedStm = appliedSem(whileStm)(filtered);
        // console.log('appliedStm', print(appliedStm));
        const joined = stateOps.join(domain)(state)(appliedStm);
        // console.log('state', print(state));
        // console.log('joined', print(joined));

        const widened = stateOps.widen(domain)(x)(joined);
        // console.log('widened', print(widened));
        return widened;
      };

      return domain.test(new Neg(bexpr))(lim(domain)(F));
    }
  }
};
