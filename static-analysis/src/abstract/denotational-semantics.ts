import { Stm, Neg } from '../syntax';
import { compose, identity } from 'fp-ts/lib/function';
import { Domain, assign } from './domain';
import { State, stateOps, bottomState } from './state';
// import { printState } from './state';

export const lim = <T>(F: (state: State<T>) => State<T>): State<T> => {
  let curr: (state: State<T>) => State<T> = identity;
  let next = compose(
    F,
    curr,
  );

  while (!stateOps.eq(curr(bottomState))(next(bottomState))) {
    curr = next;
    next = compose(
      F,
      curr,
    );
    // console.log('-----------------------------------------------------------');
  }

  return next(bottomState);
};

export const semantic = <T>(domain: Domain<T>) => (stm: Stm) => (state: State<T>): State<T> => {
  switch (stm.type) {
    case 'Ass': {
      return assign(domain)(stm)(state);
    }
    case 'Skip':
      return state;
    case 'Comp': {
      const { stm1, stm2 } = stm;

      return compose(
        semantic(domain)(stm2),
        semantic(domain)(stm1),
      )(state);
    }
    case 'If': {
      const { bexpr, stm1, stm2 } = stm;
      const state1 = semantic(domain)(stm1)(domain.test(bexpr)(state));
      const state2 = semantic(domain)(stm2)(domain.test(new Neg(bexpr))(state));

      return stateOps.join(domain)(state1)(state2);
    }
    case 'While': {
      const { bexpr, stm: whileStm } = stm;

      const F = (x: State<T>): State<T> => {
        // console.log('x', printState(x));
        const sx = semantic(domain)(whileStm)(domain.test(bexpr)(x));
        // console.log('sx', printState(sx));
        const y = stateOps.join(domain)(state)(sx);
        // console.log('state', printState(state));
        // console.log('y', printState(y));

        const widened = stateOps.widen(domain)(x)(y);
        // console.log('widened', printState(widened));

        // console.log('------------------');
        return widened;
      };

      return domain.test(new Neg(bexpr))(lim(F));
    }
  }
};
