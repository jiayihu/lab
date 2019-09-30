import { trampoline, semantic, Thunked } from '../denotational-semantics';
import { factorial, whileNotZeroSkip, whileTrueSkip } from './fixtures';
import { initialState } from '../syntax';

describe('trampoline', () => {
  it('should return the factorial result', () => {
    const factorial: Thunked<number> = (n, ret) => {
      return n > 1
        ? function next() {
            return factorial(n - 1, res => ret(n * res));
          }
        : ret(1);
    };
    const result = trampoline(factorial)(3);

    expect(result).toEqual(6);
  });
});

describe('denotational semantic', () => {
  it('should return the factorial result', () => {
    const finalState = semantic(factorial)(initialState({ x: 3 }));

    expect(finalState('y')).toBe(6);
    expect(finalState('x')).toBe(1);
  });

  it('should terminate whileNotZeroSkip', () => {
    const finalState = semantic(whileNotZeroSkip)(initialState({ x: 0 }));

    expect(finalState('x')).toBe(0);
  });

  // it('should not terminate whileNotZeroSkip', () => {
  //   const finalState = semantic(whileNotZeroSkip)(initialState({ x: 1 }));

  //   expect(finalState('x')).toBe(0);
  // });

  // it('should not terminate whileTrueSkip', () => {
  //   const finalState = semantic(whileTrueSkip)(initialState({ x: 0 }));

  //   expect(finalState('x')).toBe(0);
  // });
});
