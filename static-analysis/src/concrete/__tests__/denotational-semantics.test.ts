import { semantic } from '../denotational-semantics';
import { factorial, whileNotZeroSkip } from './fixtures';
import { initState } from '../state';

describe('denotational semantic', () => {
  it('should return the factorial result', () => {
    const finalState = semantic(factorial)(initState({ x: 3 }));

    expect(finalState('y')).toBe(6);
    expect(finalState('x')).toBe(1);
  });

  it('should terminate whileNotZeroSkip', () => {
    const finalState = semantic(whileNotZeroSkip)(initState({ x: 0 }));

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
