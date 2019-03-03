import { semantic } from '../denotational-semantics';
import { factorial, initialState } from './fixtures';

describe('denotational semantic', () => {
  it('should return the factorial result', () => {
    const finalState = semantic(factorial)(initialState);

    expect(finalState('y')).toBe(6);
    expect(finalState('x')).toBe(1);
  });
});
