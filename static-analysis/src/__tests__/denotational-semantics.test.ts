import { semantic } from '../denotational-semantics';
import { factorial } from './fixtures';
import { initialState } from '../syntax';

describe('denotational semantic', () => {
  it('should return the factorial result', () => {
    const finalState = semantic(factorial)(initialState({ x: 3 }));

    expect(finalState('y')).toBe(6);
    expect(finalState('x')).toBe(1);
  });
});
