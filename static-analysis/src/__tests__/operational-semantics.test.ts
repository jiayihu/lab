import { deriveSeq, InterConfig, semantic } from '../operational-semantics';
import { factorial } from './fixtures';
import { initialState } from '../syntax';

describe('operational semantic', () => {
  it('should derive the configuration', () => {
    const derivation = deriveSeq(new InterConfig(factorial, initialState({ x: 3 })));

    expect(derivation).toMatchSnapshot();
  });

  it('should return the factorial result', () => {
    const finalState = semantic(factorial)(initialState({ x: 3 }));

    expect(finalState('y')).toBe(6);
    expect(finalState('x')).toBe(1);
  });
});
