import { deriveSeq, InterConfig, semantic } from '../semantics';
import { factorial, initialState } from './fixtures';

describe('deriveSeq', () => {
  it('should derive the configuration', () => {
    const derivation = deriveSeq(new InterConfig(factorial, initialState));

    expect(derivation).toMatchSnapshot();
  });

  it('should return the factorial result', () => {
    const finalState = semantic(factorial, initialState);

    expect(finalState('y')).toBe(6);
    expect(finalState('x')).toBe(1);
  });
});
