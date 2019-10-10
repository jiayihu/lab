import { initState, substState, isBottomState } from '../state';
import { signDomain, zero, geZero } from '../domain-sign';

describe('abstract state', () => {
  it('should return the variable in the state', () => {
    const state = initState(signDomain)([['x', geZero]]);

    expect(state('x')).toBe(geZero);
  });

  it('should change the variable in the state', () => {
    const state = initState(signDomain)([['x', geZero]]);
    const updatedState = substState(state)('x')(zero);

    if (isBottomState(updatedState)) return fail('Unexpected bottom state');

    return expect(updatedState('x')).toBe(zero);
  });

  it('should add a new variable in the state', () => {
    const state = initState(signDomain)([['x', geZero]]);
    const updatedState = substState(state)('y')(zero);

    if (isBottomState(updatedState)) return fail('Unexpected bottom state');

    return expect(updatedState('y')).toBe(zero);
  });
});
