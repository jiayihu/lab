import { initState, substState, isBottomState, stateOps, bottomState, eq } from '../state';
import { signDomain, zero, geZero, top, bottom, leZero } from '../domain-sign';

describe('abstract state', () => {
  it('should return the variable in the state', () => {
    const state = initState(signDomain)([['x', geZero]]);

    expect(state('x')).toBe(geZero);
  });

  it('should return top if the variable is not defined', () => {
    const state = initState(signDomain)([]);

    expect(state('x')).toBe(top);
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

  it('should return true if the state x <= x', () => {
    const x = initState(signDomain)([['x', geZero], ['y', bottom]]);
    const y = initState(signDomain)([['x', geZero], ['y', zero]]);

    return expect(stateOps.le(signDomain)(x)(y)).toBe(true);
  });

  it('should return if the state x <= y', () => {
    const x = initState(signDomain)([['x', geZero], ['y', leZero]]);
    const y = initState(signDomain)([['x', geZero], ['y', geZero]]);

    return expect(stateOps.le(signDomain)(x)(y)).toBe(false);
  });

  it('should return if the state x <= y with missing variable', () => {
    const x = initState(signDomain)([['x', geZero], ['y', leZero]]);
    const y = initState(signDomain)([['x', geZero]]);

    return expect(stateOps.le(signDomain)(x)(y)).toBe(true);
  });

  it('should return if the bottomState <= y', () => {
    const x = bottomState;
    const y = initState(signDomain)([['x', geZero]]);

    return expect(stateOps.le(signDomain)(x)(y)).toBe(true);
  });

  it('should return if the state x <= bottomState', () => {
    const x = initState(signDomain)([['x', geZero]]);
    const y = bottomState;

    return expect(stateOps.le(signDomain)(x)(y)).toBe(false);
  });

  it('should join two states', () => {
    const x = initState(signDomain)([['x', zero], ['y', leZero], ['z', leZero]]);
    const y = initState(signDomain)([['x', geZero], ['y', geZero], ['z', zero], ['w', zero]]);
    const joined = stateOps.join(signDomain)(x)(y);

    if (isBottomState(joined)) return fail('Unexpected bottom state');

    expect(joined('x')).toBe(geZero);
    expect(joined('y')).toBe(top);
    expect(joined('z')).toBe(leZero);
    expect(joined('w')).toBe(top);
    return;
  });

  it('should join two states as y if x is bottomState', () => {
    const x = bottomState;
    const y = initState(signDomain)([['x', geZero]]);
    const joined = stateOps.join(signDomain)(x)(y);

    if (isBottomState(joined)) return fail('Unexpected bottom state');

    return expect(joined('x')).toBe(geZero);
  });

  it('should join two states as x if y is bottomState', () => {
    const x = initState(signDomain)([['x', geZero]]);
    const y = bottomState;
    const joined = stateOps.join(signDomain)(x)(y);

    if (isBottomState(joined)) return fail('Unexpected bottom state');

    return expect(joined('x')).toBe(geZero);
  });

  it('should meet two states', () => {
    const x = initState(signDomain)([['x', zero], ['y', leZero], ['z', leZero]]);
    const y = initState(signDomain)([['x', geZero], ['y', geZero], ['z', zero], ['w', zero]]);
    const met = stateOps.meet(signDomain)(x)(y);

    if (isBottomState(met)) return fail('Unexpected bottom state');

    expect(met('x')).toBe(zero);
    expect(met('y')).toBe(zero);
    expect(met('z')).toBe(zero);
    expect(met('w')).toBe(zero);
    return;
  });

  it('should meet two states as bottomState if x is bottomState', () => {
    const x = bottomState;
    const y = initState(signDomain)([['x', geZero]]);
    const met = stateOps.meet(signDomain)(x)(y);

    expect(isBottomState(met)).toBe(true);
  });

  it('should meet two states as bottomState if y is bottomState', () => {
    const x = initState(signDomain)([['x', geZero]]);
    const y = bottomState;
    const met = stateOps.meet(signDomain)(x)(y);

    expect(isBottomState(met)).toBe(true);
  });

  it('should meet two states as bottomState if any meet between variables is bottom', () => {
    const x = initState(signDomain)([['x', zero], ['y', leZero], ['z', leZero]]);
    const y = initState(signDomain)([['x', geZero], ['y', geZero], ['z', bottom], ['w', zero]]);
    const met = stateOps.meet(signDomain)(x)(y);

    expect(isBottomState(met)).toBe(true);
  });

  it('should widen two states', () => {
    const x = initState(signDomain)([['x', zero], ['y', leZero], ['z', leZero]]);
    const y = initState(signDomain)([['x', geZero], ['y', geZero], ['z', zero], ['w', zero]]);
    const met = stateOps.widen(signDomain)(x)(y);

    if (isBottomState(met)) return fail('Unexpected bottom state');

    expect(met('x')).toBe(geZero);
    expect(met('y')).toBe(top);
    expect(met('z')).toBe(leZero);
    expect(met('w')).toBe(top);
    return;
  });

  it('should widen two states as y if x is bottomState', () => {
    const x = bottomState;
    const y = initState(signDomain)([['x', geZero]]);
    const met = stateOps.widen(signDomain)(x)(y);

    expect(met).toEqual(y);
  });

  it('should widen two states as x if y is bottomState', () => {
    const x = initState(signDomain)([['x', geZero]]);
    const y = bottomState;
    const met = stateOps.widen(signDomain)(x)(y);

    expect(met).toEqual(x);
  });

  it('should check if two AS are equal', () => {
    const s1 = initState(signDomain)([]);
    const s2 = initState(signDomain)([]);

    expect(eq(signDomain)(s1)(s2)).toBe(true);
  });

  it('should check if two AS are equal', () => {
    const s1 = initState(signDomain)([['x', zero]]);
    const s2 = initState(signDomain)([['x', zero]]);

    expect(eq(signDomain)(s1)(s2)).toBe(true);
  });

  it('should check if two AS are equal', () => {
    const s1 = initState(signDomain)([['x', leZero]]);
    const s2 = initState(signDomain)([['x', zero]]);

    expect(eq(signDomain)(s1)(s2)).toBe(false);
  });

  it('should check if two AS are equal', () => {
    const s1 = initState(signDomain)([['x', leZero]]);
    const s2 = initState(signDomain)([['x', zero], ['y', zero]]);

    expect(eq(signDomain)(s1)(s2)).toBe(false);
  });
});
