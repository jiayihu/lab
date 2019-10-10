import { Comp, Ass, Num, Add, Var, While, True } from '../../syntax';
import { initState, isBottomState, eq, printState } from '../state';
import { signDomain, zero, geZero, leZero, top } from '../domain-sign';
import { semantic } from '../denotational-semantics';
import { hundredLoop, whileTrueSkip, whileNotZeroSkip, factorial, division } from '../../fixtures';

describe('domain sign', () => {
  it('should check if two AS are equal', () => {
    const s1 = initState(signDomain)([]);
    const s2 = initState(signDomain)([]);

    expect(eq(s1)(s2)).toBe(true);
  });

  it('should check if two AS are equal', () => {
    const s1 = initState(signDomain)([['x', zero]]);
    const s2 = initState(signDomain)([['x', zero]]);

    expect(eq(s1)(s2)).toBe(true);
  });

  it('should check if two AS are equal', () => {
    const s1 = initState(signDomain)([['x', leZero]]);
    const s2 = initState(signDomain)([['x', zero]]);

    expect(eq(s1)(s2)).toBe(false);
  });

  it('should check if two AS are equal', () => {
    const s1 = initState(signDomain)([['x', leZero]]);
    const s2 = initState(signDomain)([['x', zero], ['y', zero]]);

    expect(eq(s1)(s2)).toBe(false);
  });

  it('should return the AS of simple math programs', () => {
    const program = new Comp(
      new Ass('x', new Num(3)),
      new Comp(new Ass('y', new Num(-3)), new Ass('z', new Num(0))),
    );
    const state = initState(signDomain)([]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual(geZero);
    expect(result('y')).toEqual(leZero);
    return expect(result('z')).toEqual(zero);
  });

  it('should return the AS of sum', () => {
    const program = new Ass('x', new Add(new Var('x'), new Num(1)));
    const state = initState(signDomain)([['x', zero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual(geZero);
  });

  it('should return bottomState with while true', () => {
    const program = new While(new True(), new Ass('x', new Add(new Var('x'), new Num(1))));
    const state = initState(signDomain)([['x', zero]]);
    const result = semantic(signDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should return the AS of whileTrueSkip', () => {
    const program = whileTrueSkip;
    const state = initState(signDomain)([]);
    const result = semantic(signDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should return the AS of whileNotZeroSkip', () => {
    const program = whileNotZeroSkip;
    const state = initState(signDomain)([]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(printState(result)).toEqual([]);
  });

  it('should return the AS of factorial', () => {
    const program = factorial;
    const state = initState(signDomain)([['x', geZero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual(top);
    return expect(result('y')).toEqual(top);
  });

  it('should return the AS of division', () => {
    const program = division;
    const state = initState(signDomain)([['A', geZero], ['B', geZero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('A')).toEqual(geZero);
    expect(result('B')).toEqual(geZero);
    expect(result('R')).toEqual(top);
    return expect(result('Q')).toEqual(top);
  });

  it('should return the AS of hundredLoop', () => {
    const program = hundredLoop;
    const state = initState(signDomain)([]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('A')).toEqual(top);
    return expect(result('B')).toEqual(top);
  });
});
