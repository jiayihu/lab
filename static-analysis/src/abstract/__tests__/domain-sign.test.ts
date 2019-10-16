import { Comp, Ass, Num, Add, Var } from '../../syntax';
import { initState, isBottomState } from '../state';
import { signDomain, zero, geZero, top, gZero, lZero } from '../domain-sign';
import { semantic } from '../denotational-semantics';
import {
  hundredLoop,
  whileTrueSkip,
  whileNotZeroSkip,
  factorial,
  division,
  whileTrueIncrement,
  divisionByZero,
  indirectDivByZero,
  fourtyLoop,
} from '../../fixtures';

describe('domain sign', () => {
  it('should return the AS of simple math programs', () => {
    const program = new Comp(
      new Ass('x', new Num(3)),
      new Comp(new Ass('y', new Num(-3)), new Ass('z', new Num(0))),
    );
    const state = initState(signDomain)([]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual(gZero);
    expect(result('y')).toEqual(lZero);
    return expect(result('z')).toEqual(zero);
  });

  it('should return the AS of sum', () => {
    const program = new Ass('x', new Add(new Var('x'), new Num(1)));
    const state = initState(signDomain)([['x', zero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual(gZero);
  });

  it('should return bottomState with while true', () => {
    const program = whileTrueIncrement;
    const state = initState(signDomain)([['x', zero]]);
    const result = semantic(signDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should return bottomState with division by zero', () => {
    const program = divisionByZero;
    const state = initState(signDomain)([]);
    const result = semantic(signDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should not be able to return bottomState with division by zero', () => {
    const program = indirectDivByZero;
    const state = initState(signDomain)([]);
    const result = semantic(signDomain)(program)(state);

    return expect(isBottomState(result)).toBe(false);
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

    return expect(result('x')).toEqual(zero);
  });

  it('should return the AS of whileNotZeroSkip', () => {
    const program = whileNotZeroSkip;
    const state = initState(signDomain)([['x', zero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual(zero);
  });

  it('should return the AS of whileNotZeroSkip', () => {
    const program = whileNotZeroSkip;
    const state = initState(signDomain)([['x', gZero]]);
    const result = semantic(signDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should return the AS of whileNotZeroSkip', () => {
    const program = whileNotZeroSkip;
    const state = initState(signDomain)([['x', geZero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual(zero);
  });

  it('should return the AS of factorial', () => {
    const program = factorial;
    const state = initState(signDomain)([['x', gZero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual(gZero);
    return expect(result('y')).toEqual(top);
  });

  it('should return the AS of division', () => {
    const program = division;
    const state = initState(signDomain)([['A', gZero], ['B', gZero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('A')).toEqual(gZero);
    expect(result('B')).toEqual(gZero);
    expect(result('R')).toEqual(gZero);
    return expect(result('Q')).toEqual(geZero);
  });

  it('should return the AS of hundredLoop', () => {
    const program = hundredLoop;
    const state = initState(signDomain)([]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('A')).toEqual(gZero);
    return expect(result('B')).toEqual(geZero);
  });

  it('should return the AS of fourtyLoop', () => {
    const program = fourtyLoop;
    const state = initState(signDomain)([]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual(gZero);
  });
});
