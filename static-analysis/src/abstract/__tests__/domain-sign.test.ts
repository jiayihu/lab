import { Comp, Ass, Num, Add, Var, Neg, Le } from '../../syntax';
import { initState, isBottomState, printState, bottomState } from '../state';
import { signDomain, zero, geZero, top, gZero, lZero, notZero, leZero } from '../domain-sign';
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
  whileNotZeroIncrement,
  whileXGeZeroDecrXAndIncrY,
} from '../../fixtures';

const print = printState(signDomain);
print(bottomState);

describe('domain sign', () => {
  it('should test lZero > notZero correctly', () => {
    const bexpr = new Neg(new Le(new Var('x'), new Var('y')));
    const state = initState(signDomain)([['x', lZero], ['y', notZero]]);
    const result = signDomain.test(bexpr)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual(lZero);
    return expect(result('y')).toEqual(lZero);
  });

  it('should test leZero > geZero correctly', () => {
    const bexpr = new Neg(new Le(new Var('x'), new Var('y')));
    const state = initState(signDomain)([['x', leZero], ['y', geZero]]);
    const result = signDomain.test(bexpr)(state);

    expect(isBottomState(result)).toEqual(true);
  });

  it('should test geZero > leZero correctly', () => {
    const bexpr = new Neg(new Le(new Var('x'), new Var('y')));
    const state = initState(signDomain)([['x', geZero], ['y', leZero]]);
    const result = signDomain.test(bexpr)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual(geZero);
    return expect(result('y')).toEqual(leZero);
  });

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
    expect(result('R')).toEqual(top);
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

  it('should return the AS of whileNotZeroIncrement', () => {
    const program = whileNotZeroIncrement;
    const state = initState(signDomain)([['x', lZero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual(zero);
  });

  it('should return the AS of whileNotZeroIncrement', () => {
    const program = whileNotZeroIncrement;
    const state = initState(signDomain)([['x', zero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual(zero);
  });

  it('should return the AS of whileNotZeroIncrement', () => {
    const program = whileNotZeroIncrement;
    const state = initState(signDomain)([['x', gZero]]);
    const result = semantic(signDomain)(program)(state);

    return expect(isBottomState(result)).toEqual(true);
  });

  it('should return the AS of whileNotZeroDecrement', () => {
    const program = whileXGeZeroDecrXAndIncrY;
    const state = initState(signDomain)([['x', gZero], ['y', zero]]);
    const result = semantic(signDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual(lZero);
    return expect(result('y')).toEqual(geZero);
  });
});
