import { Comp, Ass, Num, Add, Var, Le, Neg } from '../../syntax';
import { initState, isBottomState } from '../state';
import { semantic } from '../denotational-semantics';
import { intervalDomain, top, negInf, posInf } from '../domain-interval';
import {
  whileTrueIncrement,
  divisionByZero,
  indirectDivByZero,
  whileTrueSkip,
  whileNotZeroSkip,
  factorial,
  division,
  hundredLoop,
  fourtyLoop,
  whileNotZeroIncrement,
  whileXGeZeroDecrXAndIncrY,
} from '../../fixtures';

describe('domain interval', () => {
  it('should test correctly R >= B', () => {
    const bexpr = new Le(new Var('B'), new Var('R'));
    const state = initState(intervalDomain)([
      ['Q', [0, 0]],
      ['R', [0, 150]],
      ['B', [1, 3]],
      ['A', [0, 150]],
    ]);
    const result = intervalDomain.test(bexpr)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('Q')).toEqual([0, 0]);
    expect(result('R')).toEqual([1, 150]);
    expect(result('B')).toEqual([1, 3]);
    return expect(result('A')).toEqual([0, 150]);
  });

  it('should test correctly R < B', () => {
    const bexpr = new Neg(new Le(new Var('B'), new Var('R')));
    const state = initState(intervalDomain)([
      ['Q', [0, 0]],
      ['R', [0, 150]],
      ['B', [1, 3]],
      ['A', [0, 150]],
    ]);
    const result = intervalDomain.test(bexpr)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('Q')).toEqual([0, 0]);
    expect(result('R')).toEqual([0, 2]);
    expect(result('B')).toEqual([1, 3]);
    return expect(result('A')).toEqual([0, 150]);
  });

  it('should test correctly R <= B', () => {
    const bexpr = new Le(new Var('R'), new Var('B'));
    const state = initState(intervalDomain)([
      ['Q', [0, 0]],
      ['R', [0, 150]],
      ['B', [1, 3]],
      ['A', [0, 150]],
    ]);
    const result = intervalDomain.test(bexpr)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('Q')).toEqual([0, 0]);
    expect(result('R')).toEqual([0, 3]);
    expect(result('B')).toEqual([1, 3]);
    return expect(result('A')).toEqual([0, 150]);
  });

  it('should return the AS of simple math programs', () => {
    const program = new Comp(
      new Ass('x', new Num(3)),
      new Comp(new Ass('y', new Num(-3)), new Ass('z', new Num(0))),
    );
    const state = initState(intervalDomain)([]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual([3, 3]);
    expect(result('y')).toEqual([-3, -3]);
    return expect(result('z')).toEqual([0, 0]);
  });

  it('should return the AS of sum', () => {
    const program = new Ass('x', new Add(new Var('x'), new Num(1)));
    const state = initState(intervalDomain)([['x', [1, 1]]]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual([2, 2]);
  });

  it('should return bottomState with while true', () => {
    const program = whileTrueIncrement;
    const state = initState(intervalDomain)([['x', [0, 0]]]);
    const result = semantic(intervalDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should return bottomState with division by zero', () => {
    const program = divisionByZero;
    const state = initState(intervalDomain)([]);
    const result = semantic(intervalDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should be able to return bottomState with indirect division by zero', () => {
    const program = indirectDivByZero;
    const state = initState(intervalDomain)([]);
    const result = semantic(intervalDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should return the AS of whileTrueSkip', () => {
    const program = whileTrueSkip;
    const state = initState(intervalDomain)([]);
    const result = semantic(intervalDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should return the AS of whileNotZeroSkip', () => {
    const program = whileNotZeroSkip;
    const state = initState(intervalDomain)([]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual([0, 0]);
  });

  it('should return the AS of whileNotZeroSkip', () => {
    const program = whileNotZeroSkip;
    const state = initState(intervalDomain)([['x', [0, 0]]]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual([0, 0]);
  });

  it('should return the AS of whileNotZeroSkip', () => {
    const program = whileNotZeroSkip;
    const state = initState(intervalDomain)([['x', [1, 1]]]);
    const result = semantic(intervalDomain)(program)(state);

    return expect(isBottomState(result)).toBe(true);
  });

  it('should return the AS of whileNotZeroSkip', () => {
    const program = whileNotZeroSkip;
    const state = initState(intervalDomain)([['x', [0, 2]]]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual([0, 0]);
  });

  it('should return the AS of factorial', () => {
    const program = factorial;
    const state = initState(intervalDomain)([['x', [3, 3]]]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual([1, 1]);
    return expect(result('y')).toEqual(top);
  });

  it('should return the AS of division', () => {
    const program = division;
    const state = initState(intervalDomain)([['A', [12, 12]], ['B', [3, 3]]]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('A')).toEqual([12, 12]);
    expect(result('B')).toEqual([3, 3]);
    expect(result('R')).toEqual([negInf, 2]);
    return expect(result('Q')).toEqual([0, posInf]);
  });

  it('should return the AS of hundredLoop', () => {
    const program = hundredLoop;
    const state = initState(intervalDomain)([]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('A')).toEqual([101, posInf]);
    return expect(result('B')).toEqual([0, posInf]);
  });

  it('should return the AS of fourtyLoop', () => {
    const program = fourtyLoop;
    const state = initState(intervalDomain)([]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual([41, posInf]);
  });

  it('should return the AS of whileNotZeroIncrement', () => {
    const program = whileNotZeroIncrement;
    const state = initState(intervalDomain)([['x', [-10, -5]]]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual([0, 0]);
  });

  it('should return the AS of whileNotZeroIncrement', () => {
    const program = whileNotZeroIncrement;
    const state = initState(intervalDomain)([['x', [0, 0]]]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    return expect(result('x')).toEqual([0, 0]);
  });

  it('should return the AS of whileNotZeroIncrement', () => {
    const program = whileNotZeroIncrement;
    const state = initState(intervalDomain)([['x', [1, 10]]]);
    const result = semantic(intervalDomain)(program)(state);

    return expect(isBottomState(result)).toEqual(true);
  });

  it('should return the AS of whileXNotZeroDecrXAndIncrY', () => {
    const program = whileXGeZeroDecrXAndIncrY;
    const state = initState(intervalDomain)([['x', [10, 10]], ['y', [0, 0]]]);
    const result = semantic(intervalDomain)(program)(state);

    if (isBottomState(result)) return fail('Unexpected bottom state');

    expect(result('x')).toEqual([negInf, -1]);
    return expect(result('y')).toEqual([0, posInf]);
  });
});
