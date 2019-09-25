import { Aexpr, Var, Add, Num, Z, State, Bexpr, Neg, Eq, Mult } from '../syntax';
import { evalAexpr, evalBexpr, freeVariablesAexpr, substAexpr } from '../eval';

describe('evalAexpr', () => {
  it('should return the correct evaluation', () => {
    const aexpr: Aexpr = new Add(new Var('x'), new Num(1));
    const state: State = name => {
      const vars: Record<string, Z> = { x: 3 };
      return vars[name];
    };

    expect(evalAexpr(aexpr)(state)).toBe(4);
  });
});

describe('evalBexpr', () => {
  it('should return the correct evaluation', () => {
    const bexpr: Bexpr = new Neg(new Eq(new Var('x'), new Num(1)));
    const state: State = name => {
      const vars: Record<string, Z> = { x: 3 };
      return vars[name];
    };

    expect(evalBexpr(bexpr)(state)).toBe(true);
  });
});

describe('freeVariablesAexpr', () => {
  it('should return the free variable in the expression', () => {
    const aexpr: Aexpr = new Add(new Var('x'), new Num(1));

    expect(freeVariablesAexpr(aexpr)).toEqual(['x']);
  });

  it('should avoid duplicate variables', () => {
    const aexpr: Aexpr = new Add(new Var('x'), new Mult(new Var('y'), new Var('x')));

    expect(freeVariablesAexpr(aexpr)).toEqual(['x', 'y']);
  });
});

describe('substAexpr', () => {
  it('should replace with a new expression', () => {
    const a: Aexpr = new Add(new Var('x'), new Num(1));
    const a0 = new Num(3);
    const expected = new Add(new Num(3), new Num(1));

    expect(substAexpr(a)('x')(a0)).toEqual(expected);
  });
});
