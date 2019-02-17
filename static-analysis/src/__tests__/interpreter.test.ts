import { Aexpr, Var, Add, Num, Z, State, Bexpr, Neg, Eq } from '../language';
import { evalAexpr, evalBexpr } from '../interpreter';

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
