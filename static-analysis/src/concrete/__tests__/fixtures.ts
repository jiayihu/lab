import { Comp, Ass, Num, While, Neg, Eq, Var, Mult, Sub, Skip, True } from '../../syntax';

export const factorial = new Comp(
  new Ass('y', new Num(1)),
  new While(
    new Neg(new Eq(new Var('x'), new Num(1))),
    new Comp(
      new Ass('y', new Mult(new Var('y'), new Var('x'))),
      new Ass('x', new Sub(new Var('x'), new Num(1))),
    ),
  ),
);

export const whileNotZeroSkip = new While(new Neg(new Eq(new Var('x'), new Num(0))), new Skip());

export const whileTrueSkip = new While(new True(), new Skip());
