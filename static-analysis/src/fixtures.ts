import { Comp, Ass, Num, While, Neg, Eq, Var, Mult, Sub, Skip, True, Le, Add, Div } from './syntax';

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

export const division = new Comp(
  new Ass('Q', new Num(0)),
  new Comp(
    new Ass('R', new Var('A')),
    new While(
      new Le(new Var('B'), new Var('R')),
      new Comp(
        new Ass('R', new Sub(new Var('R'), new Var('B'))),
        new Ass('Q', new Add(new Var('Q'), new Num(1))),
      ),
    ),
  ),
);

export const divisionByZero = new Ass('x', new Div(new Num(5), new Num(0)));

export const indirectDivByZero = new Comp(
  new Ass('x', new Num(1)),
  new Comp(
    new Ass('x', new Sub(new Var('x'), new Num(1))),
    new Ass('y', new Div(new Num(5), new Var('x'))),
  ),
);

export const whileNotZeroSkip = new While(new Neg(new Eq(new Var('x'), new Num(0))), new Skip());

export const whileTrueSkip = new While(new True(), new Skip());

export const whileTrueIncrement = new While(
  new True(),
  new Ass('x', new Add(new Var('x'), new Num(1))),
);

export const whileNotZeroIncrement = new While(
  new Neg(new Eq(new Var('x'), new Num(0))),
  new Ass('x', new Add(new Var('x'), new Num(1))),
);

export const hundredLoop = new Comp(
  new Ass('A', new Num(0)),
  new Comp(
    new Ass('B', new Num(0)),
    new While(
      new Le(new Var('A'), new Num(100)),
      new Comp(
        new Ass('A', new Add(new Var('A'), new Num(1))),
        new Ass('B', new Add(new Var('B'), new Num(1))),
      ),
    ),
  ),
);

export const fourtyLoop = new Comp(
  new Ass('x', new Num(0)),
  new While(new Le(new Var('x'), new Num(40)), new Ass('x', new Add(new Var('x'), new Num(1)))),
);

export const whileXGeZeroDecrXAndIncrY = new While(
  new Le(new Num(0), new Var('x')),
  new Comp(
    new Ass('x', new Sub(new Var('x'), new Num(1))),
    new Ass('y', new Add(new Var('y'), new Num(1))),
  ),
);
