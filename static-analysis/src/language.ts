export type Z = number;
export type T = boolean;
export type Name = string;

export type Aexpr = Num | Var | Add | Mult | Sub;

export class Num {
  type: 'Num' = 'Num';
  constructor(readonly value: Z) {}
}
export class Var {
  type: 'Var' = 'Var';
  constructor(readonly value: Name) {}
}
export class Add {
  type: 'Add' = 'Add';
  constructor(readonly a1: Aexpr, readonly a2: Aexpr) {}
}
export class Mult {
  type: 'Mult' = 'Mult';
  constructor(readonly a1: Aexpr, readonly a2: Aexpr) {}
}
export class Sub {
  type: 'Sub' = 'Sub';
  constructor(readonly a1: Aexpr, readonly a2: Aexpr) {}
}

export type Bexpr = True | False | Eq | Le | Neg | And;

export class True {
  type: 'True' = 'True';
  constructor(readonly value: true) {}
}
export class False {
  type: 'False' = 'False';
  constructor(readonly value: false) {}
}
export class Eq {
  type: 'Eq' = 'Eq';
  constructor(readonly a1: Aexpr, readonly a2: Aexpr) {}
}
export class Le {
  type: 'Le' = 'Le';
  constructor(readonly a1: Aexpr, readonly a2: Aexpr) {}
}
export class Neg {
  type: 'Neg' = 'Neg';
  constructor(readonly value: Bexpr) {}
}
export class And {
  type: 'And' = 'And';
  constructor(readonly b1: Bexpr, readonly b2: Bexpr) {}
}

export type Stm = Ass | Skip | Comp | If | While;

export class Ass {
  type: 'Ass' = 'Ass';
  constructor(readonly name: Name, readonly a: Aexpr) {}
}
export class Skip {
  type: 'Skip' = 'Skip';
  constructor() {}
}
export class Comp {
  type: 'Comp' = 'Comp';
  constructor(readonly s1: Stm, readonly s2: Stm) {}
}
export class If {
  type: 'If' = 'If';
  constructor(readonly b: Bexpr, readonly s1: Stm, readonly s2: Stm) {}
}
export class While {
  type: 'While' = 'While';
  constructor(readonly b: Bexpr, readonly s: Stm) {}
}

export type State = (name: Name) => Z;
