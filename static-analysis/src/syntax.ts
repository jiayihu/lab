import { random } from './utils';

export type Z = number;
export type T = boolean;
export type Name = string;

export type Aexpr = Num | Var | Add | Sub | Mult | Div;

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
  constructor(readonly aexpr1: Aexpr, readonly aexpr2: Aexpr) {}
}
export class Mult {
  type: 'Mult' = 'Mult';
  constructor(readonly aexpr1: Aexpr, readonly aexpr2: Aexpr) {}
}
export class Div {
  type: 'Div' = 'Div';
  constructor(readonly aexpr1: Aexpr, readonly aexpr2: Aexpr) {}
}
export class Sub {
  type: 'Sub' = 'Sub';
  constructor(readonly aexpr1: Aexpr, readonly aexpr2: Aexpr) {}
}

export type Bexpr = True | False | Eq | Le | Neg | And;

export class True {
  type: 'True' = 'True';
  value = true;
  constructor() {}
}
export class False {
  type: 'False' = 'False';
  value = false;
  constructor() {}
}
export class Eq {
  type: 'Eq' = 'Eq';
  constructor(readonly aexpr1: Aexpr, readonly aexpr2: Aexpr) {}
}
export class Le {
  type: 'Le' = 'Le';
  constructor(readonly aexpr1: Aexpr, readonly aexpr2: Aexpr) {}
}
export class Neg {
  type: 'Neg' = 'Neg';
  constructor(readonly value: Bexpr) {}
}
export class And {
  type: 'And' = 'And';
  constructor(readonly bexpr1: Bexpr, readonly bexpr2: Bexpr) {}
}

export type Stm = Ass | Skip | Comp | If | While;

export class Ass {
  type: 'Ass' = 'Ass';
  constructor(readonly name: Name, readonly aexpr: Aexpr) {}
}
export class Skip {
  type: 'Skip' = 'Skip';
  constructor() {}
}
export class Comp {
  type: 'Comp' = 'Comp';
  constructor(readonly stm1: Stm, readonly stm2: Stm) {}
}
export class If {
  type: 'If' = 'If';
  constructor(readonly bexpr: Bexpr, readonly stm1: Stm, readonly stm2: Stm) {}
}
export class While {
  type: 'While' = 'While';
  constructor(readonly bexpr: Bexpr, readonly stm: Stm) {}
}

export type State = (name: Name) => Z;

export const initialState: (vars: Record<Name, Z>) => State = (vars: Record<Name, Z>) => name => {
  const value = vars[name];

  return value !== undefined ? value : (vars[name] = random());
};

export const twoCharOps = [':=', '!=', '<=', '>='];

export const keywords = ['if', 'then', 'else', 'while', 'do'];
