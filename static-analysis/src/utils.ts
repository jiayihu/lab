export type Lit = string | number | boolean | undefined | null | void | {};

export function pair<A extends Lit, B extends Lit>([a, b]: [A, B]): [A, B] {
  return [a, b];
}
