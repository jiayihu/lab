export type Lit = string | number | boolean | undefined | null | void | {};

export function pair<A extends Lit, B extends Lit>([a, b]: [A, B]): [A, B] {
  return [a, b];
}

export function random() {
  const sign = Math.random() < 0.5 ? -1 : 1;

  return Math.round(Math.random() * Number.MAX_SAFE_INTEGER * sign);
}
