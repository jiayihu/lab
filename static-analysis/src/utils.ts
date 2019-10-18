export type Brand<K, T> = K & { _brand: T };

import { identity } from 'fp-ts/lib/function';

export type Lit = string | number | boolean | undefined | null | void | {};

export function pair<A extends Lit, B extends Lit>([a, b]: [A, B]): [A, B] {
  return [a, b];
}

export function random() {
  const sign = Math.random() < 0.5 ? -1 : 1;

  return Math.round(Math.random() * Number.MAX_SAFE_INTEGER * sign);
}

/**
 * A thunked function returns an expression wrapped in an argument-less function.
 * This wrapping delays the evaluation of the expression until the point at which
 * the function is called.
 */
export type Thunker<T> = (s: T, ret: (s: T) => T) => T | (() => ReturnType<Thunker<T>>);

function isNextFn<T>(fn: T | (() => T)): fn is () => T {
  return typeof fn === 'function' && fn.name === 'next';
}

/**
 * A trampoline is a loop that iteratively invokes thunk-returning functions.
 * The idea is to not make the final continuation call inside the function, but
 * to exit and to return the continuation to a trampoline. That trampoline is
 * simply a loop that invokes the returned continuations. Hence, there are no
 * nested function calls and the stack won’t grow.
 * @src https://eli.thegreenplace.net/2017/on-recursion-continuations-and-trampolines/
 * @src https://en.wikipedia.org/wiki/Trampoline_(computing)
 */
export function trampoline<T>(thunker: Thunker<T>): (s: T) => T {
  return function(s: T): T {
    let result = thunker(s, identity);

    while (result && isNextFn(result)) result = result();

    return result;
  };
}
