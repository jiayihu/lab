import { Name } from '../syntax';
import { Domain } from './domain';

export class BottomState {
  type: 'BottomState' = 'BottomState';
}

export const bottomState = new BottomState();

export type FState<T> = (name: Name) => T;
export type State<T> = FState<T> | BottomState;

export const isBottomState = <T>(x: State<T>): x is BottomState => {
  return typeof x !== 'function';
};

const getVarNames = <T>(...states: Array<State<T>>): Name[] => {
  const uniqueNames = new Set(states.flatMap(state => getPrivateNames(state)));

  return Array.from(uniqueNames);
};

const getPrivateNames = <T>(state: State<T>): Array<Name> => {
  if (isBottomState(state)) return [];

  return (state as any)(); // Uses hack provided by `createState`
};

// Implementation hack to return the variable names when needed
const createState = <T>(state: FState<T>, names: Name[]): FState<T> => {
  return (name: Name): T => {
    if (name === undefined) return names as any;

    return state(name);
  };
};

export const substState = <T>(state: State<T>) => (y: Name) => (value: T): State<T> => {
  if (isBottomState(state)) return state;

  const names = Array.from(new Set([...getPrivateNames(state), y]));

  return createState((name: Name) => (name === y ? value : state(name)), names);
};

export const initState = <T>(domain: Domain<T>) => (vars: Array<[Name, T]>): FState<T> => {
  const names = vars.map(([name]) => name);

  return createState((name: Name) => {
    const [, value] = vars.find(([x]) => x === name) || [];

    return value !== undefined ? value : domain.top;
  }, names);
};

export const printState = <T>(state: State<T>): string[] => {
  if (isBottomState(state)) return ['Bottom'];

  return getPrivateNames(state).map(name => {
    return `${name} := ${(state(name) as any)['type']}`;
  });
};

export const eq = <T>(x: State<T>) => (y: State<T>): boolean => {
  if (isBottomState(x) || isBottomState(y)) return x === y;

  return getVarNames(x, y).every(name => x(name) === y(name));
};

export const le = <T>(domain: Domain<T>) => (x: State<T>) => (y: State<T>): boolean => {
  if (isBottomState(x)) return true;

  return !isBottomState(y) && getVarNames(x, y).every(name => domain.le(x(name))(y(name)));
};

export const join = <T>(domain: Domain<T>) => (x: State<T>) => (y: State<T>): State<T> => {
  if (isBottomState(x)) return y;
  if (isBottomState(y)) return x;

  return createState((name: Name) => domain.join(x(name))(y(name)), getVarNames(x, y));
};

export const meet = <T>(domain: Domain<T>) => (x: State<T>) => (y: State<T>): State<T> => {
  if (isBottomState(x) || isBottomState(y)) return bottomState;

  const hasBottom = getVarNames(x, y).find(name => {
    return domain.meet(x(name))(y(name)) === domain.bottom;
  });

  if (hasBottom) return bottomState;

  return createState((name: Name) => domain.meet(x(name))(y(name)), getVarNames(x, y));
};

export const widen = <T>(domain: Domain<T>) => (x: State<T>) => (y: State<T>): State<T> => {
  if (isBottomState(x)) return y;
  if (isBottomState(y)) return x;

  return createState((name: Name) => domain.widen(x(name))(y(name)), getVarNames(x, y));
};

export const stateOps = {
  eq,
  le,
  join,
  meet,
  widen,
};
