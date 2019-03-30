export type Omit<T, K> = Pick<T, Exclude<keyof T, K>>;
export type ValueOf<T> = T[keyof T];

export function getRandomColor(): string {
  const letters = '0123456789ABCDEF';
  const hex = Array.from({ length: 6 }, (_, i) => i)
    .map(() => letters[Math.floor(Math.random() * 16)])
    .join('');

  return `#${hex}`;
}

export function isObject(value: {}): value is object {
  return typeof value === 'object' && !Array.isArray(value) && value !== null;
}

export function isValidDate(value: Date) {
  return value instanceof Date && !isNaN(value as any);
}

export const noop = () => {};

export function notEmpty<T>(value: T | null | undefined): value is T {
  return value !== null && value !== undefined;
}

export function omit<T extends Record<string, any>, K extends Array<keyof T>>(
  obj: T,
  keys: K,
): Omit<T, K> {
  return (Object.keys(obj) as Array<keyof T>)
    .filter(key => !keys.includes(key))
    .reduce(
      (acc, key) => {
        acc[key] = obj[key];

        return acc;
      },
      {} as T,
    );
}

export function series(n: number, fn = (_, i) => i) {
  Array.from({ length: n }, fn);
}

export function shuffle<T>(values: T[]): T[] {
  return values.sort(() => 0.5 - Math.random());
}

export const uniqueId = (prefix: string = ''): string => {
  return `${prefix}_${Math.random()
    .toString(36)
    .substr(2, 9)}`;
};
