import { Thunked, trampoline } from '../utils';

describe('trampoline', () => {
  it('should return the factorial result', () => {
    const factorial: Thunked<number> = (n, ret) => {
      return n > 1
        ? function next() {
            return factorial(n - 1, res => ret(n * res));
          }
        : ret(1);
    };
    const result = trampoline(factorial)(3);

    expect(result).toEqual(6);
  });
});
