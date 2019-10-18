import { semantic } from '../denotational-semantics';
import { factorial, whileNotZeroSkip, hundredLoop, division } from '../../fixtures';
import { initState } from '../state';
// import { parse } from '../../parser';
// import { Skip } from '../../syntax';

describe('denotational semantic', () => {
  it('should return the factorial result', () => {
    const finalState = semantic(factorial)(initState({ x: 3 }));

    expect(finalState('y')).toBe(6);
    expect(finalState('x')).toBe(1);
  });

  it('should terminate whileNotZeroSkip', () => {
    const finalState = semantic(whileNotZeroSkip)(initState({ x: 0 }));

    expect(finalState('x')).toBe(0);
  });

  it('should return the result of division', () => {
    const finalState = semantic(division)(initState({ A: 11, B: 3 }));

    expect(finalState('R')).toBe(2);
    expect(finalState('Q')).toBe(3);
  });

  it('should return the result of hundredLoop', () => {
    const finalState = semantic(hundredLoop)(initState({}));

    expect(finalState('A')).toBe(101);
    expect(finalState('B')).toBe(101);
  });

  // it('should not terminate whileNotZeroSkip', () => {
  //   const finalState = semantic(whileNotZeroSkip)(initState({ x: 1 }));

  //   expect(finalState('x')).toBe(0);
  // });

  // it('should not terminate whileTrueSkip', () => {
  //   const finalState = semantic(whileTrueSkip)(initState({ x: 0 }));

  //   expect(finalState('x')).toBe(0);
  // });

  // it('should not terminate with for to x + 1', () => {
  //   const input = 'for x := 1 to x + 1 do skip';
  //   const [stm] = parse(input).getOrElse([new Skip(), []]);
  //   const finalState = semantic(stm)(initState({}));

  //   expect(finalState('x')).toBe(0);
  // });
});
