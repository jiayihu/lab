import { Add, Var, Num, Eq } from '../language';
import { strToChars, tokenizer, charsToStr, pAexpr, pBexpr } from '../parser';
import { some } from 'fp-ts/lib/Option';

describe('tokenizer', () => {
  it('should split in tokens', () => {
    const input = strToChars('x + 1');
    const tokens = tokenizer(input).map(charsToStr);

    expect(tokens).toEqual(['x', '+', '1']);
  });

  it('should split in tokens', () => {
    const input = strToChars('x + y * x');
    const tokens = tokenizer(input).map(charsToStr);

    expect(tokens).toEqual(['x', '+', 'y', '*', 'x']);
  });

  it('should handle two-char operators', () => {
    const input = strToChars('x <= 2');
    const tokens = tokenizer(input).map(charsToStr);

    expect(tokens).toEqual(['x', '<=', '2']);
  });
});

describe('parser', () => {
  it('should parse simple arithmetic expressions', () => {
    const tokens = ['x', '+', '1'].map(strToChars);

    expect(pAexpr.parse(tokens)).toEqual(some([new Add(new Var('x'), new Num(1)), []]));
  });

  it('should parse simple boolean expressions', () => {
    const tokens = ['3', '=', '2'].map(strToChars);

    expect(pBexpr.parse(tokens)).toEqual(some([new Eq(new Num(3), new Num(2)), []]));
  });
});
