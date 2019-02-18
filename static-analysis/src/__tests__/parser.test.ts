// import { Aexpr, Add, Var, Num } from "../language";
import { strToChars, tokenizer, charsToStr, pAexpr } from '../parser';

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

    console.log(pAexpr.parse(tokens));
  });
});
