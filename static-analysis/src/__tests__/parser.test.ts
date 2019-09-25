import { Add, Var, Num, Eq, Ass, Neg, Sub, Mult } from '../syntax';
import { strToChars, tokenizer, charsToStr, pAexpr, pBexpr, pProg } from '../parser';
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
  it('should parse simple Add expression', () => {
    const input = strToChars('x + 1');
    const tokens = tokenizer(input);

    expect(pAexpr.parse(tokens)).toEqual(some([new Add(new Var('x'), new Num(1)), []]));
  });

  it('should parse simple Sub expression', () => {
    const input = strToChars('x - 1');
    const tokens = tokenizer(input);

    expect(pAexpr.parse(tokens)).toEqual(some([new Sub(new Var('x'), new Num(1)), []]));
  });

  it('should give precedence to Mult operator', () => {
    const input = strToChars('2 * 3 + 4');
    const tokens = tokenizer(input);

    expect(pAexpr.parse(tokens)).toEqual(
      some([new Add(new Mult(new Num(2), new Num(3)), new Num(4)), []]),
    );
  });

  it('should associate to right', () => {
    const input = strToChars('2 * (3 + 4)');
    const tokens = tokenizer(input);

    expect(pAexpr.parse(tokens)).toEqual(
      some([new Mult(new Num(2), new Add(new Num(3), new Num(4))), []]),
    );
  });

  it('should parse basis boolean expression', () => {
    const input = strToChars('3 = 2');
    const tokens = tokenizer(input);

    expect(pBexpr.parse(tokens)).toEqual(some([new Eq(new Num(3), new Num(2)), []]));
  });

  it('should parse neg boolean expression', () => {
    const input = strToChars('¬(x = 1)');
    const tokens = tokenizer(input);

    expect(pBexpr.parse(tokens)).toEqual(some([new Neg(new Eq(new Var('x'), new Num(1))), []]));
  });

  it('should parse simple statement', () => {
    const input = strToChars('y := 1');
    const tokens = tokenizer(input);

    expect(pProg.parse(tokens)).toEqual(some([new Ass('y', new Num(1)), []]));
  });

  it('should parse factorial statement', () => {
    const input = strToChars('y := 1; while ¬(x = 1) do (y := y * x; x := x - 1)');
    const tokens = tokenizer(input);

    expect(pProg.parse(tokens)).toMatchSnapshot();
  });
});
