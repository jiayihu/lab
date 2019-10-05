import {
  Add,
  Var,
  Num,
  Eq,
  Ass,
  Neg,
  Sub,
  Mult,
  And,
  Div,
  Le,
  True,
  False,
  Skip,
  If,
  While,
  initialState,
  Comp,
} from '../syntax';
import { strToChars, tokenizer, charsToStr, pAexpr, pBexpr, pProg } from '../parser';
import { some, option } from 'fp-ts/lib/Option';
import { array } from 'fp-ts/lib/Array';
import { evalBexpr, evalAexpr } from '../eval';
import { semantic } from '../operational-semantics';

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

  it('should give precedence to Mult operator', () => {
    const input = strToChars('2 + 3 * 4');
    const tokens = tokenizer(input);

    expect(pAexpr.parse(tokens)).toEqual(
      some([new Add(new Num(2), new Mult(new Num(3), new Num(4))), []]),
    );
  });

  it('should give precedence to parenthesis', () => {
    const input = strToChars('2 * (3 + 4)');
    const tokens = tokenizer(input);

    expect(pAexpr.parse(tokens)).toEqual(
      some([new Mult(new Num(2), new Add(new Num(3), new Num(4))), []]),
    );
  });

  it('should associate Aexpr to right', () => {
    const input = strToChars('2 + 3 + 4');
    const tokens = tokenizer(input);

    expect(pAexpr.parse(tokens)).toEqual(
      some([new Add(new Num(2), new Add(new Num(3), new Num(4))), []]),
    );
  });

  it('should parse the battery of Aexpr tests', () => {
    const input = ['x+2*y', 'x*(2*y)', '(x*2)*y', '(x/3)*2', 'x/3*2'];
    const tokens = input.map(aexpr => tokenizer(strToChars(aexpr)));
    const syntaxes = tokens.map(ts => pAexpr.parse(ts));
    const result = array.sequence(option)(syntaxes);

    expect(result).toEqual(
      some([
        [new Add(new Var('x'), new Mult(new Num(2), new Var('y'))), []],
        [new Mult(new Var('x'), new Mult(new Num(2), new Var('y'))), []],
        [new Mult(new Mult(new Var('x'), new Num(2)), new Var('y')), []],
        [new Mult(new Div(new Var('x'), new Num(3)), new Num(2)), []],
        [new Div(new Var('x'), new Mult(new Num(3), new Num(2))), []],
      ]),
    );
  });

  it('should handle syntactic sugar', () => {
    const input = ['-1 + 2', '-(1 + 2)', '3 - 1'];
    const tokens = input.map(bexpr => tokenizer(strToChars(bexpr)));
    const syntaxes = tokens
      .map(ts => pAexpr.parse(ts))
      .map(syntax => syntax.map(([aexpr]) => evalAexpr(aexpr)(() => 0)));
    const result = array.sequence(option)(syntaxes);
    const expected = some([1, -3, 2]);

    expect(result).toEqual(expected);
  });

  it('should parse basis boolean expression', () => {
    const input = strToChars('3 = 2');
    const tokens = tokenizer(input);

    expect(pBexpr.parse(tokens)).toEqual(some([new Eq(new Num(3), new Num(2)), []]));
  });

  it('should parse neg boolean expression', () => {
    const input = strToChars('!(x = 1)');
    const tokens = tokenizer(input);

    expect(pBexpr.parse(tokens)).toEqual(some([new Neg(new Eq(new Var('x'), new Num(1))), []]));
  });

  it('should give precedence to Neg operator', () => {
    const input = strToChars('!(x = 1) & (x = 2)');
    const tokens = tokenizer(input);

    expect(pBexpr.parse(tokens)).toEqual(
      some([
        new And(new Neg(new Eq(new Var('x'), new Num(1))), new Eq(new Var('x'), new Num(2))),
        [],
      ]),
    );
  });

  it('should associate Bexpr to right', () => {
    const input = strToChars('x = 3 & x = 4 & x = 5');
    const tokens = tokenizer(input);

    expect(pBexpr.parse(tokens)).toEqual(
      some([
        new And(
          new Eq(new Var('x'), new Num(3)),
          new And(new Eq(new Var('x'), new Num(4)), new Eq(new Var('x'), new Num(5))),
        ),
        [],
      ]),
    );
  });

  it('should parse the battery of Bexpr tests', () => {
    const input = [
      '(4+(3)) <= 5',
      '!true & (8 <= 2)',
      '!true & false',
      '!(true & false)',
      '!((4 = 8) & false)',
    ];
    const tokens = input.map(aexpr => tokenizer(strToChars(aexpr)));
    const syntaxes = tokens.map(ts => pBexpr.parse(ts));
    const result = array.sequence(option)(syntaxes);

    expect(result).toEqual(
      some([
        [new Le(new Add(new Num(4), new Num(3)), new Num(5)), []],
        [new And(new Neg(new True()), new Le(new Num(8), new Num(2))), []],
        [new And(new Neg(new True()), new False()), []],
        [new Neg(new And(new True(), new False())), []],
        [new Neg(new And(new Eq(new Num(4), new Num(8)), new False())), []],
      ]),
    );
  });

  it('should handle syntactic sugar', () => {
    const input = [
      '5 != 5',
      '5 != 4',
      '5 < 4',
      '5 < 6',
      '5 < 5',
      '6 > 7',
      '6 > 5',
      '6 > 6',
      '6 >= 5',
      '6 >= 6',
      '6 >= 7',
      '5 <= 3 | 5 <= 3',
      '5 <= 3 | 3 <= 5',
      '3 <= 5 | 3 <= 5',
    ];
    const tokens = input.map(bexpr => tokenizer(strToChars(bexpr)));
    const syntaxes = tokens
      .map(ts => pBexpr.parse(ts))
      .map(syntax => syntax.map(([bexpr]) => evalBexpr(bexpr)(() => 0)));
    const result = array.sequence(option)(syntaxes);
    const expected = some([
      false,
      true,
      false,
      true,
      false,
      false,
      true,
      false,
      true,
      true,
      false,
      false,
      true,
      true,
    ]);

    expect(result).toEqual(expected);
  });

  it('should parse simple statement', () => {
    const input = strToChars('y := 1');
    const tokens = tokenizer(input);

    expect(pProg.parse(tokens)).toEqual(some([new Ass('y', new Num(1)), []]));
  });

  it('should parse the battery of Stm tests', () => {
    const input = [
      'skip',
      'x := 3',
      'if 3 <= 33 then skip else x := 1',
      'while true do skip',
      '(skip)',
    ];
    const tokens = input.map(aexpr => tokenizer(strToChars(aexpr)));
    const syntaxes = tokens.map(ts => pProg.parse(ts));
    const result = array.sequence(option)(syntaxes);

    expect(result).toEqual(
      some([
        [new Skip(), []],
        [new Ass('x', new Num(3)), []],
        [new If(new Le(new Num(3), new Num(33)), new Skip(), new Ass('x', new Num(1))), []],
        [new While(new True(), new Skip()), []],
        [new Skip(), []],
      ]),
    );
  });

  it('should parse composed if', () => {
    const input = strToChars('if 1 = 1 then skip else (x := 1; y := 2)');
    const tokens = tokenizer(input);

    expect(pProg.parse(tokens)).toEqual(
      some([
        new If(
          new Eq(new Num(1), new Num(1)),
          new Skip(),
          new Comp(new Ass('x', new Num(1)), new Ass('y', new Num(2))),
        ),
        [],
      ]),
    );
  });

  it('should parse factorial statement', () => {
    const input = strToChars('y := 1; while !(x = 1) do (y := y * x; x := x - 1)');
    const tokens = tokenizer(input);

    expect(pProg.parse(tokens)).toMatchSnapshot();
  });

  it('should parse composed while', () => {
    const input = strToChars('y := 1; while !(x = 1) do (y := y * x; x := x - 1); z := 1');
    const tokens = tokenizer(input);

    expect(pProg.parse(tokens)).toMatchSnapshot();
  });

  it('should handle for syntactic sugar', () => {
    const input = strToChars('for x := 1 to 10 do y := y + x');
    const tokens = tokenizer(input);
    const syntax = pProg.parse(tokens);
    const state = initialState({ y: 1 });
    const result = syntax.map(([stm]) => semantic(stm)(state)).map(s => s('y'));

    expect(result).toEqual(some(56));
  });

  it('should handle repeat until syntactic sugar', () => {
    const input = strToChars('repeat x := x + 1 until x = 10');
    const tokens = tokenizer(input);
    const syntax = pProg.parse(tokens);
    const state = initialState({ x: 0 });
    const result = syntax.map(([stm]) => semantic(stm)(state)).map(s => s('x'));

    expect(result).toEqual(some(10));
  });

  it('should execute repeat until S at least once', () => {
    const input = strToChars('repeat x := x + 1 until 1 = 1');
    const tokens = tokenizer(input);
    const syntax = pProg.parse(tokens);
    const state = initialState({ x: 0 });
    const result = syntax.map(([stm]) => semantic(stm)(state)).map(s => s('x'));

    expect(result).toEqual(some(1));
  });
});
