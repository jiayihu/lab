import { span, split, isEmpty } from 'fp-ts/lib/Array';
import { Char } from 'newtype-ts/lib/Char';
import { iso } from 'newtype-ts';
import {
  twoCharOps,
  keywords,
  Var,
  Num,
  Aexpr,
  Add,
  Mult,
  Sub,
  True,
  False,
  Eq,
  Le,
  Neg,
  And,
  Bexpr,
} from './language';
import { Option, some, none } from 'fp-ts/lib/Option';
import { pair } from './utils';

const isoChar = iso<Char>();
export const strToChars = (xs: string): Char[] => Array.from(xs).map(isoChar.wrap);
export const charsToStr = (chars: Char[]): string => chars.map(isoChar.unwrap).join('');

type Token = Char[];

const isAlpha = (c: Char) => /^[A-Za-z_]$/.test(isoChar.unwrap(c));
const isAlphaNum = (c: Char) => /^\w$/.test(isoChar.unwrap(c));
const isDigit = (c: Char) => /^\d$/.test(isoChar.unwrap(c));
const isSpace = (c: Char) => /^\s$/.test(isoChar.unwrap(c));

export const tokenizer = (input: Char[]): Token[] => {
  if (!input.length) return [];

  const [c, ...cs] = input;
  const spanToken = (pred: (char: Char) => boolean): { token: Token; rest: Char[] } => {
    const { init, rest } = span(input, pred);
    return { token: init, rest };
  };

  if (isSpace(c)) return tokenizer(cs);
  if (isDigit(c)) {
    const { token, rest } = spanToken(isDigit);
    return [token, ...tokenizer(rest)];
  }
  if (isAlpha(c)) {
    const { token, rest } = spanToken(isAlphaNum);
    return [token, ...tokenizer(rest)];
  }
  if (input.length >= 2) {
    const [twoChars, rest] = split(2, input);
    if (twoCharOps.includes(charsToStr(twoChars))) {
      return [twoChars, ...tokenizer(rest)];
    }
  }

  return [[c], ...tokenizer(cs)];
};

/**
 * Parser Monad, basically a State monad implementation where the state is the
 * list of tokens but the computation can fail.
 * {@link https://github.com/gcanti/fp-ts/blob/master/src/State.ts#L19-L65}
 */
export class Parser<A> {
  constructor(readonly parse: (tokens: Token[]) => Option<[A, Token[]]>) {}

  static of = <A>(a: A): Parser<A> => {
    return new Parser(tokens => some(pair([a, tokens])));
  };

  map<B>(f: (a: A) => B): Parser<B> {
    return new Parser(tokens => {
      return this.parse(tokens).map(([v, restTokens]) => {
        return pair([f(v), restTokens]);
      });
    });
  }

  ap<B>(fab: Parser<(a: A) => B>): Parser<B> {
    return fab.chain(f => this.map(f));
  }

  ap_<B, C>(this: Parser<(b: B) => C>, fb: Parser<B>): Parser<C> {
    return fb.ap(this);
  }

  chain<B>(f: (a: A) => Parser<B>): Parser<B> {
    return new Parser(tokens => {
      return this.parse(tokens).chain(([v, restTokens]) => {
        return f(v).parse(restTokens);
      });
    });
  }

  static empty = <A>(): Parser<A> => new Parser(_ => none);

  alt(fa: Parser<A>): Parser<A> {
    return new Parser(tokens => {
      return this.parse(tokens).orElse(() => fa.parse(tokens));
    });
  }
}

const item: Parser<Token> = new Parser(tokens => {
  if (isEmpty(tokens)) return none;

  const [x, ...xs] = tokens;
  return some(pair([x, xs]));
});

const pWithSep = <A, B>(pa: Parser<A>) => (pb: Parser<B>): Parser<A[]> => {
  return pa.chain(a =>
    pb
      .chain(() => pWithSep(pa)(pb))
      .chain(as => {
        return Parser.of([a, ...as]).alt(Parser.of([a]));
      }),
  );
};

const pSat = (pred: (token: Token) => boolean): Parser<Token> => {
  return item.chain(token => (pred(token) ? Parser.of(token) : Parser.empty()));
};

const pLit = (c: Token): Parser<Token> => pSat(x => x.join('') === c.join(''));

/**
 * Arithmetic expression parsers
 */

export const pVar: Parser<Var> = pSat(
  token => !keywords.includes(charsToStr(token)) && isAlpha(token[0]),
).map(token => new Var(charsToStr(token)));

export const pNum: Parser<Num> = pSat(token => token.every(isDigit)).map(
  token => new Num(Number(token.join(''))),
);

export const pBasisAexpr: Parser<Aexpr> = (pVar as Parser<Aexpr>).alt(pNum);

export const pParAexpr: Parser<Aexpr> = pLit(strToChars('('))
  .chain(() => pBasisAexpr)
  .chain(a => pLit(strToChars(')')).map(() => a));

export const pAdd: Parser<Add> = pBasisAexpr.chain(a1 =>
  pLit(strToChars('+'))
    .chain(() => pBasisAexpr)
    .map(a2 => new Add(a1, a2)),
);

export const pMult: Parser<Mult> = pBasisAexpr.chain(a1 =>
  pLit(strToChars('*'))
    .chain(() => pBasisAexpr)
    .map(a2 => new Mult(a1, a2)),
);

export const pSub: Parser<Sub> = pBasisAexpr.chain(a1 =>
  pLit(strToChars('-'))
    .chain(() => pBasisAexpr)
    .map(a2 => new Sub(a1, a2)),
);

export const pAexpr: Parser<Aexpr> = (pAdd as Parser<Aexpr>)
  .alt(pMult)
  .alt(pSub)
  .alt(pBasisAexpr)
  .alt(pParAexpr);

/**
 * Boolean expression parsers
 */

export const pTrue: Parser<True> = pSat(token => charsToStr(token) === 'true').map(
  () => new True(true),
);

export const pFalse: Parser<False> = pSat(token => charsToStr(token) === 'false').map(
  () => new False(false),
);

export const pEq: Parser<Eq> = pAexpr.chain(a1 =>
  pLit(strToChars('='))
    .chain(() => pAexpr)
    .map(a2 => new Eq(a1, a2)),
);

export const pLe: Parser<Le> = pAexpr.chain(a1 =>
  pLit(strToChars('<'))
    .chain(() => pLit(strToChars('=')))
    .chain(() => pAexpr)
    .map(a2 => new Le(a1, a2)),
);

export const pBasisBexpr: Parser<Bexpr> = (pTrue as Parser<Bexpr>)
  .alt(pFalse)
  .alt(pEq)
  .alt(pLe);

export const pNeg: Parser<Neg> = pLit(strToChars('¬'))
  .chain(() => pBexpr)
  .map(b => new Neg(b));

export const pAnd: Parser<And> = pBasisBexpr.chain(a1 =>
  pLit(strToChars('∧'))
    .chain(() => pBasisBexpr)
    .map(a2 => new And(a1, a2)),
);

export const pParBexpr: Parser<Bexpr> = pLit(strToChars('('))
  .chain(() => pBasisBexpr)
  .chain(a => pLit(strToChars(')')).map(() => a));

export const pBexpr: Parser<Bexpr> = (pNeg as Parser<Bexpr>)
  .alt(pAnd)
  .alt(pBasisBexpr)
  .alt(pParBexpr);
