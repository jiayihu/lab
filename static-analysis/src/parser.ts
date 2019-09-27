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
  Stm,
  Ass,
  Skip,
  Comp,
  If,
  While,
  Div,
} from './syntax';
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

const pOneOrMoreWithSep = <A, B>(pa: Parser<A>) => (pb: Parser<B>): Parser<A[]> => {
  return pa.chain(a =>
    pb
      .chain(() => pOneOrMoreWithSep(pa)(pb))
      .chain(as => Parser.of([a, ...as]))
      .alt(Parser.of([a])),
  );
};

const pSat = (pred: (token: Token) => boolean): Parser<Token> => {
  return item.chain(token => (pred(token) ? Parser.of(token) : Parser.empty()));
};

const pLit = (cs: string): Parser<Token> => pSat(token => token.join('') === cs);

/**
 * Arithmetic expression parsers, it handles right-associativity and Mult precedence.
 * Based on the following grammar:
 *
 * aexpr ::= term (+ aexpr | ε) | term (- aexpr | ε)
 * term ::= factor (* aexpr | ε)
 * factor ::= (aexpr) | basis
 * basic ::= Var | Num
 */

const pVar: Parser<Var> = pSat(
  token => !keywords.includes(charsToStr(token)) && isAlpha(token[0]),
).map(token => new Var(charsToStr(token)));

const pNum: Parser<Num> = pSat(token => token.every(isDigit)).map(
  token => new Num(Number(token.join(''))),
);

const pBasisAexpr: Parser<Aexpr> = (pVar as Parser<Aexpr>).alt(pNum);

const pFactor: Parser<Aexpr> = pLit('(')
  .chain(() => pAexpr)
  .chain(a => pLit(')').map(() => a))
  .alt(pBasisAexpr);

const pTerm: Parser<Aexpr> = pFactor.chain(a1 =>
  pLit('*')
    .alt(pLit('/'))
    .chain(operator => {
      return pTerm.map(a2 =>
        operator.join('') === '*' ? new Mult(a1, a2) : (new Div(a1, a2) as Aexpr),
      );
    })
    .alt(Parser.of(a1)),
);

const pAdd: Parser<Aexpr> = pTerm.chain(a1 =>
  pLit('+')
    .chain(() => pAexpr)
    .map(a2 => new Add(a1, a2)),
);

const pSub: Parser<Aexpr> = pTerm.chain(a1 =>
  pLit('-')
    .chain(() => pAexpr)
    .map(a2 => new Sub(a1, a2)),
);

export const pAexpr: Parser<Aexpr> = pAdd.alt(pSub).alt(pTerm);

/**
 * Boolean expression parsers
 */

const pTrue: Parser<Bexpr> = pSat(token => charsToStr(token) === 'true').map(() => new True());

const pFalse: Parser<Bexpr> = pSat(token => charsToStr(token) === 'false').map(() => new False());

const pEq: Parser<Bexpr> = pAexpr.chain(a1 =>
  pLit('=')
    .chain(() => pAexpr)
    .map(a2 => new Eq(a1, a2)),
);

// Syntactic sugar
const pNotEq: Parser<Bexpr> = pAexpr.chain(a1 =>
  pLit('!=')
    .chain(() => pAexpr)
    .map(a2 => new Neg(new Eq(a1, a2))),
);

const pLe: Parser<Bexpr> = pAexpr.chain(a1 =>
  pLit('<=')
    .chain(() => pAexpr)
    .map(a2 => new Le(a1, a2)),
);

// Syntactic sugar
const pLt: Parser<Bexpr> = pAexpr.chain(
  a1 =>
    pLit('<')
      .chain(() => pAexpr)
      .map(a2 => new And(new Le(a1, a2), new Neg(new Eq(a1, a2)))), // <= & !=
);

// Syntactic sugar
const pGt: Parser<Bexpr> = pAexpr.chain(
  a1 =>
    pLit('>')
      .chain(() => pAexpr)
      .map(a2 => new Neg(new Le(a1, a2))), // ! (<=)
);

// Syntactic sugar
const pGe: Parser<Bexpr> = pAexpr.chain(
  a1 =>
    pLit('>=')
      .chain(() => pAexpr)
      .map(a2 => new Neg(new And(new Le(a1, a2), new Neg(new Eq(a1, a2))))), // !<
);

const pBasisBexpr: Parser<Bexpr> = pTrue
  .alt(pFalse)
  .alt(pEq)
  .alt(pNotEq)
  .alt(pLe)
  .alt(pLt)
  .alt(pGt)
  .alt(pGe);

const pParBexpr: Parser<Bexpr> = pLit('(')
  .chain(() => pBexpr)
  .chain(a => pLit(')').map(() => a));

const pFactorBexpr: Parser<Bexpr> = pParBexpr.alt(pBasisBexpr);

const pTermBExpr: Parser<Bexpr> = pLit('!')
  .chain(() => pFactorBexpr)
  .map(b => new Neg(b) as Bexpr)
  .alt(pFactorBexpr);

const pAnd: Parser<Bexpr> = pTermBExpr.chain(a1 =>
  pLit('&')
    .chain(() => pBexpr)
    .map(a2 => new And(a1, a2)),
);

// Syntactic sugar
const pOr: Parser<Bexpr> = pTermBExpr.chain(
  a1 =>
    pLit('|')
      .chain(() => pBexpr)
      .map(a2 => new Neg(new And(new Neg(a1), new Neg(a2)))), // De Morgan : A | B = !(!A & !B)
);

export const pBexpr: Parser<Bexpr> = pAnd.alt(pOr).alt(pTermBExpr);

/**
 * Statement parsers
 */

const pAss: Parser<Stm> = pSat(token => isAlpha(token[0])).chain(name =>
  pLit(':=')
    .chain(() => pAexpr)
    .map(a2 => new Ass(name.join(''), a2)),
);

const pSkip: Parser<Stm> = pLit('skip').map(() => new Skip());

const pIf: Parser<Stm> = pLit('if')
  .chain(() => pBexpr)
  .chain(bexpr =>
    pLit('then')
      .chain(() => pStm)
      .chain(stm1 =>
        pLit('else')
          .chain(() => pStm)
          .map(stm2 => new If(bexpr, stm1, stm2)),
      ),
  );

const pWhile: Parser<Stm> = pLit('while')
  .chain(() => pBexpr)
  .chain(bexpr =>
    pLit('do')
      .chain(() => pStm)
      .map(stm => new While(bexpr, stm)),
  );

// Syntactic sugar
const pFor: Parser<Stm> = pLit('for').chain(() =>
  pAss.chain(ass =>
    pLit('to')
      .chain(() => pAexpr)
      .chain(aexpr =>
        pLit('do')
          .chain(() => pStm)
          .map(stm => {
            const index = (ass as Ass).name;

            return new Comp(
              ass,
              new While(
                new Le(new Var(index), aexpr),
                new Comp(stm, new Ass(index, new Add(new Var(index), new Num(1)))),
              ),
            );
          }),
      ),
  ),
);

const pRepeatUntil: Parser<Stm> = pLit('repeat')
  .chain(() => pStm)
  .chain(stm =>
    pLit('until')
      .chain(() => pBexpr)
      .map(bexpr => new Comp(stm, new While(new Neg(bexpr), stm))),
  );

const pParStm: Parser<Stm> = pLit('(')
  .chain(() => pProg)
  .chain(stm => pLit(')').map(() => stm));

const pStm: Parser<Stm> = pAss
  .alt(pSkip)
  .alt(pIf)
  .alt(pWhile)
  .alt(pFor)
  .alt(pRepeatUntil)
  .alt(pParStm);

export const pProg: Parser<Stm> = pStm
  .chain(stm1 =>
    pLit(';')
      .chain(() => pProg)
      .map(stm2 => new Comp(stm1, stm2) as Stm),
  )
  .alt(pStm);
