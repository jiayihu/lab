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

const pLit = (c: Token): Parser<Token> => pSat(token => token.join('') === c.join(''));

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

const pFactor: Parser<Aexpr> = pLit(strToChars('('))
  .chain(() => pAexpr)
  .chain(a => pLit(strToChars(')')).map(() => a))
  .alt(pBasisAexpr);

const pTerm: Parser<Aexpr> = pFactor.chain(a1 =>
  pLit(strToChars('*'))
    .chain(() => pTerm)
    .map(a2 => new Mult(a1, a2) as Aexpr)
    .alt(Parser.of(a1)),
);

const pAdd: Parser<Aexpr> = pTerm.chain(a1 =>
  pLit(strToChars('+'))
    .chain(() => pAexpr)
    .map(a2 => new Add(a1, a2)),
);

const pSub: Parser<Aexpr> = pTerm.chain(a1 =>
  pLit(strToChars('-'))
    .chain(() => pAexpr)
    .map(a2 => new Sub(a1, a2)),
);

export const pAexpr: Parser<Aexpr> = pAdd.alt(pSub).alt(pTerm);

/**
 * Boolean expression parsers
 */

const pTrue: Parser<True> = pSat(token => charsToStr(token) === 'true').map(() => new True(true));

const pFalse: Parser<False> = pSat(token => charsToStr(token) === 'false').map(
  () => new False(false),
);

const pEq: Parser<Eq> = pAexpr.chain(a1 =>
  pLit(strToChars('='))
    .chain(() => pAexpr)
    .map(a2 => new Eq(a1, a2)),
);

const pLe: Parser<Le> = pAexpr.chain(a1 =>
  pLit(strToChars('<'))
    .chain(() => pLit(strToChars('=')))
    .chain(() => pAexpr)
    .map(a2 => new Le(a1, a2)),
);

const pBasisBexpr: Parser<Bexpr> = (pTrue as Parser<Bexpr>)
  .alt(pFalse)
  .alt(pEq)
  .alt(pLe);

const pNeg: Parser<Neg> = pLit(strToChars('¬'))
  .chain(() => pBexpr)
  .map(b => new Neg(b));

const pAnd: Parser<And> = pBasisBexpr.chain(a1 =>
  pLit(strToChars('∧'))
    .chain(() => pBasisBexpr)
    .map(a2 => new And(a1, a2)),
);

const pParBexpr: Parser<Bexpr> = pLit(strToChars('('))
  .chain(() => pBexpr)
  .chain(a => pLit(strToChars(')')).map(() => a));

export const pBexpr: Parser<Bexpr> = (pNeg as Parser<Bexpr>)
  .alt(pAnd)
  .alt(pBasisBexpr)
  .alt(pParBexpr);

/**
 * Statement parsers
 */

const pAss: Parser<Ass> = pSat(token => isAlpha(token[0])).chain(name =>
  pLit(strToChars(':='))
    .chain(() => pAexpr)
    .map(a2 => new Ass(name.join(''), a2)),
);

const pSkip: Parser<Skip> = pLit(strToChars('skip')).map(() => new Skip());

const pIf: Parser<If> = pLit(strToChars('if'))
  .chain(() => pBexpr)
  .chain(bexpr =>
    pLit(strToChars('then'))
      .chain(() => pStm)
      .chain(stm1 =>
        pLit(strToChars('else'))
          .chain(() => pStm)
          .map(stm2 => new If(bexpr, stm1, stm2)),
      ),
  );

const pWhile: Parser<While> = pLit(strToChars('while'))
  .chain(() => pBexpr)
  .chain(bexpr =>
    pLit(strToChars('do'))
      .chain(() => pProg)
      .map(stm => new While(bexpr, stm)),
  );

const pStm: Parser<Stm> = (pAss as Parser<Stm>)
  .alt(pSkip)
  .alt(pIf)
  .alt(pWhile);

const pParStm: Parser<Stm> = pLit(strToChars('('))
  .chain(() => pProg)
  .chain(stm => pLit(strToChars(')')).map(() => stm));

export const pProg: Parser<Stm> = pOneOrMoreWithSep(pStm.alt(pParStm))(pLit(strToChars(';'))).map(
  stms =>
    stms.reduce<Stm>((comp, stm) => {
      // Cannot avoid putting Skip as initial value so if the program is only one stm avoid Comp
      return comp.type === 'Skip' ? stm : new Comp(comp, stm);
    }, new Skip()),
);
