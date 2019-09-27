import { Stm, State, If, Comp, While, Skip } from './syntax';
import { substState, evalAexpr, evalBexpr } from './eval';

export class InterConfig {
  type: 'InterConfig' = 'InterConfig';
  constructor(readonly stm: Stm, readonly state: State) {}
}

export class FinalConfig {
  type: 'FinalConfig' = 'FinalConfig';
  constructor(readonly state: State) {}
}

export type Config = InterConfig | FinalConfig;

const isFinal = (config: Config): config is FinalConfig => {
  return config.type === 'FinalConfig';
};

export const transition = (config: Config): Config => {
  if (isFinal(config)) return config;

  const { stm, state } = config;

  switch (stm.type) {
    case 'Ass': {
      const { name, aexpr } = stm;
      return new FinalConfig(substState(state)(name)(evalAexpr(aexpr)(state)));
    }
    case 'Skip': {
      return new FinalConfig(state);
    }
    case 'Comp': {
      const { stm1, stm2 } = stm;
      const config1 = transition(new InterConfig(stm1, state));

      return isFinal(config1)
        ? new InterConfig(stm2, config1.state)
        : new InterConfig(new Comp(config1.stm, stm2), config1.state);
    }
    case 'If': {
      const { bexpr, stm1, stm2 } = stm;

      return evalBexpr(bexpr)(state) ? new InterConfig(stm1, state) : new InterConfig(stm2, state);
    }
    case 'While': {
      const { bexpr, stm: whileStm } = stm;

      return new InterConfig(
        new If(bexpr, new Comp(whileStm, new While(bexpr, whileStm)), new Skip()),
        state,
      );
    }
  }
};

export const deriveSeq = (config: Config): Config[] => {
  if (isFinal(config)) return [config];

  return [config, ...deriveSeq(transition(config))];
};

export const semantic = (stm: Stm) => (state: State): State => {
  const derivation = deriveSeq(new InterConfig(stm, state));
  const last = derivation[derivation.length - 1];

  return last.state;
};
