import { Name, Z } from '../syntax';
import { random } from '../utils';

export type State = (name: Name) => Z;

export const initState: (vars: Record<Name, Z>) => State = (vars: Record<Name, Z>) => name => {
  const value = vars[name];

  return value !== undefined ? value : (vars[name] = random());
};
