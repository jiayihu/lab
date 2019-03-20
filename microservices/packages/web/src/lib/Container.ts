/**
 * Barebone dependency injection container
 */
export class Container<T extends Record<string, any>> {
  registry = new Map<keyof T, any>();

  bind<K extends keyof T>(name: K, value: T[K]) {
    this.registry.set(name, value);
  }

  get<K extends keyof T>(name: K): T[K] {
    return this.registry.get(name);
  }
}
