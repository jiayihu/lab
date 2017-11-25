export function debugMethods(obj) {
  return new Proxy(obj, {
    get: function(target, name, receiver) {
      if (typeof name === 'function') {
        return function(...args) {
          const methodName = name;
          console.log(methodName);
          return target[name](...args);
        };
      } else {
        return Reflect.get(target, name, receiver);
      }
    },
  });
}
