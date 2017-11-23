import Reconciler from 'react-reconciler';
import emptyObject from 'fbjs/lib/emptyObject';

const { spawnSync } = require('child_process');

const createVoices = (voice, text, options = {}) => {
  const args = ['-v', voice];

  if (options.interactive) args.push('-i');
  if (options.rate) {
    args.push('-r');
    args.push(options.rate);
  }

  args.push(text);

  spawnSync('say', args, { stdio: 'inherit' });
};

const SpeechRenderer = Reconciler({
  // Only first time
  appendInitialChild(parentInstance, child) {},

  createInstance(type, props, internalInstanceHandle) {
    return emptyObject;
  },

  // Useful for React Native
  createTextInstance(text, rootContainerInstance, internalInstanceHandle) {
    return text;
  },

  // Calls `commitMount` if it returns true, useful for focus
  finalizeInitialChildren(instance, type, props) {
    return true;
  },

  // Useful only for testing
  getPublicInstance(inst) {
    return inst;
  },

  // Commit hooks, useful mainly for react-dom syntethic events
  prepareForCommit() {},
  resetAfterCommit() {},

  prepareUpdate(instance, type, oldProps, newProps) {
    return true;
  },

  getRootHostContext(rootInstance) {
    return emptyObject;
  },
  getChildHostContext() {
    return emptyObject;
  },

  shouldSetTextContent(type, props) {
    return false;
  },

  now: () => {},

  useSyncScheduling: true,

  mutation: {
    appendChild(parentInstance, child) {},

    appendChildToContainer(parentInstance, child) {},

    removeChild(parentInstance, child) {},

    removeChildFromContainer(parentInstance, child) {},

    insertBefore(parentInstance, child, beforeChild) {},

    // Actually apply the new props
    commitUpdate(instance, updatePayload, type, oldProps, newProps, internalInstanceHandle) {},

    commitMount(instance, type, newProps, internalInstanceHandle) {
      if (typeof newProps.children === 'string') {
        createVoices(type, newProps.children, newProps);
      }
    },

    commitTextUpdate(textInstance, oldText, newText) {},

    resetTextContent(instance) {},
  },
});

export const ReactSpeech = {
  render(element, callback) {
    const root = SpeechRenderer.createContainer({});
    SpeechRenderer.updateContainer(element, root, null, callback);
  },
};
