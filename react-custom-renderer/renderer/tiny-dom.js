import Reconciler from 'react-reconciler';
import emptyObject from 'fbjs/lib/emptyObject';

const TinyDOMRenderer = Reconciler({
  appendInitialChild(parentInstance, child) {
    console.group('appendInitialChild');
    console.log(parentInstance, child);
    console.groupEnd('appendInitialChild');
    parentInstance.appendChild(child);
  },

  // Create the DOMElement, but attributes are set in `finalizeInitialChildren`
  createInstance(type, props, rootContainerInstance, hostContext, internalInstanceHandle) {
    console.group('createInstance');
    console.log(type, props, hostContext);
    console.groupEnd('createInstance');
    return document.createElement(type);
  },

  // Useful for React Native
  createTextInstance(text, rootContainerInstance, internalInstanceHandle) {
    console.group('createInstance');
    console.log(text);
    console.groupEnd('createInstance');
    return document.createTextNode(text);
  },

  // Actually set the attributes and text content and check if needs focus
  finalizeInitialChildren(domElement, type, props) {
    console.group('finalizeInitialChildren');
    console.log(domElement, type, props);
    console.groupEnd('finalizeInitialChildren');

    // Set the prop to the domElement
    Object.keys(props).forEach(propName => {
      const propValue = props[propName];

      switch (propName) {
        case 'children':
          domElement.textContent = propValue;
          break;

        default:
          domElement.setAttribute(propName, propValue);
          break;
      }
    });

    // Check if needs focus
    switch (type) {
      case 'button':
      case 'input':
      case 'select':
      case 'textarea':
        return !!props.autoFocus;
    }

    return false;
  },

  // Useful only for testing
  getPublicInstance(inst) {
    return inst;
  },

  // Commit hooks, useful mainly for react-dom syntethic events
  prepareForCommit() {
    console.log('prepareForCommit');
  },
  resetAfterCommit() {
    console.log(resetAfterCommit);
  },

  // Calculate the updatePayload
  prepareUpdate(domElement, type, oldProps, newProps) {
    console.group('prepareUpdate');
    console.log(domElement, type, oldProps, newProps);
    console.groupEnd('prepareUpdate');
    return [];
  },

  getRootHostContext(rootInstance) {
    console.group('getRootHostContext');
    console.log(rootInstance);
    console.groupEnd('getRootHostContext');
    return emptyObject;
  },
  getChildHostContext(parentHostContext, type) {
    console.group('getRootHostContext');
    console.log(parentHostContext, type);
    console.groupEnd('getRootHostContext');
    return emptyObject;
  },

  shouldSetTextContent(type, props) {
    console.group('shouldSetTextContent');
    console.log(type, props);
    console.groupEnd('shouldSetTextContent');
    return (
      type === 'textarea' ||
      typeof props.children === 'string' ||
      typeof props.children === 'number'
    );
  },

  now: () => {},

  useSyncScheduling: true,

  mutation: {
    appendChild(parentInstance, child) {
      console.group('appendChild');
      console.log(parentInstance, child);
      console.groupEnd('appendChild');
      parentInstance.appendChild(child);
    },

    appendChildToContainer(parentInstance, child) {
      console.group('appendChildToContainer');
      console.log(parentInstance, child);
      console.groupEnd('appendChildToContainer');
      parentInstance.appendChild(child);
    },

    removeChild(parentInstance, child) {
      console.group('removeChild');
      console.log(parentInstance, child);
      console.groupEnd('removeChild');
      parentInstance.removeChild(child);
    },

    removeChildFromContainer(parentInstance, child) {
      console.group('removeChildFromContainer');
      console.log(parentInstance, child);
      console.groupEnd('removeChildFromContainer');
      parentInstance.removeChild(child);
    },

    insertBefore(parentInstance, child, beforeChild) {
      console.group('insertBefore');
      console.log(parentInstance, child, beforeChild);
      console.groupEnd('insertBefore');
      parentInstance.insertBefore(child, beforeChild);
    },

    insertInContainerBefore(parentInstance, child, beforeChild) {
      console.group('insertInContainerBefore');
      console.log(parentInstance, child, beforeChild);
      console.groupEnd('insertInContainerBefore');
      parentInstance.insertBefore(child, beforeChild);
    },

    // Actually apply the new props
    commitUpdate(domElement, updatePayload, type, oldProps, newProps, internalInstanceHandle) {
      console.group('commitUpdate');
      console.log(domElement, updatePayload, type, oldProps, newProps);
      console.groupEnd('commitUpdate');
      console.log('Update to be done', updatePayload);
    },

    commitMount(domElement, type, newProps, internalInstanceHandle) {
      console.group('commitMount');
      console.log(domElement, type, newProps);
      console.groupEnd('commitMount');
      domElement.focus();
    },

    commitTextUpdate(textInstance, oldText, newText) {
      console.group('commitTextUpdate');
      console.log(textInstance, oldText, newText);
      console.groupEnd('commitTextUpdate');
      textInstance.nodeValue = newText;
    },

    resetTextContent(domElement) {
      console.group('commitTextUpdate');
      console.log(textInstance, oldText, newText);
      console.groupEnd('commitTextUpdate');
      domElement.textContent = '';
    },
  },
});

function renderSubtreeIntoContainer(children, domContainer, callback) {
  let root = domContainer._reactRootContainer;

  if (!root) {
    // Remove all children of the domContainer
    let rootSibling;
    while ((rootSibling = domContainer.lastChild)) {
      domContainer.removeChild(rootSibling);
    }

    const newRoot = TinyDOMRenderer.createContainer(domContainer);
    root = domContainer._reactRootContainer = newRoot;
  }

  TinyDOMRenderer.updateContainer(children, root, null, callback);
}

export const ReactTinyDOM = {
  render(element, domContainer, callback) {
    return renderSubtreeIntoContainer(element, domContainer, callback);
  },
};
