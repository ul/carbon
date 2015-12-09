return VDOM = {
  diff: require("virtual-dom/diff"),
  patch: require("virtual-dom/patch"),
  create: require("virtual-dom/create-element"),
  h: require("virtual-dom/h"),
  svg: require("virtual-dom/virtual-hyperscript/svg"),
  VText: require("virtual-dom/vnode/vtext"),
  VNode: require("virtual-dom/vnode/vnode"),
  Delegator: require('dom-delegator')
}
