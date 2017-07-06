'use strict'

var QUEUE_BLOCK_SIZE = 1000

function Queue() {
  if (!(this instanceof Queue)) return new Queue()
  var b = createBlock()
  this._blocks = 1
  this._first  = b
  this._fp     = 0
  this._last   = b
  this._lp     = 0
}

function createBlock(o) {
  var b = Object.create(null)
  if (o) {
    b[0] = o
  }
  b.n = null
  return b
}

function _sizeQueue(queue) {
  return QUEUE_BLOCK_SIZE * (queue._blocks - 1) + queue._lp - queue._fp
}

function _isEmpty(queue) {
  return queue._blocks === 1 && queue._lp >= queue._fp
}

Object.defineProperty(Queue.prototype, 'size', {
  get: function () {
    return _sizeQueue(this)
  }
})

Object.defineProperty(Queue.prototype, 'isEmpty', {
  get: function () {
    return _isEmpty(this)
  }
})

Queue.prototype.enqueue = function (o) {
  if (this._lp === QUEUE_BLOCK_SIZE) {
    var newBlock = createBlock(o)
    this._blocks++
    this._last.n = newBlock
    this._last = newBlock
    this._lp = 1
  } else {
    this._last[this._lp++] = o
  }
}

Queue.prototype.dequeue = function () {
  if (this._blocks === 1 && this._fp >= this._lp) return null
  var qfb = this._first, r = qfb[this._fp]
  qfb[this._fp] = null
  if (++this._fp === QUEUE_BLOCK_SIZE) {
    if (this._blocks === 1) {
      this._lp = 0;
    } else {
      this._blocks--;
      this._first = this._first.n;
    }
    this._fp = 0
  } else if (this._blocks === 1 && this._fp >= this._lp) {
    this._lp = this._fp = 0
  }
  return r
}

function _queueIterator(queue) {
  var b = queue._first, bp = queue._fp, lb = queue._last, lp = queue._lp
  return {
    next: function () {
      if (b === null || (b === lb && bp >= lp)) {
        return { done: true }
      } else {
        var r = b[bp]
        if (++bp === QUEUE_BLOCK_SIZE) {
          b = b.n
          bp = 0
          if (b === null) lb = null
        }
        return { done: false, value: r }
      }
    }
  }
}

Queue.prototype.iter = function () {
  return _queueIterator(this)
}

var MVAR_EMPTY  = 1 << 0
var MVAR_FULL   = 1 << 1
var MVAR_KILLED = 1 << 2

function MVar() {
  this.readers = new Queue()
  this.writers = new Queue()
  this.waiters = 0
  this.val     = undefined
  this._state  = 0
  _setMVarEmpty(this)
}

function _isMVarFull (mv) {
  return (mv._state & MVAR_FULL) !== 0
}

function _setMVarValue (mv, value) {
  mv._state = mv._state | MVAR_FULL
  mv.val = value
}

function _unsetMVarFull (mv) {
  mv._state = mv._state & (~MVAR_FULL)
  mv.val = undefined
}

function _isMVarEmpty (mv) {
  return (mv._state & MVAR_EMPTY) !== 0
}

function _setMVarEmpty(mv) {
  mv._state = mv._state | MVAR_EMPTY
  mv.val = undefined
}

function _unsetMVarEmpty(mv) {
  mv._state = mv._state & (~MVAR_EMPTY)
}

function _isMVarKilled(mv) {
  return (mv._state & MVAR_KILLED) !== 0
}

function _setMVarKilled(mv, err) {
  mv._state = mv._state | MVAR_KILLED
  mv.val = err
}

function createEffRec(cb) {
  var w = Object.create(null)
  w.cb = cb
  return w
}

function createWriter(cb, value) {
  var w = Object.create(null)
  w.cb = cb
  w.value = value
  return w
}

function _queueWaiter(waiter, mv) {
  var i = mv.waiters
  mv.waiters++
  mv[i] = waiter
}

function _wakeupWaiter(mv, val, right) {
  if (mv.waiters > 0) {
    var l = mv.waiters, value = right(val)
    for (var i = 0; i < l; i++) {
      mv[i].cb(value)
      mv[i] = undefined
    }
    mv.waiters = 0
  }
}

exports.makeEmptyVar = function () {
  return new MVar()
}
