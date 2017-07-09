'use strict'

var PURE      = 0
var THROW     = 1
var SYNC      = 2
var ASYNC     = 3
var BIND      = 4
var ATTEMPT   = 5
var BRACKET   = 6
var CONS      = 7
var RECOVER   = 8
var RESUME    = 9
var FINALIZED = 10

// Status
var BLOCKED   = 0; // No effect is running.
var PENDING   = 1; // An async effect is running.
var RETURN    = 2; // The current stack has returned.
var CONTINUE  = 3; // Run the next effect.
var BINDSTEP  = 4; // Apply the next bind.
var COMPLETED = 5; // The entire thread has completed.

function IO(tag, _1, _2, _3) {
  this.tag = tag
  this._1  = _1
  this._2  = _2
  this._3  = _3
}

function nonCanceler(err) {
  return new IO(SYNC, void 0)
}

exports._pure = function (a) {
  return new IO(PURE, a)
}

exports._throwError = function (error) {
  return new IO(THROW, error)
}

exports._map = function (f, io) {
  if (io.tag === PURE) {
    return new IO(PURE, f(io._1))
  }
  return new IO(BIND, io, function (x) {
    return new IO(PURE, f(x))
  })
}

exports._bind = function (io, k) {
  return new IO(BIND, io, k)
}

exports._liftEff = function (eff) {
  return new IO(SYNC, eff)
}

exports.makeIO = function (k) {
  return new IO(ASYNC, k)
}

exports.attempt = function (io) {
  return new IO(ATTEMPT, io)
}

exports._bracket = function (acquire, release, k) {
  return new IO(BRACKET, acquire, release, k)
}

exports._delay = function () {
  function _setDelay(n, k) {
    // check if 'setImmediate present'
    if (n <= 0 && typeof setImmediate !== "undefined") {
      return setImmediate(k)
    }
    return setTimeout(k, n)
  }

  function _clearDelay(n, k) {
    if (n <= 0 && typeof clearImmediate !== "undefined") {
      return clearImmediate(k)
    }
    return clearTimeout(k)
  }

  return function (right, ms) {
    return new IO(ASYNC, function (cb) {
      return function () {
        var timer = _setDelay(ms, cb(right()))
        return function () {
          return new IO(SYNC, function () {
            return right(_clearDelay(ms, timer))
          })
        }
      }
    })
  }
}()

function _nextTick(cb) {
  // first check if this Node, if it use process next tick
  if ({}.toString.call(process) === "[object process]") {
    process.nextTick(cb)
  } else if (typeof setImmediate === 'function') {
    setImmediate(cb)
  } else {
    setTimeout(cb, 0)
  }
}

exports._launchIO = function (isLeft, fromLeft, fromRight, left, right, task) {
  return function () {
    var runTick = 0, status = CONTINUE, step = task, fail = null, interrupt = null

    // Stack of continuations for the current thread.
    var bhead = null
    var btail = null

    // Stack of attempts and finalizers for error recovery. This holds a union
    // of an arbitrary Task finalizer or a Cons list of bind continuations.
    var attempts = null, bracket = 0

    // Each join gets a new id so they can be revoked.
    var joinId = 0
    var joins  = {}

    // Temporary bindings for the various branches.
    var tmp, result, attempt, canceler

    function run(localRunTick) {
      while (true) {
        switch (status) {
          case BINDSTEP:
            status = CONTINUE
            step   = bhead(step)
            if (btail === null) {
              bhead = null
            } else {
              bhead = btail._1;
              btail = btail._2;
            }
            break;

          case CONTINUE:
            switch (step.tag) {
              case BIND:
                if (bhead) {
                  btail = new IO(CONS, bhead, btail);
                }
                bhead  = step._2
                status = CONTINUE
                step   = step._1
                break;

              case PURE:
                if (bhead === null) {
                  status = RETURN
                  step   = right(step._1)
                } else {
                  status = BINDSTEP
                  step   = step._1
                }
                break;

              case THROW:
                bhead  = null
                btail  = null
                status = RETURN
                fail   = left(step._1)
                break;

              case SYNC:
                status = BLOCKED;
                result = runSync(left, right, step._1)
                if (isLeft(result)) {
                  status = RETURN
                  fail   = result
                } else if (bhead === null) {
                  status = RETURN
                  step   = result
                } else {
                  status = BINDSTEP
                  step   = fromRight(result)
                }
                break;

              case ASYNC:
                status = BLOCKED
                canceler = runAsync(left, step._1, function (result) {
                  return function () {
                    if (runTick !== localRunTick) {
                      return;
                    }
                    tmp = status;
                    if (isLeft(result)) {
                      status = RETURN;
                      fail   = result;
                    } else if (bhead === null) {
                      status = RETURN;
                      step   = result;
                    } else {
                      status = BINDSTEP;
                      step   = fromRight(result);
                    }
                    if (tmp === PENDING) {
                      run(++runTick);
                    } else {
                      localRunTick = ++runTick;
                    }
                  }
                })
                if (status === BLOCKED) {
                  status = PENDING
                  step   = canceler
                }
                break;

              case ATTEMPT:
                attempts = new IO(CONS, new IO(RECOVER, bhead, btail), attempts)
                bhead    = null
                btail    = null
                status   = CONTINUE
                step     = step._1
                break;

              case BRACKET:
                bracket++;
                if (bhead === null) {
                   attempts = new IO(CONS, step, attempts);
                } else {
                   attempts = new IO(CONS, step, new IO(CONS, new IO(RESUME, bhead, btail), attempts));
                }
                bhead  = null;
                btail  = null;
                status = CONTINUE;
                step   = step._1;
                break;
            }
          break;

          case RETURN:
            if (attempts === null) {
              runTick++
              status = COMPLETED
              step   = interrupt || fail || step
            } else {
              attempt = attempts._1
              switch (attempt.tag) {
                case RECOVER:
                  attempts = attempts._2
                  if (interrupt === null) {
                    bhead  = attempt._1
                    btail  = attempt._2
                    status = BINDSTEP
                    step   = fail || step
                    fail   = null
                  }
                break

                case RESUME:
                  attempts = attempts._2
                  if (interrupt === null && fail === null) {
                    bhead  = attempt._1
                    btail  = attempt._2
                    status = BINDSTEP
                    step   = fromRight(step)
                  }
                  break

                case BRACKET:
                  bracket--
                  if (fail === null) {
                    result   = fromRight(step)
                    attempts = new IO(CONS, attempt._2(result), attempts._2)
                    if (interrupt === null || bracket > 0) {
                      status = CONTINUE
                      step   = attempt._3(result)
                    }
                  } else {
                    attempts = attempts._2
                  }
                  break

                case FINALIZED:
                  bracket--
                  attempts = attempts._2
                  step     = attempt._1
                  break

                default:
                  bracket++
                  attempts._1 = new IO(FINALIZED, step)
                  status      = CONTINUE
                  step        = attempt
              }
            }
            break;

          case COMPLETED:
            tmp = false
            for (var k in joins) {
              tmp = true
              runJoin(step, joins[k])
            }
            joins = tmp
            if (isLeft(step) && !joins) {
              _nextTick(function () {
                if (!joins) {
                  throw fromLeft(step);
                }
              })
            }
            return

          case BLOCKED: return
          case PENDING: return
        }
        tmp       = null
        result    = null
        attempt   = null
        canceler  = null
      }
    }

    function addJoinCallback (cb) {
      var jid    = joinId++
      joins[jid] = cb
      return function (error) {
        return new IO(SYNC, function () {
          joins[jid] = undefined
          return right()
        })
      }
    }

    function kill (error) {
      return new IO(ASYNC, function (cb) {
        return function () {
          // Shadow the canceler binding because it can potentially be
          // clobbered if we call `run`.
          var canceler
          var killCb = function () {
            return cb(right(void 0))
          };
          switch (status) {
          case COMPLETED:
            canceler = nonCanceler
            killCb()()
            break;
          case PENDING:
            canceler = addJoinCallback(killCb)
            if (interrupt === null) {
              interrupt = left(error)
            }
            // If we can interrupt the pending action, enqueue the canceler as
            // a non-interruptible finalizer.
            if (bracket === 0) {
              attempts = new IO(CONS, step(error), attempts)
              bhead    = null
              btail    = null
              status   = RETURN
              step     = null
              fail     = null
              run(runTick++)
            }
            break;
          default:
            canceler = addJoinCallback(killCb)
            if (interrupt === null) {
              interrupt = left(error)
            }
            if (bracket === 0) {
              bhead  = null
              btail  = null
              status = RETURN
            }
          }
          return canceler
        }
      })
    }

    function join () {
      return new IO(ASYNC, function (cb) {
        return function () {
          if (status === COMPLETED) {
            joins = true
            cb(step)()
            return nonCanceler
          }
          return addJoinCallback(cb)
        }
      })
    }

    run(runTick)

    return { kill: kill, join: join() };
  }
}

function runJoin (result, cb) {
  try {
    cb(result)();
  } catch (error) {
    _nextTick(function () {
      throw error;
    }, 0)
  }
}

function runSync (left, right, eff) {
  try {
    return right(eff());
  } catch (error) {
    return left(error)
  }
}

function runAsync (left, eff, k) {
  try {
    return eff(k)();
  } catch (error) {
    k(left(error))();
    return nonCanceler;
  }
}
