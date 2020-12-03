"use strict";

require("core-js/modules/es.symbol");

require("core-js/modules/es.symbol.description");

require("core-js/modules/es.array.from");

require("core-js/modules/es.array.is-array");

require("core-js/modules/es.array.iterator");

require("core-js/modules/es.array.join");

require("core-js/modules/es.array.map");

require("core-js/modules/es.array.slice");

require("core-js/modules/es.array.splice");

require("core-js/modules/es.date.now");

require("core-js/modules/es.date.to-string");

require("core-js/modules/es.function.bind");

require("core-js/modules/es.function.name");

require("core-js/modules/es.map");

require("core-js/modules/es.math.acosh");

require("core-js/modules/es.math.asinh");

require("core-js/modules/es.math.atanh");

require("core-js/modules/es.math.cbrt");

require("core-js/modules/es.math.clz32");

require("core-js/modules/es.math.cosh");

require("core-js/modules/es.math.expm1");

require("core-js/modules/es.math.hypot");

require("core-js/modules/es.math.log10");

require("core-js/modules/es.math.log1p");

require("core-js/modules/es.math.log2");

require("core-js/modules/es.math.sign");

require("core-js/modules/es.math.sinh");

require("core-js/modules/es.math.tanh");

require("core-js/modules/es.number.constructor");

require("core-js/modules/es.number.is-integer");

require("core-js/modules/es.object.define-property");

require("core-js/modules/es.object.keys");

require("core-js/modules/es.object.to-string");

require("core-js/modules/es.regexp.exec");

require("core-js/modules/es.regexp.flags");

require("core-js/modules/es.set");

require("core-js/modules/es.string.iterator");

require("core-js/modules/es.string.split");

require("core-js/modules/es.string.link");

require("core-js/modules/web.dom-collections.iterator");

require("core-js/modules/web.timers");

require("regenerator-runtime/runtime");

function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function"); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } }); if (superClass) _setPrototypeOf(subClass, superClass); }

function _setPrototypeOf(o, p) { _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf(o, p) { o.__proto__ = p; return o; }; return _setPrototypeOf(o, p); }

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = _getPrototypeOf(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = _getPrototypeOf(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return _possibleConstructorReturn(this, result); }; }

function _possibleConstructorReturn(self, call) { if (call && (_typeof(call) === "object" || typeof call === "function")) { return call; } return _assertThisInitialized(self); }

function _assertThisInitialized(self) { if (self === void 0) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return self; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Date.prototype.toString.call(Reflect.construct(Date, [], function () {})); return true; } catch (e) { return false; } }

function _getPrototypeOf(o) { _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf(o) { return o.__proto__ || Object.getPrototypeOf(o); }; return _getPrototypeOf(o); }

function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); }

function _iterableToArrayLimit(arr, i) { if (typeof Symbol === "undefined" || !(Symbol.iterator in Object(arr))) return; var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }

function _createForOfIteratorHelper(o, allowArrayLike) { var it; if (typeof Symbol === "undefined" || o[Symbol.iterator] == null) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() {}; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e2) { throw _e2; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = o[Symbol.iterator](); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e3) { didErr = true; err = _e3; }, f: function f() { try { if (!normalCompletion && it["return"] != null) it["return"](); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var RUNTIME = function () {
  var pid_ctr = 0;
  var reference_ctr = 0;
  var process_state = {
    NORMAL: Symbol["for"]('normal'),
    KILL: Symbol["for"]('kill'),
    SUSPEND: Symbol["for"]('suspend'),
    CONTINUE: Symbol["for"]('continue'),
    RECEIVE: Symbol["for"]('receive'),
    SEND: Symbol["for"]('send'),
    SLEEPING: Symbol["for"]('sleeping'),
    RUNNING: Symbol["for"]('running'),
    SUSPENDED: Symbol["for"]('suspended'),
    STOPPED: Symbol["for"]('stopped'),
    SLEEP: Symbol["for"]('sleep'),
    EXIT: Symbol["for"]('exit'),
    NOMATCH: Symbol["for"]('no_match')
  };

  var PID = function PID(id) {
    _classCallCheck(this, PID);

    if (id == undefined) {
      this.id = pid_ctr;
      pid_ctr = pid_ctr + 1;
    } else {
      this.id = id;
    }
  };

  var REF = function REF() {
    _classCallCheck(this, REF);

    this.id = reference_ctr;
    reference_ctr = reference_ctr + 1;
  };

  var ProcessQueue = /*#__PURE__*/function () {
    function ProcessQueue(pid) {
      _classCallCheck(this, ProcessQueue);

      this.pid = pid;
      this.tasks = [];
    }

    _createClass(ProcessQueue, [{
      key: "empty",
      value: function empty() {
        return this.tasks.length === 0;
      }
    }, {
      key: "add",
      value: function add(task) {
        this.tasks.push(task);
      }
    }, {
      key: "next",
      value: function next() {
        return this.tasks.shift();
      }
    }]);

    return ProcessQueue;
  }();
  /**
   * Default scheduler for the process system.
   * Schedules process execution using setTimeout.
   * The most generic scheduler and maybe not good for
   * anything with dom manipulation.
   */


  var DefaultScheduler = /*#__PURE__*/function () {
    function DefaultScheduler() {
      var throttle = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
      var reductions_per_process = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 8;

      _classCallCheck(this, DefaultScheduler);

      this.isRunning = false;

      this.invokeLater = function (callback) {
        setTimeout(callback, throttle);
      }; // In our case a reduction is equal to a task call
      // Controls how many tasks are called at a time per process


      this.reductions_per_process = reductions_per_process;
      this.queues = new Map();
      console.log("SCHEDULER");
      console.log(this.isRunning);
      this.run();
    }

    _createClass(DefaultScheduler, [{
      key: "addToQueue",
      value: function addToQueue(pid, task) {
        if (!this.queues.has(pid)) {
          this.queues.set(pid, new ProcessQueue(pid));
        }

        var queue = this.queues.get(pid);

        if (queue) {
          queue.add(task);
        }
      }
    }, {
      key: "removePid",
      value: function removePid(pid) {
        this.isRunning = true;
        this.queues["delete"](pid);
        this.isRunning = false;
      }
    }, {
      key: "_run",
      value: function _run(run) {
        console.log("_run()");
        this.invokeLater(function () {
          run();
        });
      }
    }, {
      key: "run",
      value: function run() {
        console.log("run()");

        if (this.isRunning) {
          this._run(this.run.bind(this));
        } else {
          var _iterator = _createForOfIteratorHelper(this.queues),
              _step;

          try {
            for (_iterator.s(); !(_step = _iterator.n()).done;) {
              var _step$value = _slicedToArray(_step.value, 2),
                  pid = _step$value[0],
                  queue = _step$value[1];

              var reductions = 0;

              while (queue && !queue.empty() && reductions < this.reductions_per_process) {
                var task = queue.next();
                this.isRunning = true;
                var result = void 0;

                try {
                  if (task) {
                    result = task();
                  }
                } catch (e) {
                  console.error(e);
                  result = e;
                }

                this.isRunning = false;

                if (result instanceof Error) {
                  throw result;
                }

                reductions++;
              }
            }
          } catch (err) {
            _iterator.e(err);
          } finally {
            _iterator.f();
          }

          this._run(this.run.bind(this));
        }
      }
    }, {
      key: "addToScheduler",
      value: function addToScheduler(pid, task) {
        var _this = this;

        var dueTime = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;

        if (dueTime === 0) {
          this.invokeLater(function () {
            _this.addToQueue(pid, task);
          });
        } else {
          setTimeout(function () {
            _this.addToQueue(pid, task);
          }, dueTime);
        }
      }
    }, {
      key: "schedule",
      value: function schedule(pid, task) {
        this.addToScheduler(pid, function () {
          task();
        });
      }
    }, {
      key: "scheduleFuture",
      value: function scheduleFuture(pid, dueTime, task) {
        this.addToScheduler(pid, function () {
          task();
        }, dueTime);
      }
    }]);

    return DefaultScheduler;
  }();
  /**
   * Scheduler for the process system.
   * Uses window.requestAnimationFrame to schedule process execution
   * Good for processes that do a lot of dom manipulation
   */


  var RequestAnimationScheduler = /*#__PURE__*/function (_DefaultScheduler) {
    _inherits(RequestAnimationScheduler, _DefaultScheduler);

    var _super = _createSuper(RequestAnimationScheduler);

    function RequestAnimationScheduler() {
      var throttle = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
      var reductions_per_process = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 8;

      _classCallCheck(this, RequestAnimationScheduler);

      return _super.call(this, throttle, reductions_per_process);
    }

    _createClass(RequestAnimationScheduler, [{
      key: "_run",
      value: function _run(run) {
        window.requestAnimationFrame(run);
      }
    }]);

    return RequestAnimationScheduler;
  }(DefaultScheduler);
  /**
   * Manages a process's messages.
   * A message is anything sent to the process from another
   * process
   */


  var Mailbox = /*#__PURE__*/function () {
    function Mailbox() {
      _classCallCheck(this, Mailbox);

      this.messages = [];
    }

    _createClass(Mailbox, [{
      key: "deliver",
      value: function deliver(message) {
        this.messages.push(message);
        return message;
      }
    }, {
      key: "get",
      value: function get() {
        return this.messages;
      }
    }, {
      key: "isEmpty",
      value: function isEmpty() {
        return this.messages.length === 0;
      }
    }, {
      key: "removeAt",
      value: function removeAt(index) {
        this.messages.splice(index, 1);
      }
    }]);

    return Mailbox;
  }();

  function is_sleep(value) {
    return Array.isArray(value) && value[0] === process_state.SLEEP;
  }

  function is_receive(value) {
    return Array.isArray(value) && value[0] === process_state.RECEIVE;
  }

  function receive_timed_out(value) {
    return value[2] != null && value[2] < Date.now();
  }
  /**
   * A Process. Represents the basic atomic level of concurrency in the system
   */


  var Process = /*#__PURE__*/function () {
    function Process(system, func, args) {
      _classCallCheck(this, Process);

      // TODO: Add group leader OwO
      this.system = system;
      this.func = func;
      this.args = args;
      this.status = process_state.STOPPED;
      this.pid = new PID();
      this.mailbox = new Mailbox();
      this.dict = new Map();
      this.flags = new Map();
      this.monitors = [];
    }

    _createClass(Process, [{
      key: "start",
      value: function start() {
        console.log("Process start");
        var function_scope = this;
        var machine = this.main();
        this.system.schedule(function () {
          function_scope.system.set_current(function_scope.pid);
          function_scope.run(machine, machine.next());
        }, this.pid);
      }
    }, {
      key: "main",
      value: /*#__PURE__*/regeneratorRuntime.mark(function main() {
        var retval;
        return regeneratorRuntime.wrap(function main$(_context) {
          while (1) {
            switch (_context.prev = _context.next) {
              case 0:
                retval = process_state.NORMAL;
                _context.prev = 1;
                return _context.delegateYield(this.func.apply(null, this.args), "t0", 3);

              case 3:
                _context.next = 9;
                break;

              case 5:
                _context.prev = 5;
                _context.t1 = _context["catch"](1);
                console.error(_context.t1);
                retval = _context.t1;

              case 9:
                this.system.exit(retval);

              case 10:
              case "end":
                return _context.stop();
            }
          }
        }, main, this, [[1, 5]]);
      })
    }, {
      key: "process_flag",
      value: function process_flag(flag, value) {
        var old_value = this.flags.get(flag);
        this.flags.set(flag, value);
        return old_value;
      }
    }, {
      key: "is_trapping_exits",
      value: function is_trapping_exits() {
        return this.flags.has(Symbol["for"]('trap_exit')) && this.flags.get(Symbol["for"]('trap_exit')) == true;
      }
    }, {
      key: "signal",
      value: function signal(reason) {
        if (reason !== process_state.NORMAL) {
          console.error(reason);
        }

        this.system.remove_proc(this.pid, reason);
      }
    }, {
      key: "receive",
      value: function receive(fun) {
        console.log("RECEIVE");
        var value = process_state.NOMATCH;
        var messages = this.mailbox.get();

        for (var i = 0; i < messages.length; i++) {
          try {
            value = fun(messages[i]);

            if (value !== process_state.NOMATCH) {
              this.mailbox.removeAt(i);
              break;
            }
          } catch (e) {
            if (e.constructor.name != 'MatchError') {
              this.system.exit(e);
            }
          }
        }

        return value;
      }
    }, {
      key: "run",
      value: function run(machine, step) {
        console.log("Process run");
        var function_scope = this;

        if (!step.done) {
          var value = step.value;

          if (is_sleep(value)) {
            this.system.delay(function () {
              function_scope.system.set_current(function_scope.pid);
              function_scope.run(machine, machine.next());
            }, value[1]);
          } else if (is_receive(value) && receive_timed_out(value)) {
            var result = value[3]();
            this.system.schedule(function () {
              function_scope.system.set_current(function_scope.pid);
              function_scope.run(machine, machine.next(result));
            });
          } else if (is_receive(value)) {
            var _result = function_scope.receive(value[1]);

            if (_result === process_state.NOMATCH) {
              this.system.suspend(function () {
                function_scope.system.set_current(function_scope.pid);
                function_scope.run(machine, step);
              });
            } else {
              this.system.schedule(function () {
                function_scope.system.set_current(function_scope.pid);
                function_scope.run(machine, machine.next(_result));
              });
            }
          } else {
            this.system.schedule(function () {
              function_scope.system.set_current(function_scope.pid);
              function_scope.run(machine, machine.next(value));
            });
          }
        }
      }
    }]);

    return Process;
  }();
  /**
   * Manages all of the processes.
   */


  var ProcessSystem = /*#__PURE__*/function () {
    function ProcessSystem() {
      var scheduler = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : new DefaultScheduler(5);

      _classCallCheck(this, ProcessSystem);

      this.pids = new Map();
      this.mailboxes = new Map();
      this.names = new Map();
      this.links = new Map();
      this.monitors = new Map();
      this.current_process = null;
      this.scheduler = scheduler;
      this.suspended = new Map();
      var process_system_scope = this;
      this.main_process_pid = this.spawn( /*#__PURE__*/regeneratorRuntime.mark(function _callee() {
        return regeneratorRuntime.wrap(function _callee$(_context2) {
          while (1) {
            switch (_context2.prev = _context2.next) {
              case 0:
                _context2.next = 2;
                return process_system_scope.sleep(Symbol["for"]('Infinity'));

              case 2:
              case "end":
                return _context2.stop();
            }
          }
        }, _callee);
      }));
      this.set_current(this.main_process_pid);
    }

    _createClass(ProcessSystem, [{
      key: "spawn",

      /**
       * Starts a process represented by the given generator function
       * @param args Either a generator function or a module, function and arguments
       */
      value: function spawn() {
        console.log("SPAAAWN");

        if (arguments.length === 1) {
          var fun = arguments.length <= 0 ? undefined : arguments[0];
          return this.add_proc(fun, [], false, false).pid;
        } else {
          var mod = arguments.length <= 0 ? undefined : arguments[0];

          var _fun = arguments.length <= 1 ? undefined : arguments[1];

          var the_args = arguments.length <= 2 ? undefined : arguments[2];
          return this.add_proc(mod[_fun], the_args, false, false).pid;
        }
      }
      /**
       * Starts a process using the generator function from the specified module
       * @param args Either a generator function or a module, function and arguments
       */

    }, {
      key: "spawn_link",
      value: function spawn_link() {
        if (arguments.length === 1) {
          var fun = arguments.length <= 0 ? undefined : arguments[0];
          return this.add_proc(fun, [], true, false).pid;
        } else {
          var mod = arguments.length <= 0 ? undefined : arguments[0];

          var _fun2 = arguments.length <= 1 ? undefined : arguments[1];

          var the_args = arguments.length <= 2 ? undefined : arguments[2];
          return this.add_proc(mod[_fun2], the_args, true, false).pid;
        }
      }
      /**
       * links the current process with the process from the given pid
       * @param pid pid of the process to link to
       */

    }, {
      key: "link",
      value: function link(pid) {
        var currentProcessPid = this.pid();

        if (currentProcessPid != null) {
          var currentProcessLink = this.links.get(currentProcessPid.id);
          var IncomingProcessLink = this.links.get(pid.id);

          if (currentProcessLink && IncomingProcessLink) {
            currentProcessLink.add(pid);
            IncomingProcessLink.add(currentProcessPid);
          }
        }
      }
      /**
       * unlinks the current process from the process from the given pid
       * @param pid pid of the process to link to
       */

    }, {
      key: "unlink",
      value: function unlink(pid) {
        var currentProcessPid = this.pid();

        if (currentProcessPid != null) {
          var currentProcessLink = this.links.get(currentProcessPid.id);
          var IncomingProcessLink = this.links.get(pid.id);

          if (currentProcessLink && IncomingProcessLink) {
            currentProcessLink["delete"](pid);
            IncomingProcessLink["delete"](currentProcessPid);
          }
        }
      }
      /**
       * Spawns a process and then monitors it
       * @param args Either a generator function or a module, function and arguments
       */

    }, {
      key: "spawn_monitor",
      value: function spawn_monitor() {
        if (arguments.length === 1) {
          var fun = arguments.length <= 0 ? undefined : arguments[0];
          var process = this.add_proc(fun, [], false, true);
          return [process.pid, process.monitors[0]];
        } else {
          var mod = arguments.length <= 0 ? undefined : arguments[0];

          var _fun3 = arguments.length <= 1 ? undefined : arguments[1];

          var the_args = arguments.length <= 2 ? undefined : arguments[2];

          var _process = this.add_proc(mod[_fun3], the_args, false, true);

          return [_process.pid, _process.monitors[0]];
        }
      }
      /**
       * Monitors the given process
       * @param pid pid of the process to link to
       */

    }, {
      key: "monitor",
      value: function monitor(pid) {
        var real_pid = this.pidof(pid);
        var ref = this.make_ref();

        if (this.currentProcess != null) {
          if (real_pid) {
            this.monitors.set(ref, {
              monitor: this.currentProcess.pid,
              monitee: real_pid
            });
            var process = this.pids.get(real_pid.id);

            if (process) {
              process.monitors.push(ref);
            }

            return ref;
          } else {
            this.send(this.currentProcess.pid, ['unboxed', ['DOWN', ref, pid, real_pid, 'noproc']]);
            return ref;
          }
        }

        return null;
      }
      /**
       * Removes the monitor
       * @param ref Reference to monitor
       */

    }, {
      key: "demonitor",
      value: function demonitor(ref) {
        if (this.monitors.has(ref)) {
          this.monitors["delete"](ref);
          return true;
        }

        return false;
      }
      /**
       * Sets the current process
       * @param id PID or name of process
       */

    }, {
      key: "set_current",
      value: function set_current(id) {
        var pid = this.pidof(id);

        if (pid) {
          var next = this.pids.get(pid.id);

          if (next) {
            this.current_process = next;

            if (this.currentProcess) {
              this.currentProcess.status = process_state.RUNNING;
            }
          }
        }
      }
    }, {
      key: "add_proc",
      value: function add_proc(fun, args, linked, monitored) {
        var newproc = new Process(this, fun, args);
        this.pids.set(newproc.pid.id, newproc);
        this.mailboxes.set(newproc.pid.id, newproc.mailbox);
        this.links.set(newproc.pid.id, new Set());

        if (linked) {
          this.link(newproc.pid);
        }

        if (monitored) {
          this.monitor(newproc.pid);
        }

        newproc.start();
        return newproc;
      }
    }, {
      key: "remove_proc",
      value: function remove_proc(pid, exitreason) {
        this.pids["delete"](pid.id);
        this.unregister(pid);
        this.scheduler.removePid(pid);
        var linkedPids = this.links.get(pid.id);

        if (linkedPids) {
          var _iterator2 = _createForOfIteratorHelper(linkedPids),
              _step2;

          try {
            for (_iterator2.s(); !(_step2 = _iterator2.n()).done;) {
              var linkpid = _step2.value;
              this.exit(linkpid, exitreason);
              var linkedPid = this.links.get(linkpid.id);

              if (linkedPid) {
                linkedPid["delete"](pid);
              }
            }
          } catch (err) {
            _iterator2.e(err);
          } finally {
            _iterator2.f();
          }

          this.links["delete"](pid.id);
        }
      }
      /**
       * registers the given name to the pid
       * @param name The name to give the process
       * @param pid The pid of the process
       */

    }, {
      key: "register",
      value: function register(name, pid) {
        if (!this.names.has(name)) {
          this.names.set(name, pid);
        } else {
          throw new Error('Name is already registered to another process');
        }
      }
      /**
       * Finds a process by the given name
       * @param name the name of the process
       */

    }, {
      key: "whereis",
      value: function whereis(name) {
        return this.names.has(name) ? this.names.get(name) : null;
      }
      /**
       * returns the liast of names that are registered
       */

    }, {
      key: "registered",
      value: function registered() {
        return this.names.keys();
      }
      /**
       * unregisters the names associated with the pid
       * @param pid The pid of the process
       */

    }, {
      key: "unregister",
      value: function unregister(pid) {
        var _iterator3 = _createForOfIteratorHelper(this.names.keys()),
            _step3;

        try {
          for (_iterator3.s(); !(_step3 = _iterator3.n()).done;) {
            var name = _step3.value;

            if (this.names.has(name) && this.names.get(name) === pid) {
              this.names["delete"](name);
            }
          }
        } catch (err) {
          _iterator3.e(err);
        } finally {
          _iterator3.f();
        }
      }
      /**
       * Returns the PID of the current process
       */

    }, {
      key: "pid",
      value: function pid() {
        if (this.currentProcess) {
          return this.currentProcess.pid;
        }

        return null;
      }
      /**
       * takes the input and tries to find the pid. Input can be a `pid`, `Process`, or name the pid is associated with
       * @param id The registered name or pid of the process
       */

    }, {
      key: "pidof",
      value: function pidof(id) {
        if (id instanceof PID) {
          return this.pids.has(id.id) ? id : null;
        } else if (id instanceof Process) {
          return id.pid;
        } else {
          var pid = this.whereis(id);
          if (pid === null) throw 'Process name not registered: ' + id + ' (' + _typeof(id) + ')';
          return pid;
        }
      }
      /**
       * sends a message the the process represented by the pid
       * @param id
       * @param msg
       */

    }, {
      key: "send",
      value: function send(id, msg) {
        var pid = this.pidof(id);

        if (pid) {
          var mailbox = this.mailboxes.get(pid.id);

          if (mailbox) {
            mailbox.deliver(msg);
          }

          if (this.suspended.has(pid)) {
            var fun = this.suspended.get(pid);
            this.suspended["delete"](pid);

            if (fun) {
              this.schedule(fun);
            }
          }
        }

        return msg;
      }
      /**
       * Tells the current process to receive a message that the function can handle.
       * If no match then the process is put in the suspended state until a message arrives
       * or the timeout is reached.
       * If the timeout is reached and no msg matches, then the timeoutFn is called
       * @param fun
       * @param timeout
       * @param timeoutFn
       */

    }, {
      key: "receive",
      value: function receive(fun) {
        var timeout = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
        var timeoutFn = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : function () {
          return true;
        };
        var DateTimeout = null;

        if (timeout === 0 || timeout === Infinity) {
          DateTimeout = null;
        } else {
          DateTimeout = Date.now() + timeout;
        }

        return [process_state.RECEIVE, fun, DateTimeout, timeoutFn];
      }
      /**
       * puts the current process to sleep
       * @param duration
       */

    }, {
      key: "sleep",
      value: function sleep(duration) {
        return [process_state.SLEEP, duration];
      }
      /**
       * Suspends the current process
       * @param fun
       */

    }, {
      key: "suspend",
      value: function suspend(fun) {
        if (this.currentProcess) {
          this.currentProcess.status = process_state.SUSPENDED;
          this.suspended.set(this.currentProcess.pid, fun);
        }
      }
      /**
       * Makes current process go to sleep
       * @param fun
       * @param time
       */

    }, {
      key: "delay",
      value: function delay(fun, time) {
        if (this.currentProcess) {
          this.currentProcess.status = process_state.SLEEPING;

          if (Number.isInteger(time)) {
            this.scheduler.scheduleFuture(this.currentProcess.pid, time, fun);
          }
        }
      }
      /**
       * Schedules execution of a process reduction
       * @param fun
       * @param pid
       */

    }, {
      key: "schedule",
      value: function schedule(fun, pid) {
        if (this.currentProcess) {
          var the_pid = pid != null ? pid : this.currentProcess.pid;
          this.scheduler.schedule(the_pid, fun);
        }
      }
      /**
       * terminates the current process with the given reason.
       * @param one
       * @param two
       */

    }, {
      key: "exit",
      value: function exit(one, two) {
        var pid = null;
        var reason = null;
        var process = null;

        if (two) {
          pid = one;
          reason = two;
          var thePid = this.pidof(pid);

          if (thePid) {
            process = this.pids.get(thePid.id);
          }

          if (process) {
            if (process.is_trapping_exits() || reason === process_state.KILL || reason === process_state.NORMAL) {
              var mailbox = this.mailboxes.get(process.pid.id);

              if (mailbox) {
                mailbox.deliver(['unboxed', [process_state.EXIT, this.pid(), reason]]);
              }
            } else {
              process.signal(reason);
            }
          }
        } else {
          if (this.currentProcess) {
            pid = this.currentProcess.pid;
            reason = one;
            process = this.currentProcess;
            process.signal(reason);
          }
        }

        if (process) {
          var _iterator4 = _createForOfIteratorHelper(process.monitors),
              _step4;

          try {
            for (_iterator4.s(); !(_step4 = _iterator4.n()).done;) {
              var ref = _step4.value;
              var mons = this.monitors.get(ref);

              if (mons) {
                this.send(mons['monitor'], ['unboxed', ['DOWN', ref, mons['monitee'], mons['monitee'], reason]]);
              }
            }
          } catch (err) {
            _iterator4.e(err);
          } finally {
            _iterator4.f();
          }
        }
      }
      /**
       * terminates the current process with an error
       * @param reason
       */

    }, {
      key: "error",
      value: function error(reason) {
        if (this.currentProcess) {
          this.currentProcess.signal(reason);
        }
      }
      /**
       * Sets flags on the current process.
        - Note: the only flag respected is the `Symbol.for("trap_exit")` flag.
        If value is `true`, then exit signals from linked processes are turned into
        messages and sent to the current processes mailbox.
        If value is `false`, the exit is treated as normal and terminates the process.
        Setting it to `true` is useful for supervising processes.
       * @param args
       */

    }, {
      key: "process_flag",
      value: function process_flag() {
        if (arguments.length == 2) {
          var flag = arguments.length <= 0 ? undefined : arguments[0];
          var value = arguments.length <= 1 ? undefined : arguments[1];

          if (this.currentProcess) {
            return this.currentProcess.process_flag(flag, value);
          }
        } else {
          var pid = this.pidof(arguments.length <= 0 ? undefined : arguments[0]);

          if (pid) {
            var _flag = arguments.length <= 1 ? undefined : arguments[1];

            var _value = arguments.length <= 2 ? undefined : arguments[2];

            var process = this.pids.get(pid.id);

            if (process) {
              return process.process_flag(_flag, _value);
            }
          }
        }
      }
      /**
       * Adds a value to the current process's dictionary
       * @param key
       * @param value
       */

    }, {
      key: "put",
      value: function put(key, value) {
        if (this.currentProcess) {
          this.currentProcess.dict.set(key, value);
        }
      }
      /**
       * Gets the current process's dictionary
       */

    }, {
      key: "get_process_dict",
      value: function get_process_dict() {
        if (this.currentProcess) {
          return this.currentProcess.dict;
        }

        throw new Error('No Current Process');
      }
      /**
       * Gets a value from the current process's dictionary or the default if key not in dictionary
       * @param key
       * @param default_value
       */

    }, {
      key: "get",
      value: function get(key) {
        var default_value = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : null;

        if (this.currentProcess && key in this.currentProcess.dict) {
          return this.currentProcess.dict.get(key);
        } else {
          return default_value;
        }
      }
      /**
       * Gets all the keys from the current process's dictionary
       * @param value
       */

    }, {
      key: "get_keys",
      value: function get_keys(value) {
        if (value) {
          var keys = [];

          if (this.currentProcess) {
            for (var _i2 = 0, _Object$keys = Object.keys(this.currentProcess.dict); _i2 < _Object$keys.length; _i2++) {
              var key = _Object$keys[_i2];

              if (this.currentProcess.dict.get(key) === value) {
                keys.push(key);
              }
            }
          }

          return keys;
        }

        if (this.currentProcess) {
          return Object.keys(this.currentProcess.dict);
        }

        throw new Error('No Current Process');
      }
      /**
       * Removes the key and the associated value from the current process's dictionary
       *
       * If no key is given, removes all entries from the current process's dictionary
       * @param key the key to remove
       */

    }, {
      key: "erase",
      value: function erase(key) {
        if (this.currentProcess) {
          if (key != null && this.currentProcess.dict.has(key)) {
            this.currentProcess.dict["delete"](key);
          } else {
            this.currentProcess.dict = new Map();
          }
        }
      }
      /**
       * Returns if the given pid is alive
       * @param pid
       */

    }, {
      key: "is_alive",
      value: function is_alive(pid) {
        var real_pid = this.pidof(pid);
        return real_pid != null;
      }
      /**
       * Returns a list of all the pids
       */

    }, {
      key: "list",
      value: function list() {
        return Array.from(this.pids.keys());
      }
      /**
       * Returns a unique reference
       */

    }, {
      key: "make_ref",
      value: function make_ref() {
        return new REF();
      }
    }, {
      key: "currentProcess",
      get: function get() {
        if (this.current_process) {
          return this.current_process;
        }

        return null;
      }
    }], [{
      key: "run",
      value: /*#__PURE__*/regeneratorRuntime.mark(function run(fun, args) {
        var context,
            _args3 = arguments;
        return regeneratorRuntime.wrap(function run$(_context3) {
          while (1) {
            switch (_context3.prev = _context3.next) {
              case 0:
                context = _args3.length > 2 && _args3[2] !== undefined ? _args3[2] : null;

                if (!(fun.constructor.name === 'GeneratorFunction')) {
                  _context3.next = 6;
                  break;
                }

                return _context3.delegateYield(fun.apply(context, args), "t0", 3);

              case 3:
                return _context3.abrupt("return", _context3.t0);

              case 6:
                _context3.next = 8;
                return fun.apply(context, args);

              case 8:
                return _context3.abrupt("return", _context3.sent);

              case 9:
              case "end":
                return _context3.stop();
            }
          }
        }, run);
      })
    }]);

    return ProcessSystem;
  }();

  var system = new ProcessSystem();
  var loaded_code = new Map();

  function atom_to_function_name(name, arity) {
    return "erlps__" + name + "__" + arity;
  }

  function atom_to_module_name(name) {
    return name.split('_').map(function (x) {
      return x.charAt(0).toUpperCase() + x.slice(1);
    }).join('.');
  }
  /* FFI CODE */


  function do_apply_4(moduleName) {
    return function (functionName) {
      return function (argumentArray) {
        return function (failCallback) {
          //FIXME: BIFS
          var mName = atom_to_module_name(moduleName);
          var fName = atom_to_function_name(functionName, argumentArray.length);
          return do_ffi_remote_fun_call(mName)(fName)(argumentArray)(failCallback);
        };
      };
    };
  }

  ;

  function do_function_exported_3(moduleName) {
    return function (functionName) {
      return function (arity) {
        // FIXME: BIFS
        var mName = atom_to_module_name(moduleName);
        var fName = atom_to_function_name(functionName, arity);
        var module = loaded_code.get(mName);

        if (module) {
          var f = module[fName];
          if (f) return true;
          return false;
        }

        return false;
      };
    };
  }

  function do_onload(name, module) {
    var f = module["onload"];

    if (f) {
      f();
    }
    /* Very very very dirty JS hack... */


    if (name == "Maps") {
      var bifs = get_erlang_module();
      module["erlps__get__2"] = bifs["maps__get__2"];
      module["erlps__find__2"] = bifs["maps__find__2"];
      module["erlps__from_list__1"] = bifs["maps__from_list__1"];
      module["erlps__is_key__2"] = bifs["maps__is_key__2"];
      module["erlps__keys__1"] = bifs["maps__keys__1"];
      module["erlps__merge__2"] = bifs["maps__merge__2"];
      module["erlps__put__3"] = bifs["maps__put__3"];
      module["erlps__remove__2"] = bifs["maps__remove__2"];
      module["erlps__take__2"] = bifs["maps__take__2"];
      module["erlps__to_list__1"] = bifs["maps__to_list__1"];
      module["erlps__update__3"] = bifs["maps__update__3"];
      module["erlps__values__1"] = bifs["maps__values__1"];
    } else if (name == "Lists") {
      var bifs = get_erlang_module();
      module["erlps__keyfind__3"] = bifs["lists__keyfind__3"];
      module["erlps__keymember__3"] = bifs["lists__keymember__3"];
      module["erlps__keysearch__3"] = bifs["lists__keysearch__3"];
      module["erlps__member__2"] = bifs["lists__member__2"];
      module["erlps__reverse__2"] = bifs["lists__reverse__2"];
    } else if (name == "Math") {
      var bifs = get_erlang_module(); // TODO
    } else if (name == "Prim.Eval") {
      var bifs = get_erlang_module();
      module["erlps__prim_eval__3"] = bifs["prim_eval__receive__2"];
    } else if (name == "Erts.Internal") {
      var bifs = get_erlang_module();
      module["erlps__map_next__3"] = bifs["erts_internal__map_next__3"];
    }

    return module;
  }

  function module_resolve(name) {
    try {
      return PS[name];
    } catch (e) {
      try {
        return require("../" + name + "/index.js");
      } catch (e) {
        return undefined;
      }
    }
  }

  var bif_module = undefined;

  function get_erlang_module() {
    if (bif_module === undefined) bif_module = module_resolve("Erlang.Builtins");
    return bif_module;
  }

  function do_ffi_remote_fun_call(moduleName) {
    return function (functionName) {
      return function (argumentArray) {
        return function (undefCallback) {
          var module = loaded_code.get(moduleName);

          if (module === undefined) {
            module = module_resolve(moduleName);

            if (module) {
              module = do_onload(moduleName, module);
              loaded_code.set(moduleName, module);
            } else {
              return undefCallback();
            }
          }

          if (module) {
            var f = module[functionName];

            if (f) {
              return f(argumentArray);
            } else {
              return undefCallback();
            }
          }
        };
      };
    };
  }

  function do_spawn_1(action) {
    return function (pid_ctr) {
      var pid = system.spawn( /*#__PURE__*/regeneratorRuntime.mark(function _callee2() {
        return regeneratorRuntime.wrap(function _callee2$(_context4) {
          while (1) {
            switch (_context4.prev = _context4.next) {
              case 0:
                _context4.prev = 0;
                return _context4.abrupt("return", action());

              case 4:
                _context4.prev = 4;
                _context4.t0 = _context4["catch"](0);
                console.log("AAAAAA");

              case 7:
              case "end":
                return _context4.stop();
            }
          }
        }, _callee2, null, [[0, 4]]);
      }));
      return pid_ctr(pid.id);
    };
  }

  function do_is_process_alive_1(id) {
    return system.is_alive(new PID(id));
  }

  function do_self_0(pid_ctr) {
    var pid = system.pid();
    return pid_ctr(pid.id);
  }

  function do_send_2(pid_id) {
    return function (term) {
      console.log("SEND");
      console.log(term);
      return term;
    };
  }

  function do_receive_2(match_fun) {
    return function (timeout_val) {
      return function (atom_ctr) {
        console.log(arguments);
        console.log("RRRRR");
        console.log(match_fun);
        console.log(timeout_val);

        if (timeout_val < 0) {
          timeout_val = 0;
        } else if (timeout_val == 0) {
          timeout_val = 1;
        }

        console.log("Calling receive"); // TIME FOR A VERY DIRTY HACK :(

        console.log( /*#__PURE__*/regeneratorRuntime.mark(function _callee3() {
          return regeneratorRuntime.wrap(function _callee3$(_context5) {
            while (1) {
              switch (_context5.prev = _context5.next) {
                case 0:
                  _context5.next = 2;
                  return system.receive(function (msg) {
                    console.log("RECEIVED");
                  });

                case 2:
                case "end":
                  return _context5.stop();
              }
            }
          }, _callee3);
        })().next());
      };
    };
  }

  function do_make_ref_0(ref_ctr) {
    return ref_ctr(system.make_ref().id);
  }

  function do_put_2(cmp) {
    return function (key) {
      return function (value) {
        return function (on_no_key) {
          var dict = system.get_process_dict();

          var _iterator5 = _createForOfIteratorHelper(dict),
              _step5;

          try {
            for (_iterator5.s(); !(_step5 = _iterator5.n()).done;) {
              var dpair = _step5.value;
              var dkey = dpair[0];
              var dval = dpair[1];

              if (cmp(dkey)(key)) {
                system.put(dkey, value);
                return dval;
              }
            }
          } catch (err) {
            _iterator5.e(err);
          } finally {
            _iterator5.f();
          }

          system.put(key, value);
          return on_no_key;
        };
      };
    };
  }

  function do_get_1(cmp) {
    return function (key) {
      return function (on_no_key) {
        var dict = system.get_process_dict();

        var _iterator6 = _createForOfIteratorHelper(dict),
            _step6;

        try {
          for (_iterator6.s(); !(_step6 = _iterator6.n()).done;) {
            var dpair = _step6.value;
            var dkey = dpair[0];
            var dval = dpair[1];

            if (cmp(dkey)(key)) {
              return dval;
            }
          }
        } catch (err) {
          _iterator6.e(err);
        } finally {
          _iterator6.f();
        }

        return on_no_key;
      };
    };
  }

  function do_get_0(wrap) {
    var dict = system.get_process_dict();
    var res = [];

    var _iterator7 = _createForOfIteratorHelper(dict),
        _step7;

    try {
      for (_iterator7.s(); !(_step7 = _iterator7.n()).done;) {
        var dpair = _step7.value;
        var dkey = dpair[0];
        var dval = dpair[1];
        res.push(wrap(dkey)(dval));
      }
    } catch (err) {
      _iterator7.e(err);
    } finally {
      _iterator7.f();
    }

    return res;
  }

  function do_get_keys_0(x) {
    var dict = system.get_process_dict();
    var res = [];

    var _iterator8 = _createForOfIteratorHelper(dict),
        _step8;

    try {
      for (_iterator8.s(); !(_step8 = _iterator8.n()).done;) {
        var dpair = _step8.value;
        var dkey = dpair[0];
        res.push(dkey);
      }
    } catch (err) {
      _iterator8.e(err);
    } finally {
      _iterator8.f();
    }

    return res;
  }

  function do_get_keys_1(cmp) {
    return function (val) {
      var dict = system.get_process_dict();
      var res = [];

      var _iterator9 = _createForOfIteratorHelper(dict),
          _step9;

      try {
        for (_iterator9.s(); !(_step9 = _iterator9.n()).done;) {
          var dpair = _step9.value;
          var dkey = dpair[0];
          var dval = dpair[1];

          if (cmp(dval)(val)) {
            res.push(dkey);
          }
        }
      } catch (err) {
        _iterator9.e(err);
      } finally {
        _iterator9.f();
      }

      return res;
    };
  }

  function do_erase_0(wrap) {
    var res = do_get_0(wrap);
    system.erase();
    return res;
  }

  function do_erase_1(cmp) {
    return function (key) {
      return function (on_no_key) {
        var dict = system.get_process_dict();

        var _iterator10 = _createForOfIteratorHelper(dict),
            _step10;

        try {
          for (_iterator10.s(); !(_step10 = _iterator10.n()).done;) {
            var dpair = _step10.value;
            var dkey = dpair[0];
            var dval = dpair[1];

            if (cmp(dkey)(key)) {
              system.erase(dkey);
              return dval;
            }
          }
        } catch (err) {
          _iterator10.e(err);
        } finally {
          _iterator10.f();
        }

        return on_no_key;
      };
    };
  }

  return {
    do_ffi_remote_fun_call: do_ffi_remote_fun_call,
    do_make_ref_0: do_make_ref_0,
    do_receive_2: do_receive_2,
    do_send_2: do_send_2,
    do_self_0: do_self_0,
    do_is_process_alive_1: do_is_process_alive_1,
    do_spawn_1: do_spawn_1,
    do_apply_4: do_apply_4,
    do_put_2: do_put_2,
    do_get_1: do_get_1,
    do_get_0: do_get_0,
    do_get_keys_0: do_get_keys_0,
    do_get_keys_1: do_get_keys_1,
    do_erase_0: do_erase_0,
    do_erase_1: do_erase_1,
    do_function_exported_3: do_function_exported_3,
    system: system
  };
}();

exports.do_ffi_remote_fun_call = RUNTIME.do_ffi_remote_fun_call;
exports.system = RUNTIME.system;
exports.do_make_ref_0 = RUNTIME.do_make_ref_0;
exports.do_receive_2 = RUNTIME.do_receive_2;
exports.do_send_2 = RUNTIME.do_send_2;
exports.do_self_0 = RUNTIME.do_self_0;
exports.do_is_process_alive_1 = RUNTIME.do_is_process_alive_1;
exports.do_spawn_1 = RUNTIME.do_spawn_1;
exports.do_apply_4 = RUNTIME.do_apply_4;
exports.do_put_2 = RUNTIME.do_put_2;
exports.do_get_1 = RUNTIME.do_get_1;
exports.do_get_0 = RUNTIME.do_get_0;
exports.do_get_keys_0 = RUNTIME.do_get_keys_0;
exports.do_get_keys_1 = RUNTIME.do_get_keys_1;
exports.do_erase_0 = RUNTIME.do_erase_0;
exports.do_erase_1 = RUNTIME.do_erase_1;
exports.do_function_exported_3 = RUNTIME.do_function_exported_3; // Missing math functions
//--------------------------------------------------------------------------------

exports.acosh = Math.acosh || function (x) {
  return Math.log(x + Math.sqrt(x * x - 1));
};

exports.asinh = Math.asinh || function (x) {
  if (x === -Infinity) {
    return x;
  } else {
    return Math.log(x + Math.sqrt(x * x + 1));
  }
};

exports.atanh = Math.atanh || function (x) {
  return Math.log((1 + x) / (1 - x)) / 2;
};

exports.cbrt = Math.cbrt || function (x) {
  if (x === 0) {
    return x; // +0 or -0
  } else if (x < 0) {
    return -Math.pow(-x, 1 / 3);
  } else {
    return Math.pow(x, 1 / 3);
  }
};

exports.clz32 = Math.clz32 || function (x) {
  if (x === 0) {
    return 32;
  }

  return 31 - Math.floor(Math.log(x >>> 0) * Math.LOG2E);
};

exports.cosh = Math.cosh || function (x) {
  return (Math.exp(x) + Math.exp(-x)) / 2;
};

exports.expm1 = Math.expm1 || function (x) {
  return Math.exp(x) - 1;
};

exports.hypot = Math.hypot ? function (x) {
  return function (y) {
    return Math.hypot(x, y);
  };
} : function (x) {
  return function (y) {
    return Math.sqrt(x * x + y * y);
  };
};
exports.hypot3 = Math.hypot ? function (x) {
  return function (y) {
    return function (z) {
      return Math.hypot(x, y, z);
    };
  };
} : function (x) {
  return function (y) {
    return function (z) {
      return Math.sqrt(x * x + y * y + z * z);
    };
  };
};

exports.log1p = Math.log1p || function (x) {
  return Math.log(x + 1);
};

exports.log10 = Math.log10 || function (x) {
  return Math.log(x) * Math.LOG10E;
};

exports.log2 = Math.log2 || function (x) {
  return Math.log(x) * Math.LOG2E;
};

exports.sign = Math.sign || function (x) {
  if (x > 0) {
    return 1;
  } else if (x < 0) {
    return -1;
  } else {
    return +x; // +0 or -0 or NaN
  }
};

exports.sinh = Math.sinh || function (x) {
  return (Math.exp(x) - Math.exp(-x)) / 2;
};

exports.tanh = Math.tanh || function (x) {
  var ex = Math.exp(2 * x);

  if (ex === Infinity) {
    return 1;
  } else {
    return (ex - 1) / (ex + 1);
  }
};

