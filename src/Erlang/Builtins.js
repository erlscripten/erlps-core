"use strict";

var RUNTIME = function(){

var pid_ctr = 0;
var reference_ctr = 0;

const process_state = {
    NORMAL: Symbol.for('normal'),
    KILL: Symbol.for('kill'),
    SUSPEND: Symbol.for('suspend'),
    CONTINUE: Symbol.for('continue'),
    RECEIVE: Symbol.for('receive'),
    SEND: Symbol.for('send'),
    SLEEPING: Symbol.for('sleeping'),
    RUNNING: Symbol.for('running'),
    SUSPENDED: Symbol.for('suspended'),
    STOPPED: Symbol.for('stopped'),
    SLEEP: Symbol.for('sleep'),
    EXIT: Symbol.for('exit'),
    NOMATCH: Symbol.for('no_match'),
};

class PID {
    constructor(id) {
        if(id == undefined) {
            this.id = pid_ctr;
            pid_ctr = pid_ctr + 1;
        } else {
           this.id = id;
        }
    }
}

class REF {
    constructor() {
        this.id = reference_ctr;
        reference_ctr = reference_ctr + 1;
    }
}

class ProcessQueue {
    constructor(pid) {
        this.pid = pid;
        this.tasks = [];
    }
    empty() {
        return this.tasks.length === 0;
    }
    add(task) {
        this.tasks.push(task);
    }
    next() {
        return this.tasks.shift();
    }
}

/**
 * Default scheduler for the process system.
 * Schedules process execution using setTimeout.
 * The most generic scheduler and maybe not good for
 * anything with dom manipulation.
 */
class DefaultScheduler {
    constructor(throttle = 0, reductions_per_process = 8) {
        this.isRunning = false;
        this.invokeLater = function (callback) {
            setTimeout(callback, throttle);
        };
        // In our case a reduction is equal to a task call
        // Controls how many tasks are called at a time per process
        this.reductions_per_process = reductions_per_process;
        this.queues = new Map();
        this.run();
    }
    addToQueue(pid, task) {
        if (!this.queues.has(pid)) {
            this.queues.set(pid, new ProcessQueue(pid));
        }
        const queue = this.queues.get(pid);
        if (queue) {
            queue.add(task);
        }
    }
    removePid(pid) {
        this.isRunning = true;
        this.queues.delete(pid);
        this.isRunning = false;
    }
    _run(run) {
        this.invokeLater(() => {
            run();
        });
    }
    run() {
        if (this.isRunning) {
            this._run(this.run.bind(this));
        }
        else {
            for (let [pid, queue] of this.queues) {
                let reductions = 0;
                while (queue &&
                    !queue.empty() &&
                    reductions < this.reductions_per_process) {
                    let task = queue.next();
                    this.isRunning = true;
                    let result;
                    try {
                        if (task) {
                            result = task();
                        }
                    }
                    catch (e) {
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
            this._run(this.run.bind(this));
        }
    }
    addToScheduler(pid, task, dueTime = 0) {
        if (dueTime === 0) {
            this.invokeLater(() => {
                this.addToQueue(pid, task);
            });
        }
        else {
            setTimeout(() => {
                this.addToQueue(pid, task);
            }, dueTime);
        }
    }
    schedule(pid, task) {
        this.addToScheduler(pid, () => {
            task();
        });
    }
    scheduleFuture(pid, dueTime, task) {
        this.addToScheduler(pid, () => {
            task();
        }, dueTime);
    }
}

/**
 * Scheduler for the process system.
 * Uses window.requestAnimationFrame to schedule process execution
 * Good for processes that do a lot of dom manipulation
 */
class RequestAnimationScheduler extends DefaultScheduler {
    constructor(throttle = 0, reductions_per_process = 8) {
        super(throttle, reductions_per_process);
    }
    _run(run) {
        window.requestAnimationFrame(run);
    }
}

/**
 * Manages a process's messages.
 * A message is anything sent to the process from another
 * process
 */
class Mailbox {
    constructor() {
        this.messages = [];
    }
    deliver(message) {
        this.messages.push(message);
        return message;
    }
    get() {
        return this.messages;
    }
    isEmpty() {
        return this.messages.length === 0;
    }
    removeAt(index) {
        this.messages.splice(index, 1);
    }
}

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
class Process {
    constructor(system, func, args) {
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
    start() {
        const function_scope = this;
        let machine = this.main();
        this.system.schedule(function () {
            function_scope.system.set_current(function_scope.pid);
            function_scope.run(machine, machine.next());
        }, this.pid);
    }
    *main() {
        let retval = process_state.NORMAL;
        try {
            yield* this.func.apply(null, this.args);
        }
        catch (e) {
            console.error(e);
            retval = e;
        }
        this.system.exit(retval);
    }
    process_flag(flag, value) {
        const old_value = this.flags.get(flag);
        this.flags.set(flag, value);
        return old_value;
    }
    is_trapping_exits() {
        return (this.flags.has(Symbol.for('trap_exit')) &&
            this.flags.get(Symbol.for('trap_exit')) == true);
    }
    signal(reason) {
        if (reason !== process_state.NORMAL) {
            console.error(reason);
        }
        this.system.remove_proc(this.pid, reason);
    }
    receive(fun) {
        let value = process_state.NOMATCH;
        let messages = this.mailbox.get();
        for (let i = 0; i < messages.length; i++) {
            try {
                value = fun(messages[i]);
                if (value !== process_state.NOMATCH) {
                    this.mailbox.removeAt(i);
                    break;
                }
            }
            catch (e) {
                if (e.constructor.name != 'MatchError') {
                    this.system.exit(e);
                }
            }
        }
        return value;
    }
    run(machine, step) {
        const function_scope = this;
        if (!step.done) {
            let value = step.value;
            if (is_sleep(value)) {
                this.system.delay(function () {
                    function_scope.system.set_current(function_scope.pid);
                    function_scope.run(machine, machine.next());
                }, value[1]);
            }
            else if (is_receive(value) && receive_timed_out(value)) {
                let result = value[3]();
                this.system.schedule(function () {
                    function_scope.system.set_current(function_scope.pid);
                    function_scope.run(machine, machine.next(result));
                });
            }
            else if (is_receive(value)) {
                let result = function_scope.receive(value[1]);
                if (result === process_state.NOMATCH) {
                    this.system.suspend(function () {
                        function_scope.system.set_current(function_scope.pid);
                        function_scope.run(machine, step);
                    });
                }
                else {
                    this.system.schedule(function () {
                        function_scope.system.set_current(function_scope.pid);
                        function_scope.run(machine, machine.next(result));
                    });
                }
            }
            else {
                this.system.schedule(function () {
                    function_scope.system.set_current(function_scope.pid);
                    function_scope.run(machine, machine.next(value));
                });
            }
        }
    }
}

/**
 * Manages all of the processes.
 */
class ProcessSystem {
    constructor(scheduler = new DefaultScheduler(5)) {
        this.pids = new Map();
        this.mailboxes = new Map();
        this.names = new Map();
        this.links = new Map();
        this.monitors = new Map();
        this.current_process = null;
        this.scheduler = scheduler;
        this.suspended = new Map();
        let process_system_scope = this;
        this.main_process_pid = this.spawn(function* () {
            yield process_system_scope.sleep(Symbol.for('Infinity'));
        });
        this.set_current(this.main_process_pid);
    }
    static *run(fun, args, context = null) {
        if (fun.constructor.name === 'GeneratorFunction') {
            return yield* fun.apply(context, args);
        }
        else {
            return yield fun.apply(context, args);
        }
    }
    /**
     * Starts a process represented by the given generator function
     * @param args Either a generator function or a module, function and arguments
     */
    spawn(...args) {
        if (args.length === 1) {
            let fun = args[0];
            return this.add_proc(fun, [], false, false).pid;
        }
        else {
            let mod = args[0];
            let fun = args[1];
            let the_args = args[2];
            return this.add_proc(mod[fun], the_args, false, false).pid;
        }
    }
    /**
     * Starts a process using the generator function from the specified module
     * @param args Either a generator function or a module, function and arguments
     */
    spawn_link(...args) {
        if (args.length === 1) {
            let fun = args[0];
            return this.add_proc(fun, [], true, false).pid;
        }
        else {
            let mod = args[0];
            let fun = args[1];
            let the_args = args[2];
            return this.add_proc(mod[fun], the_args, true, false).pid;
        }
    }
    /**
     * links the current process with the process from the given pid
     * @param pid pid of the process to link to
     */
    link(pid) {
        const currentProcessPid = this.pid();
        if (currentProcessPid != null) {
            const currentProcessLink = this.links.get(currentProcessPid.id);
            const IncomingProcessLink = this.links.get(pid.id);
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
    unlink(pid) {
        const currentProcessPid = this.pid();
        if (currentProcessPid != null) {
            const currentProcessLink = this.links.get(currentProcessPid.id);
            const IncomingProcessLink = this.links.get(pid.id);
            if (currentProcessLink && IncomingProcessLink) {
                currentProcessLink.delete(pid);
                IncomingProcessLink.delete(currentProcessPid);
            }
        }
    }
    /**
     * Spawns a process and then monitors it
     * @param args Either a generator function or a module, function and arguments
     */
    spawn_monitor(...args) {
        if (args.length === 1) {
            let fun = args[0];
            let process = this.add_proc(fun, [], false, true);
            return [process.pid, process.monitors[0]];
        }
        else {
            let mod = args[0];
            let fun = args[1];
            let the_args = args[2];
            let process = this.add_proc(mod[fun], the_args, false, true);
            return [process.pid, process.monitors[0]];
        }
    }
    /**
     * Monitors the given process
     * @param pid pid of the process to link to
     */
    monitor(pid) {
        const real_pid = this.pidof(pid);
        const ref = this.make_ref();
        if (this.currentProcess != null) {
            if (real_pid) {
                this.monitors.set(ref, {
                    monitor: this.currentProcess.pid,
                    monitee: real_pid,
                });
                const process = this.pids.get(real_pid.id);
                if (process) {
                    process.monitors.push(ref);
                }
                return ref;
            }
            else {
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
    demonitor(ref) {
        if (this.monitors.has(ref)) {
            this.monitors.delete(ref);
            return true;
        }
        return false;
    }
    /**
     * Sets the current process
     * @param id PID or name of process
     */
    set_current(id) {
        let pid = this.pidof(id);
        if (pid) {
            const next = this.pids.get(pid.id);
            if (next) {
                this.current_process = next;
                if (this.currentProcess) {
                    this.currentProcess.status = process_state.RUNNING;
                }
            }
        }
    }
    add_proc(fun, args, linked, monitored) {
        let newproc = new Process(this, fun, args);
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
    remove_proc(pid, exitreason) {
        this.pids.delete(pid.id);
        this.unregister(pid);
        this.scheduler.removePid(pid);
        const linkedPids = this.links.get(pid.id);
        if (linkedPids) {
            for (let linkpid of linkedPids) {
                this.exit(linkpid, exitreason);
                const linkedPid = this.links.get(linkpid.id);
                if (linkedPid) {
                    linkedPid.delete(pid);
                }
            }
            this.links.delete(pid.id);
        }
    }
    /**
     * registers the given name to the pid
     * @param name The name to give the process
     * @param pid The pid of the process
     */
    register(name, pid) {
        if (!this.names.has(name)) {
            this.names.set(name, pid);
        }
        else {
            throw new Error('Name is already registered to another process');
        }
    }
    /**
     * Finds a process by the given name
     * @param name the name of the process
     */
    whereis(name) {
        return this.names.has(name) ? this.names.get(name) : null;
    }
    /**
     * returns the liast of names that are registered
     */
    registered() {
        return this.names.keys();
    }
    /**
     * unregisters the names associated with the pid
     * @param pid The pid of the process
     */
    unregister(pid) {
        for (let name of this.names.keys()) {
            if (this.names.has(name) && this.names.get(name) === pid) {
                this.names.delete(name);
            }
        }
    }
    /**
     * Returns the PID of the current process
     */
    pid() {
        if (this.currentProcess) {
            return this.currentProcess.pid;
        }
        return null;
    }
    /**
     * takes the input and tries to find the pid. Input can be a `pid`, `Process`, or name the pid is associated with
     * @param id The registered name or pid of the process
     */
    pidof(id) {
        if (id instanceof PID) {
            return this.pids.has(id.id) ? id : null;
        }
        else if (id instanceof Process) {
            return id.pid;
        }
        else {
            let pid = this.whereis(id);
            if (pid === null)
                throw 'Process name not registered: ' + id + ' (' + typeof id + ')';
            return pid;
        }
    }
    /**
     * sends a message the the process represented by the pid
     * @param id
     * @param msg
     */
    send(id, msg) {
        const pid = this.pidof(id);
        if (pid) {
            const mailbox = this.mailboxes.get(pid.id);
            if (mailbox) {
                mailbox.deliver(msg);
            }
            if (this.suspended.has(pid)) {
                let fun = this.suspended.get(pid);
                this.suspended.delete(pid);
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
    receive(fun, timeout = 0, timeoutFn = () => true) {
        let DateTimeout = null;
        if (timeout === 0 || timeout === Infinity) {
            DateTimeout = null;
        }
        else {
            DateTimeout = Date.now() + timeout;
        }
        return [process_state.RECEIVE, fun, DateTimeout, timeoutFn];
    }
    /**
     * puts the current process to sleep
     * @param duration
     */
    sleep(duration) {
        return [process_state.SLEEP, duration];
    }
    /**
     * Suspends the current process
     * @param fun
     */
    suspend(fun) {
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
    delay(fun, time) {
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
    schedule(fun, pid) {
        if (this.currentProcess) {
            const the_pid = pid != null ? pid : this.currentProcess.pid;
            this.scheduler.schedule(the_pid, fun);
        }
    }
    /**
     * terminates the current process with the given reason.
     * @param one
     * @param two
     */
    exit(one, two) {
        let pid = null;
        let reason = null;
        let process = null;
        if (two) {
            pid = one;
            reason = two;
            const thePid = this.pidof(pid);
            if (thePid) {
                process = this.pids.get(thePid.id);
            }
            if (process) {
                if (process.is_trapping_exits() ||
                    reason === process_state.KILL ||
                    reason === process_state.NORMAL) {
                    const mailbox = this.mailboxes.get(process.pid.id);
                    if (mailbox) {
                        mailbox.deliver(['unboxed', [process_state.EXIT, this.pid(), reason]]);
                    }
                }
                else {
                    process.signal(reason);
                }
            }
        }
        else {
            if (this.currentProcess) {
                pid = this.currentProcess.pid;
                reason = one;
                process = this.currentProcess;
                process.signal(reason);
            }
        }
        if (process) {
            for (let ref of process.monitors) {
                let mons = this.monitors.get(ref);
                if (mons) {
                    this.send(mons['monitor'], ['unboxed', ['DOWN', ref, mons['monitee'], mons['monitee'], reason]]
                        );
                }
            }
        }
    }
    /**
     * terminates the current process with an error
     * @param reason
     */
    error(reason) {
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
    process_flag(...args) {
        if (args.length == 2) {
            const flag = args[0];
            const value = args[1];
            if (this.currentProcess) {
                return this.currentProcess.process_flag(flag, value);
            }
        }
        else {
            const pid = this.pidof(args[0]);
            if (pid) {
                const flag = args[1];
                const value = args[2];
                const process = this.pids.get(pid.id);
                if (process) {
                    return process.process_flag(flag, value);
                }
            }
        }
    }
    /**
     * Adds a value to the current process's dictionary
     * @param key
     * @param value
     */
    put(key, value) {
        if (this.currentProcess) {
            this.currentProcess.dict.set(key, value);
        }
    }
    /**
     * Gets the current process's dictionary
     */
    get_process_dict() {
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
    get(key, default_value = null) {
        if (this.currentProcess && key in this.currentProcess.dict) {
            return this.currentProcess.dict.get(key);
        }
        else {
            return default_value;
        }
    }
    /**
     * Gets all the keys from the current process's dictionary
     * @param value
     */
    get_keys(value) {
        if (value) {
            let keys = [];
            if (this.currentProcess) {
                for (let key of Object.keys(this.currentProcess.dict)) {
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
    erase(key) {
        if (this.currentProcess) {
            if (key != null && this.currentProcess.dict.has(key)) {
                this.currentProcess.dict.delete(key);
            }
            else {
                this.currentProcess.dict = new Map();
            }
        }
    }
    /**
     * Returns if the given pid is alive
     * @param pid
     */
    is_alive(pid) {
        const real_pid = this.pidof(pid);
        return real_pid != null;
    }
    /**
     * Returns a list of all the pids
     */
    list() {
        return Array.from(this.pids.keys());
    }
    /**
     * Returns a unique reference
     */
    make_ref() {
        return new REF();
    }
    get currentProcess() {
        if (this.current_process) {
            return this.current_process;
        }
        return null;
    }
}

var system = new ProcessSystem();
var loaded_code = new Map();

function atom_to_function_name(module, name, arity) {
    if (module == "erlang")
        return "erlang__" + name + "__" + arity;
    return "erlps__" + name + "__" + arity;
}

function atom_to_module_name(name) {
    if (name == "erlang") return "Erlang.Builtins"
    if (name == "io") return "Erlang.Io"
    return name.split('_').map((x) => x.charAt(0).toUpperCase() + x.slice(1)).join('.')
}

/* FFI CODE */
function do_apply_4(moduleName) {
        return function(functionName) {
            return function(argumentArray) {
                return function(failCallback) {
                    var mName = atom_to_module_name(moduleName);
                    var fName = atom_to_function_name(moduleName, functionName, argumentArray.length);
                    return do_ffi_remote_fun_call(mName)(fName)(argumentArray)(failCallback);
                };
            };
        };
    };

function do_function_exported_3(moduleName) {
    return function(functionName) {
        return function(arity) {
            var mName = atom_to_module_name(moduleName);
            var fName = atom_to_function_name(moduleName, functionName, arity);
            var module = loaded_code.get(mName);
            if (module) {
                var f = module[fName];
                if (f)
                    return true;
                return false;
            }
            return false;
        }
    }
}

function do_onload(name, module) {
    var f = module["onload"];
    if (f) {
        f();
    }
    /* Very very very dirty JS hack... */
    if (name == "Erlang.Builtins") {
        const op_aliases = {
                 "+": "op_plus"
                 , "-": "op_minus"
                 , "*": "op_mult"
                 , "/":   "op_div"
                 , "div": "op_div_strict"
                 , "%":   "op_rem"
                 , "rem": "op_rem_strict"
                 , "/=":  "op_neq"
                 , "=/=": "op_exactNeq"
                 , "==":  "op_eq"
                 , "=:=": "op_exactEq"
                 , ">":   "op_greater"
                 , "<":   "op_lesser"
                 , ">=":  "op_greaterEq"
                 , "=<":  "op_lesserEq"
                 , "++":  "op_append"
                 , "--":  "op_unAppend"
                 , "and": "op_and"
                 , "or":  "op_or"
                 , "!":   "send__2"
                 };
        const ar1_aliases = { "not": "op_not"
                            , "-": "op_neg"
                            , "+": "op_unary_plus"
                            };
        for(var v in op_aliases) {
            module["erlang__" + v + "__2"] = module["erlang__" + op_aliases[v]]
        };
        for(var v in ar1_aliases) {
            module["erlang__" + v + "__1"] = module["erlang__" + ar1_aliases[v]]
        };
    }
    else if (name == "Maps") {
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
    }
    else if (name == "Lists") {
        var bifs = get_erlang_module();
        module["erlps__keyfind__3"] = bifs["lists__keyfind__3"];
        module["erlps__keymember__3"] = bifs["lists__keymember__3"];
        module["erlps__keysearch__3"] = bifs["lists__keysearch__3"];
        module["erlps__member__2"] = bifs["lists__member__2"];
        module["erlps__reverse__2"] = bifs["lists__reverse__2"];
    }
    else if (name == "Math") {
        var bifs = get_erlang_module();
        // TODO
    }
    else if (name == "Prim.Eval") {
        var bifs = get_erlang_module();
        module["erlps__prim_eval__3"] = bifs["prim_eval__receive__2"];
    }
    else if (name == "Erts.Internal") {
        var bifs = get_erlang_module();
        module["erlps__map_next__3"] = bifs["erts_internal__map_next__3"];
    }

    return module;
}

function module_resolve(name) {
    try {
        return PS[name];
    } catch(e) {
        try {
            return require("../"+name+"/index.js");
        } catch(e) {
            return undefined;
        }
    }
}

var bif_module = undefined;
function get_erlang_module() {
    if(bif_module === undefined) {
        const name = "Erlang.Builtins";
        bif_module = loaded_code.get(name);
        if(bif_module === undefined) {
            bif_module = module_resolve(name);
            bif_module = do_onload(name, bif_module);
            loaded_code.set(name, bif_module);
        }
    }
    return bif_module;
}

function do_ffi_ensure_loaded(moduleName) {
    return function(nofile) {
        return function(success) {
            moduleName = atom_to_module_name(moduleName);
            var module = loaded_code.get(moduleName);
            if (module === undefined) {
                module = module_resolve(moduleName);
                if(module) {
                    module = do_onload(moduleName, module);
                    loaded_code.set(moduleName, module);
                    return success;
                } else {
                    return nofile;
                }
            }
            return success;
        }
    }
}

function do_ffi_remote_fun_call(moduleName) {
    return function(functionName) {
        return function(argumentArray) {
            return function(undefCallback) {
                var module = loaded_code.get(moduleName);
                if (module === undefined) {
                    module = module_resolve(moduleName);
                    if(module) {
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
            }
        }
    }
}

function do_spawn_1(action) {
    return function(pid_ctr) {
        var pid = system.spawn(function* (){
            try {
                return action();
            } catch (e) {
            }
        });
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
    return function(term) {
        return term;
    }
}

function do_receive_2(match_fun) {
    return function(timeout_val) {
        return function(atom_ctr) {
            if (timeout_val < 0) {
                timeout_val = 0;
            } else if(timeout_val == 0) {
                timeout_val = 1;
            }

            // TIME FOR A VERY DIRTY HACK :(
            console.log ( (function*() {yield system.receive( (msg) => {
                console.log("RECEIVED: ", msg)
                });
            })().next());
        }
    }
}

function do_make_ref_0(ref_ctr) {
    return ref_ctr(system.make_ref().id);
}

function do_put_2(cmp) {
    return function(key) {
        return function(value) {
            return function(on_no_key) {
                var dict = system.get_process_dict();
                for (let dpair of dict) {
                    let dkey = dpair[0];
                    let dval = dpair[1];
                    if (cmp(dkey)(key)) {
                        system.put(dkey, value);
                        return dval;
                    }
                }
                system.put(key, value);
                return on_no_key;
            }
        }
    }
}

function do_get_1(cmp) {
    return function(key) {
        return function(on_no_key) {
            var dict = system.get_process_dict();
            for (let dpair of dict) {
                let dkey = dpair[0];
                let dval = dpair[1];
                if (cmp(dkey)(key)) {
                    return dval;
                }
            }
            return on_no_key;
        }
    }
}

function do_get_0(wrap) {
    var dict = system.get_process_dict();
    var res = [];
    for (let dpair of dict) {
        let dkey = dpair[0];
        let dval = dpair[1];
        res.push(wrap(dkey)(dval))
    }
    return res;
}

function do_get_keys_0(x) {
    var dict = system.get_process_dict();
    var res = [];
    for (let dpair of dict) {
        let dkey = dpair[0];
        res.push(dkey)
    }
    return res;
}

function do_get_keys_1(cmp) {
    return function(val) {
        var dict = system.get_process_dict();
        var res = [];
        for (let dpair of dict) {
            let dkey = dpair[0];
            let dval = dpair[1];
            if (cmp(dval)(val)) {
                res.push(dkey)
            }
        }
        return res;
    }
}

function do_erase_0(wrap) {
    var res = do_get_0(wrap);
    system.erase();
    return res;
}

function do_erase_1(cmp) {
    return function(key) {
        return function(on_no_key) {
            var dict = system.get_process_dict();
            for (let dpair of dict) {
                let dkey = dpair[0];
                let dval = dpair[1];
                if (cmp(dkey)(key)) {
                    system.erase(dkey);
                    return dval;
                }
            }
            return on_no_key;
        }
    }
}

    return {
        do_ffi_ensure_loaded: do_ffi_ensure_loaded,
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

export const do_ffi_remote_fun_call = RUNTIME.do_ffi_remote_fun_call;
export const do_ffi_ensure_loaded = RUNTIME.do_ffi_ensure_loaded;
export const system = RUNTIME.system;
export const do_make_ref_0 = RUNTIME.do_make_ref_0;
export const do_receive_2 = RUNTIME.do_receive_2;
export const do_send_2 = RUNTIME.do_send_2;
export const do_self_0 = RUNTIME.do_self_0;
export const do_is_process_alive_1 = RUNTIME.do_is_process_alive_1;
export const do_spawn_1 = RUNTIME.do_spawn_1;
export const do_apply_4 = RUNTIME.do_apply_4;
export const do_put_2 = RUNTIME.do_put_2;
export const do_get_1 = RUNTIME.do_get_1;
export const do_get_0 = RUNTIME.do_get_0;
export const do_get_keys_0 = RUNTIME.do_get_keys_0;
export const do_get_keys_1 = RUNTIME.do_get_keys_1;
export const do_erase_0 = RUNTIME.do_erase_0;
export const do_erase_1 = RUNTIME.do_erase_1;
export const do_function_exported_3 = RUNTIME.do_function_exported_3;

// Missing math functions
//--------------------------------------------------------------------------------
export const acosh = Math.acosh || function (x) {
  return Math.log(x + Math.sqrt(x * x - 1));
};

export const asinh = Math.asinh || function (x) {
  if (x === -Infinity) {
    return x;
  } else {
    return Math.log(x + Math.sqrt(x * x + 1));
  }
};

export const atanh = Math.atanh || function (x) {
  return Math.log((1 + x) / (1 - x)) / 2;
};

export const cbrt = Math.cbrt || function (x) {
  if (x === 0) {
    return x; // +0 or -0
  } else if (x < 0) {
    return -Math.pow(-x, 1 / 3);
  } else {
    return Math.pow(x, 1 / 3);
  }
};

export const clz32 = Math.clz32 || function (x) {
  if (x === 0) {
    return 32;
  }
  return 31 - Math.floor(Math.log(x >>> 0) * Math.LOG2E);
};

export const cosh = Math.cosh || function (x) {
  return (Math.exp(x) + Math.exp(-x)) / 2;
};

export const expm1 = Math.expm1 || function (x) {
  return Math.exp(x) - 1;
};

export const hypot = Math.hypot ? function (x) {
  return function (y) {
    return Math.hypot(x, y);
  };
} : function (x) {
  return function (y) {
    return Math.sqrt(x * x + y * y);
  };
};

export const hypot3 = Math.hypot ? function (x) {
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

export const log1p = Math.log1p || function (x) {
  return Math.log(x + 1);
};

export const log10 = Math.log10 || function (x) {
  return Math.log(x) * Math.LOG10E;
};

export const log2 = Math.log2 || function (x) {
  return Math.log(x) * Math.LOG2E;
};

export const sign = Math.sign || function (x) {
  if (x > 0) {
    return 1;
  } else if (x < 0) {
    return -1;
  } else {
    return +x; // +0 or -0 or NaN
  }
};

export const sinh = Math.sinh || function (x) {
  return (Math.exp(x) - Math.exp(-x)) / 2;
};

export const tanh = Math.tanh || function (x) {
  var ex = Math.exp(2 * x);
  if (ex === Infinity) {
    return 1;
  } else {
    return (ex - 1) / (ex + 1);
  }
};
