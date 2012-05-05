#!/usr/bin/env rdmd -g -debug=verbose

import std.array, std.conv, std.stdio, std.math;

import std.file : read;

enum LUA_VERSION = "Lua 5.1",
     LUA_RELEASE = "Lua 5.1.4",
     LUA_VERSION_NUM = 501;

/*===========================================================================
  We assume that instructions are unsigned numbers.
  All instructions have an opcode in the first 6 bits.
  Instructions can have the following fields:
	`A' : 8 bits
	`B' : 9 bits
	`C' : 9 bits
	`Bx' : 18 bits (`B' and `C' together)
	`sBx' : signed Bx

  A signed argument is represented in excess K; that is, the number
  value is the unsigned value minus K. K is exactly the maximum value
  for that argument (so that -max is represented by 0, and +max is
  represented by 2*max), which is half the maximum for the corresponding
  unsigned argument.
===========================================================================*/
enum ArgMode { iABC, iABx, iAsBx };

enum SIZE_C = 9,
            SIZE_B = 9,
            SIZE_Bx = (SIZE_C + SIZE_B),
            SIZE_A = 8,

            SIZE_OP = 6,

            POS_OP = 0,
            POS_A = (POS_OP + SIZE_OP),
            POS_C = (POS_A + SIZE_A),
            POS_B = (POS_C + SIZE_C),
            POS_Bx = POS_C;

enum LUA_TNIL = 0,
              LUA_TBOOLEAN =    1,
              LUA_TLIGHTUSERDATA =  2,
              LUA_TNUMBER  =    3,
              LUA_TSTRING  =    4,
              LUA_TTABLE   =    5,
              LUA_TFUNCTION  =  6,
              LUA_TUSERDATA  =  7,
              LUA_TTHREAD    =  8;

debug(verbose)
    void dbg(T ...)(lazy T t)
    {
        writeln("\033[34m", t, "\033[0m");
    }
else
    void dbg(T ...)(T t) {}

// A too-simple Lua table impl.
class Table {
    void opIndexAssign(V, K)(V val, K key)
    {
        static if (is(V == LuaValue))
            auto _val = val;
        else
            auto _val = LuaValue(val);
        static if (is(K == LuaValue))
            auto _key = key;
        else
            auto _key = LuaValue(key);

        vals[_key] = _val;
    }

    LuaValue opIndex(K)(K key) {
        static if (is(K == LuaValue))
            auto _key = key;
        else
            auto _key = LuaValue(key);

        LuaValue *ret = _key in vals;
        if (!ret)
            return nil;
        return *ret;
    }

    private LuaValue[LuaValue] vals;
}

// A native D function callable from Lua
alias LuaValue[] function(LuaValue[]) LuaFunc;
// The state of a running function, akin to LUA_TFUNCTION stuffs
class Closure {
    FunctionInfo *fun;
    this(FunctionInfo *fun) {
        this.fun = fun;
    }
}
// I need to combine the two concepts

// can't add this type to an AA, so using a manual struct for now.
// see http://d.puremagic.com/issues/show_bug.cgi?id=2451
import std.variant;
alias Algebraic!(bool, double, string) LuaValue2;

// A Lua value tagged union, akin to Lua's TValue
struct LuaValue1 {

    this(T)(T v) {
        val = v;
    }

    alias Algebraic!(bool, double, string, Table, LuaFunc, Closure) Val;
    Val val;
    alias val.peek peek;

    ulong toHash() const {
        return (cast(Val)val).toHash();
    }

    int opCmp(ref const LuaValue1 rhs) const {
        return (cast(Val)val).opCmp((cast(LuaValue1)rhs).val);
    }

    bool nil() {
        return !val.hasValue;
    }

    string toString() {
        if (nil) {
            return "nil";
        } else if (auto b = peek!bool) {
            return (*b) ? "true" : "false";
        } else if (auto d = peek!double) {
            return to!string(*d);
        } else if (auto s = peek!string) {
            return *s;
        } else if (auto t = peek!Table) {
            return "table: " ~ to!string(t);
        } else if (auto f = peek!LuaFunc) {
            return "function: " ~ to!string(f);
        } else if (auto f = peek!Closure) {
            return "function: " ~ to!string(f);
        }
        assert(0);
    }
}

alias LuaValue1 LuaValue;

private LuaValue nil;

/* Some simple Lua runtime function impls for testing things */
LuaValue[] LuaWrite(LuaValue[] args)
{
    foreach (val; args) {
        write(val.toString);
    }
    return [];
}

LuaValue[] LuaRead(LuaValue[] args)
{
    if (args.empty) {
        // read until newline
        string ret = stdin.readln();
        return [ LuaValue(ret) ];
    } else {
        auto results = new LuaValue[args.length];
        foreach (i, val; args) {
            auto len = val.peek!double;
            if (!len)
                throw new Exception("bad argument to read");
            auto buf = new string(cast(size_t)*len);
            stdin.rawRead(cast(ubyte[])buf);
            results[i] = LuaValue(buf);
        }
        return results;
    }
}

LuaValue[] LuaFormat(LuaValue[] args)
{
    if (args.empty) {
        throw new Exception("need one argument to format");
    }

    string ret;

    foreach (val; args) {
        ret ~= val.toString;
    }

    return [ LuaValue(ret) ];
}

LuaValue[] LuaByte(LuaValue[] args)
{
    assert(args.length > 0);
    auto input = *args[0].peek!string;
    int i = 1;
    if (args.length > 1)
        i = cast(int)*args[1].peek!double;
    int j = i;
    if (args.length > 2)
        j = cast(int)*args[2].peek!double;

    LuaValue[] ret;
    ret.length = j+1 - i;
    foreach (x; i .. j+1) {
        ret[x - i] = LuaValue(cast(double)input[x - 1]);
    }
    return ret;
}

// very incomplete
enum OpCode : uint {
    OP_MOVE = 0,
    OP_LOADK,
    OP_LOADBOOL,
    OP_LOADNIL,
    OP_GETUPVAL,

    OP_GETGLOBAL,
    OP_GETTABLE,

    OP_SETGLOBAL,

    OP_EQ = 23,

    OP_CALL = 28,

    OP_RETURN = 30,

    OP_CLOSURE = 36,
}

class LuaState {
    alias int Instruction;

    this() {
        stack = new LuaValue[100];
        G = new Table;
        baseGlobals(G);
    }

    void baseGlobals(Table G) {
        G["_VERSION"] = LUA_VERSION;
        G["_G"] = G;

        auto io = new Table;
        io["write"] = &LuaWrite;
        io["read"] = &LuaRead;
        G["io"] = io;

        auto string = new Table;
        string["format"] = &LuaFormat;
        string["byte"] = &LuaByte;
        G["string"] = string;
    }

    int execute(ref FunctionInfo fun, uint calldepth = 0) {
        Instruction[] pc = fun.code;
        consts = fun.constants;

        debug(verbose)
            void dbg(T ...)(lazy T t)
            {
                foreach (i; 0 .. calldepth)
                    write("  ");
                .dbg(t);
            }
        else
            void dbg(T ...)(T t) {}

        while (true) {
            assert(!pc.empty);
            const Instruction i = pc.front;
            pc.popFront();
            LuaValue *ra = &stack[argA(i)];
            dbg("opcode: ", to!string(opcode(i)));
            with (OpCode) {
                switch(opcode(i)) {
                    case OP_MOVE:
                        *ra = stack[argB(i)];
                        dbg("moved (", ra.toString, ") from ",
                                argB(i), " to ", argA(i));
                        continue;

                    case OP_LOADK:
                        *ra = consts[argBx(i)];
                        dbg("loaded (", ra.toString, ") into reg ", argA(i));
                        continue;
                    case OP_GETGLOBAL:
                        int k_i = argBx(i);
                        LuaValue val = consts[k_i];
                        auto str = val.peek!(string);
                        dbg("getglobal \"", val.toString, '"');
                        assert(str);
                        *ra = G[val];
                        continue;
                    case OP_GETTABLE:
                        Table *table = stack[argB(i)].peek!(Table);
                        if (!table)
                            throw new Exception("Can't read table");
                        auto key = lookupK(argC(i));
                        dbg("looking up: \"", key.toString, "\"");
                        *ra = (*table)[*key];
                        continue;

                    case OP_SETGLOBAL:
                        auto val = consts[argBx(i)];
                        assert(val.peek!string);
                        G[val] = *ra;
                        dbg("setglobal \"", consts[argBx(i)].toString, "\" = (",
                                ra.toString, ")");
                        continue;

                    case OP_EQ:
                        auto rb = lookupK(argB(i));
                        auto rc = lookupK(argC(i));
                        int result = (rb == rc) ? 1 : 0;
                        if (result == argA(i)) {
                            pc = pc[argsBx(pc.front) .. $];
                        }
                        pc.popFront();
                        continue;

                    case OP_CALL:
                        int b = argB(i);
                        int nresults = argC(i) - 1;
                        dbg("b: ", b, " nresults: ", nresults);
                        /*assert(b != 0); // not sure yet what b == 0 does*/
                        /*lastpc = pc;*/
                        if (auto func = ra.peek!LuaFunc) {
                            auto results = b > 0 ?
                                (*func)(stack[argA(i) + 1 .. argA(i) + b]) :
                                (*func)([]);
                            auto resultsToUse =
                                (nresults < 0 || nresults > results.length) ?
                                results.length : nresults;
                            if (resultsToUse > 0) {
                                stack[argA(i) .. argA(i) + resultsToUse]
                                    = results[0 .. resultsToUse];
                            }
                            dbg("got back ", results.length, " results, "
                                    "inserted ", resultsToUse, " results "
                                    "starting at reg ", argA(i));
                        } else if (auto func = ra.peek!Closure) {
                            auto savedstack = stack;
                            auto savedpc = pc;
                            stack = stack[argA(i) + 1 .. $];
                            auto results = execute(*func.fun, calldepth + 1);
                            assert(results > 0);
                            pc = savedpc;
                            stack = savedstack;
                            consts = fun.constants;

                            int resultsToUse =
                                (nresults < 0 || nresults > results - 1) ?
                                results - 1 : nresults;
                            if (resultsToUse > 0) {
                                foreach (x; 0 .. resultsToUse)
                                    stack[argA(i) + x] =
                                    stack[argA(i) + x + 2];
                            }
                            dbg("got back ", results - 1, " results, "
                                    "inserted ", resultsToUse, " results "
                                    "starting at reg ", argA(i));
                        } else {
                            throw new Exception("attempt to call "
                                    ~ ra.toString ~ " which is not a function");
                        }
                        continue;
                    case OP_RETURN:
                        int b = argB(i);
                        assert(b != 0); // not sure yet what b == 0 does
                        /*if (--calldepth == 0)*/
                            return b;
                        continue;

                    case OP_CLOSURE:
                        auto fun_num = argBx(i);
                        auto new_fun = &fun.subfuns[fun_num];
                        assert(new_fun.nups == 0); // not ready for upvals
                        *ra = LuaValue(new Closure(new_fun));
                        continue;

                    default:
                        throw new Exception("opcode " ~ to!string(opcode(i))
                                ~ " not implemented");

                }
            }
        }
    }

    pure nothrow OpCode opcode(Instruction i) {
        return cast(OpCode)((i >> POS_OP) & 0b111_111);
    }

    int argBx(Instruction i) {
        return cast(int)((i >> POS_Bx) & 0b111111111_111111111);
    }

    int argsBx(Instruction i) {
        return argBx(i) - (((1 << SIZE_Bx) - 1) >> 1);
    }

    int argA(Instruction i) {
        return cast(int)((i >> POS_A) & 0b11111111);
    }

    int argB(Instruction i) {
        return cast(int)((i >> POS_B) & 0b111111111);
    }

    int argC(Instruction i) {
        return cast(int)((i >> POS_C) & 0b111111111);
    }

    // this bit 1 means constant (0 means register)
    enum BITRK = (1 << (SIZE_B - 1));
    LuaValue *lookupK(int i) {
        if (i & BITRK) {
            return &consts[(i & ~BITRK)];
        } else {
            return &stack[i];
        }
    }

    LuaValue[] consts;
    LuaValue[] stack;
    Table G;
}

void LoadVar(T)(ref ubyte[] data, out T x)
{
    x = (cast(T*)data)[0];
    data = data[x.sizeof .. $];
}

string LoadString(ref ubyte[] data)
{
    ulong size;
    LoadVar(data, size);
    if (size == 0)
        return null;
    string ret = cast(string)(data[0 .. cast(size_t)size-1].idup);
    data = data[cast(size_t)size .. $];
    return ret;
}

struct FunctionInfo
{
    string source;
    int lineDefined, lastLineDefined;
    ubyte nups, numparams, vararg, maxstack;
    LuaState.Instruction[] code;
    LuaValue[] constants;
    FunctionInfo[] subfuns;
}

// recursive
void LoadFunction(ref ubyte[] dat, ref FunctionInfo fun)
{
    fun.source = LoadString(dat);
    dbg("LoadFunction Name: (", fun.source, ")");

    LoadVar(dat, fun.lineDefined);
    LoadVar(dat, fun.lastLineDefined);
    with (fun)
        dbg("Line Defined: ", lineDefined, " Last Line: ", lastLineDefined);

    LoadVar(dat, fun.nups);
    LoadVar(dat, fun.numparams);
    LoadVar(dat, fun.vararg);
    LoadVar(dat, fun.maxstack);
    with (fun)
        dbg("nups: ", nups, " numparams: ", numparams, " vararg: ",
                vararg, " maxstack: ", maxstack);

    int codesize;
    LoadVar(dat, codesize);
    fun.code = cast(LuaState.Instruction[])
        dat[0 .. codesize * LuaState.Instruction.sizeof];
    dat = dat[codesize * LuaState.Instruction.sizeof .. $];

    int numconst;
    LoadVar(dat, numconst);
    fun.constants.length = numconst;

    dbg("constants: ", numconst);

    foreach(i; 0 .. numconst) {
        ubyte t;
        LoadVar(dat, t);
        switch (t) {
            case LUA_TNIL:
                // constants[i] is already nil
                break;
            case LUA_TBOOLEAN:
                byte b;
                LoadVar(dat, b);
                fun.constants[i] = LuaValue(b != 0);
                break;
            case LUA_TNUMBER:
                double num;
                LoadVar(dat, num);
                fun.constants[i] = LuaValue(num);
                break;
            case LUA_TSTRING:
                string str = LoadString(dat);
                fun.constants[i] = LuaValue(str);
                break;
            default:
        }
        /*dbg("loaded k ", fun.constants[i].toString);*/
    }

    int subfuns;
    LoadVar(dat, subfuns);
    dbg("subfuns: ", subfuns);
    fun.subfuns.length = subfuns;

    foreach(ref subfun; fun.subfuns)
        LoadFunction(dat, subfun);
}

void main(string[] args) {
    auto dat = (cast(ubyte[])read(args[1]))[12 .. $];

    FunctionInfo main;
    LoadFunction(dat, main);

    auto s = new LuaState;
    dbg("");
    s.execute(main);
}

unittest {
    auto dat = (cast(ubyte[])read("test/hello.luac"))[12 .. $];

    FunctionInfo main;
    LoadFunction(dat, main);

    auto s = new LuaState;
    dbg("");
    s.execute(main);
}
