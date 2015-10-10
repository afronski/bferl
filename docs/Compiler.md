# Compiler

## Internals

Another part of that project is a *Compiler*. It uses all standard techniques built in the *Erlang VM* related with compiler pipeline in order to fully leverage machine abilities. It is a **toy**, and it does not do any additional optimizations (like trimming the unused functions, rolling up increments and decrements etc.).

### Compiler pipeline

[Compiler pipeline](./images/compiler-pipeline.png)

Life of an *Erlang* file (http://studzien.github.io/hack-vm/part1.html#slide-5) begins with *preprocessing* (in order to evaluate all macros). Then, we are doing an *expansion* process , which is the first level of indirection on which you can hook as a *compiler engineer*.

If we want to *hook* into compiler pipeline we can do it on various levels. First possible way to hook, is to generate the *Erlang Abstract Format* parse trees, described [here](http://www.erlang.org/doc/apps/erts/absform.html). It is a very convenient way to create new language, and - what is even more important - you can still use tools like *dialyzer* or *cover*, because they are operating on that level (https://twitter.com/josevalim/status/626471306825986049). Below that level you are loosing the ability of running them on your code, and translating results into your representation.

Second place where you can hook (and at this level this compiler is implemented) is compilation to the *Core Erlang*. There is no point of running those tools on the *Brainfuck* / *Brainfork* code, so it is a small loss. :wink: AFAIR this level is used also by a *LFE* compiler and *codegen* module. The main drawback is that, even if there is a specification of the format and complete documentation of the `cerl` module (which is a helper module for generating *Core Erlang* forms) there are not so many examples how they should look like.

You can also compile to *Kernel Erlang* or even directly to the *BEAM Assembler*, but they have even more scarce resources and documentation. I guess that you have to *use the code*, to properly attack this level of compiler.

### Lexer

Before we can generate our forms in the *Core Erlang* format, we need to proceed with standard procedures related to any kind of languages. If the syntax and semantics are simple, like in our case (it is just a *Turing machine*-like syntax and semantics), correspondingly *lexer* and *parser* will be really simple.

In both cases *Erlang VM* has built-in mechanisms for dealing with those problems. File with the extension `.xrl` is a Lexer definition. It is called a [*Leex*](http://erlang.org/doc/man/leex.html), a lexical analyzer generator for *Erlang*. Taking the name as an example, you will probably deduce that it is heavily inspired by a predecessors called *Lex* or *Flex* - and that is true, syntax and behavior are identical. Lexer example can be shown [here](../src/bferl_compiler_lexer_bf.xrl).

### Parser

The same applies to parser. All files with an extension `.yrl` are handled by [*Yecc*](http://erlang.org/doc/man/yecc.html) which is an *LALR-1* parser generator for Erlang. Again it is inspired by *Yacc*. Parser example can be shown [here](../src/bferl_compiler_parser_bf.yrl). As you can see there, the only one rule which should be verified is related with loops and their syntactical structure (open and not closed loops are invalid *Brainfuck* / *Brainfork* programs).

### Code generation

This implementation generates the *Core Erlang* forms and then proceeds with a standard compilation flow down to the *BEAM* representation. Afterwards, it can be loaded inside a *VM* as a normal module. Whole implementation is gathered in the [bferl_compiler_codegen](../src/bferl_compiler_codegen.erl).

There is a one *thing*, which not explained enough in various places. Because we are compiling directly to *Core Erlang*, compiler cannot add two functions which are required by a *VM* in order to load modules properly - I am talking about `module_info/{0,1}`. The simplest possible implementation is a call to the `erlang:get_module_info/{0,1}` depending on the function version. Afterwards, we can construct and generate code in our module as we like.

#### What exactly is a *Core Erlang*?

*Core Erlang* is an intermediate representation of Erlang, intended to lie at a level between source code and the intermediate code typically found in compilers. Specification can be found [here](https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf).

The main point of choosing that over the generating the *Erlang* syntax is its regularity - it is just easier to generate *Core Erlang*, because it has much stricter and fair rules regarding, both semantics and structure. Additionally, all forms and clauses have helpers prepared in the modules [`cerl`](http://erldocs.com/18.0/compiler/cerl.html) and [`cerl_clauses`](http://erldocs.com/18.0/compiler/cerl_clauses.html).

## API

*TODO*

### Helpers

*TODO*

### `bferl_tools_compiler`

*TODO*

### *Brainfork*

*TODO*

## Example Session

*TODO*
