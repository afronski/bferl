# Compiler

## Internals

## API

- Compiling code from file - `bferl_app:compile_file("hello_world.bf").`
- Compile code delivered as a string - `bferl_app:compile_code(",+.")`
- Both commands will produce new module with `start` command.
  - You can start it by typing `MODULE_NAME_AFTER_COMPILATION:start().`
  - If you would like to provide a *tape* with input characters you can do it by:
  - `MODULE_NAME_AFTER_COMPILATION:start("This is input tape content.").`

### *Brainfork*

- *Brainfork* variation can be delivered to the *compiler* facilities
  only via functions that read files.
  - That particular file should have extension `.bfo`, otherwise it will be
  interpreted as a *Brainfuck*.

## Example Session
