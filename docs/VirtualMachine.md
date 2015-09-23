# Virtual Machine

## Internals

## API

- Loading code from file - `bferl_app:run_file_on_vm("hello_world.bf").`
- Evaluating code - `bferl_app:run_code_on_vm(",+.").`
- Both methods have ability to run program in debug mode.
  - Add `debug_mode` flag to run it with debugging annotations:
    - `bferl_app:run_code_on_vm(",+.", [ debug_mode ]).`

### *Brainfork*

- *Brainfork* variation can be delivered to the *virtual machine* facilities
  only via functions that read files.
  - That particular file should have extension `.bfo`, otherwise it will be
    interpreted as a *Brainfuck*.

## Example Session
