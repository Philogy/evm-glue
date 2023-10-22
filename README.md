# 🫠 EVM Glue 

EVM Glue ("Glue") is an EVM assembler written in Rust. Glue gives you a simple API that allows 
you to insert opcodes, raw bytes, or so-called "references" (e.g. jump destinations) and assembles 
it into EVM bytecode. Glue also supports padded blocks, bytecode sections that will be padded up to
a certain length.

## Features
- ✅ Base Assembly Building Blocks
  - ✅ Opcodes
  - ✅ Raw Bytecode
  - ✅ Marks & References (e.g. for jump destinations, code sections)
- ✅ Extra Building Blocks
  - ✅ Padded Blocks
- ✅ Assembly Optimizations
  - ✅ Reference push/literal size reduction (e.g. reduce `PUSH2 0x0017` => `PUSH1 0x17` for
    references)

**Other TODOs:**
- [ ] Add tests

## Quickstart, A full example:

```rust
use evm_glue::assembler::assemble_full;
use evm_glue::assembly::Asm;
use evm_glue::data;
use evm_glue::opcodes::Opcode::*;
use evm_glue::utils::MarkTracker;
use hex_literal::hex as hx;
use Asm::*;

fn main() {
    let mut runtime_marks = MarkTracker::new();

    let empty_revert = runtime_marks.next();

    let runtime = vec![
        // Load x, y
        Op(PUSH0),
        Op(CALLDATALOAD), // x
        Op(PUSH1(hx!("20"))),
        Op(CALLDATALOAD), // x, y
        // Add and check for overflow
        Op(DUP2),                // x, y, x
        Op(ADD),                 // x, r
        Op(DUP1),                // x, r, r
        Op(SWAP2),               // r, r, x
        Op(GT),                  // r, x > r
        Asm::mref(empty_revert), // r, x > r, l
        Op(JUMPI),               // r
        // Return result.
        Op(MSIZE),
        Op(MSTORE),
        Op(MSIZE),
        Op(PUSH0),
        Op(RETURN),
        // Revert
        Mark(empty_revert),
        Op(JUMPDEST),
        Op(PUSH0),
        Op(PUSH0),
        Op(REVERT),
    ];

    let runtime_bytecode = assemble_full(&runtime, true).unwrap();

    let mut deploy_marks = MarkTracker::new();
    let runtime_start = deploy_marks.next();
    let runtime_end = deploy_marks.next();

    let deploy = vec![
        // Constructor
        Asm::delta_ref(runtime_start, runtime_end), // rt_size
        Op(DUP1),                                   // rt_size, rt_size
        Asm::mref(runtime_start),                   // rt_size, rt_size, rt_start
        Op(RETURNDATASIZE),                         // rt_size, rt_size, rt_start, 0
        Op(CODECOPY),                               // rt_size
        Op(RETURNDATASIZE),                         // rt_size, 0
        Op(RETURN),                                 // -- end
        // Runtime body
        Mark(runtime_start),
        Data(runtime_bytecode.clone()),
        Mark(runtime_end),
        // Random metadata padded to 32-bytes, will not be included in returned runtime bytecode.
        Asm::padded_back(32, vec![data!("49203c3320796f75203a29")]),
    ];

    let deploy_bytecode = assemble_full(&deploy, true).unwrap();

    println!("runtime bytecode: {}", hex::encode(runtime_bytecode));
    println!("deploy bytecode: {}", hex::encode(deploy_bytecode));
}
```

## API

The overarching API is that you create a list of assembly blocks (`Vec<Asm>`) and feed it into
`assembler::assemble_full` that then turns it into one long string of bytecode. Blocks and their 
types are found under `evm_glue::assembly`.

### Executing the assembler (`assembler::assemble_full`)

The `assemble_full` is the main entry point for assembling a string of assembly blocks, it takes two
arguments:
- `asm: &mut Vec<Asm>`: This is the array containing all the assembly
- `minimize_refs: bool`: A boolean flag indicating whether you want the enable the reference
  size minimization step. If set to `false` all references will use the same amount of bytes.


> [!IMPORTANT]
> Note that the passed `asm` object will be modified in-place, with the assemble step setting
> concrete sizes for all the references. Be sure to `.clone()` your `asm` object before passing it
> in if you wish to retain it in its original, size unset form.

### Op

Op "blocks" indicate individual opcodes. For PUSH1-PUSH32 operations this already includes their
immediate push value e.g.:

```rust
use evm_glue::assembly::Asm::*;
use evm_glue::opcodes::Opcode::*;
// `hex_literal` is a recommended helper library
use hex_literal::hex;

let asm = vec![
    Op(PUSH1(hx!("20"))),
    Op(CALLDATALOAD),
    Op(DUP2),
    Op(PUSH5(hex!("0000138302"))),
]
```

### Marks & References

Probably one of the most useful features of the assembler are its marks and references, these allow
you to easily construct jump labels, tables and code returns.

#### Marks

Marks generate 0 bytecode and are simply an indicator for the assembler. They consist of a simple
`usize` "mark id" (referenced as `mid` in the code):

```rust
use evm_glue::assembly::Asm::*;

let asm = vec![
    Mark(3)
];
```
> [!IMPORTANT]  
> If you're manually allocating mark IDs ensure that they start at 0 and stay in a concise range.
> This is for performance reasons as a simple `Vec` is used under the hood as a mark => offset map.


**`MarkTracker` helper**

The `MarkTracker` is a simple helper struct that allows you to easily allocate mark IDs without
worrying about duplicates:

```rust
use evm_glue::utils::MarkTracker;
use evm_glue::assembly::Asm::*;

let mut mt = MarkTracker::new();

let label1 = mt.next();       // 0
let empty_revert = mt.next(); // 1

let asm = vec![
    Mark(label1),
    // ...
    Mark(empty_revert),
    // ...
];

```

> [!WARNING]  
> Marks must be unique, marks with duplicate Mark IDs will result in an error.

#### Mark References

References are objects that allow for the insertion of mark offsets into the final bytecode at
compile time. This can be used to push jump labels to the stack or reference tables. There are
2 types of references:

- `Direct` references: a single mark directly and insert their bytecode offset at compile time
- `Delta` references: the offset between a `start` and `end` mark

Beyond that marks are also differentiated by their representation type. References can either be
`Pushed` meaning they'll generate an actual `PUSH1-PUSH32` opcode together with the reference value
at compile time or `Literal` meaning they'll be inserted into the bytecode standalone. The former is
useful for jump labels, code and data offsets while the later is useful for constructing e.g. jump
tables.

These also have helpers:

```rust
use evm_glue::assembly::Asm;

let asm = vec![
    Asm::delta_ref(runtime_start, runtime_end), // Pushed delta reference
    // ...,
    Asm::mref(runtime_start),                   // Pushed direct reference
    // ...,
    Asm::mark(runtime_start),
];
```

Literal helpers are `Asm::mref_literal` and `Asm::delta_ref_literal` respectively.

### Data

Data blocks are just continuous sequences of bytecode to be directly inserted into the final output.
This can be other compiled contracts, metadata or even lookup tables.

To insert a byte stream (`Vec<u8>`) as data you can use the `Asm::Data` enum member:

```rust
use evm_glue::assembly::Asm::*;

let runtime_bytecode: Vec<u8> = /* some bytecode */;

let asm = vec![
    Data(runtime_bytecode.clone())
];
```

**`data!` macro**

You can use the `evm_glue::data` macro to directly specify a hex literal as data:

```rust
use evm_glue::data;

let asm = vec![
    data!("0283")
];
```

### Padded Blocks

These are bytecode sections that you want to have a certain length, this can be the case if you're
generating a jump table. Padded blocks bad their sections with a specific byte, by default the
0-byte `0x00`. They can pad to the front or to the back.
 
To create padded blocks the following helpers are available:

- `Asm::padded_back(size: usize, blocks: Vec<Asm>) -> Asm`
- `Asm::padded_front(size: usize, blocks: Vec<Asm>) -> Asm`
- `Asm::padded_back_with(size: usize, padding: u8, blocks: Vec<Asm>) -> Asm`
- `Asm::padded_front_with(size: usize, padding: u8, blocks: Vec<Asm>) -> Asm`
