use crate::opcodes::Opcode;
use hex;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RefType {
    Direct(usize),
    Delta(usize, usize),
}

impl fmt::Display for RefType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Direct(mid) => write!(f, "#{}", mid),
            Self::Delta(start_mid, end_mid) => write!(f, "#{}-#{}", end_mid, start_mid),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PadSide {
    Front,
    Back,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MarkRef {
    pub size: Option<usize>,
    pub ref_type: RefType,
    pub is_pushed: bool,
}

impl MarkRef {
    fn static_size(&self) -> usize {
        if self.is_pushed {
            1
        } else {
            0
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Asm {
    Op(Opcode),
    Data(Vec<u8>),
    Mark(usize),
    PaddedBlock {
        size: usize,
        padding: u8,
        blocks: Vec<Asm>,
        side: PadSide,
    },
    Ref(MarkRef),
}

impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Data(d) => write!(f, "0x{}", hex::encode(d)),
            Op(i) => write!(f, "{}", i),
            Mark(mid) => write!(f, "#{}:", mid),
            Ref(MarkRef {
                ref_type,
                is_pushed,
                size: maybe_size,
            }) => match (is_pushed, maybe_size) {
                (false, Some(size)) => write!(f, "[{}] ({})", size, ref_type),
                (true, Some(size)) => write!(f, "PUSH{} {}", size, ref_type),
                (false, None) => write!(f, "({})", ref_type),
                (true, None) => write!(f, "PUSH {}", ref_type),
            },
            _ => write!(f, "{:?}", self),
        }
    }
}

use Asm::*;

pub fn for_each_mref_mut<F>(asm: &mut [Asm], f: &mut F)
where
    F: FnMut(&mut MarkRef),
{
    asm.iter_mut().for_each(|block| match block {
        Ref(mref) => f(mref),
        PaddedBlock { blocks, .. } => for_each_mref_mut(blocks, f),
        _ => {}
    })
}

pub fn for_each_mref<A, F>(asm: A, f: &mut F)
where
    A: AsRef<[Asm]>,
    F: FnMut(&MarkRef),
{
    asm.as_ref().iter().for_each(|block| match block {
        Ref(mref) => f(mref),
        PaddedBlock { blocks, .. } => for_each_mref(blocks, f),
        _ => {}
    })
}

impl Asm {
    pub fn static_size(&self) -> usize {
        match self {
            Op(i) => i.len(),
            Data(d) => d.len(),
            Mark(_) => 0,
            PaddedBlock { size, .. } => *size,
            Ref(mref) => mref.static_size(),
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            Ref(mref @ MarkRef { size, .. }) => size.map(|x| x + mref.static_size()),
            _ => Some(self.static_size()),
        }
    }

    pub fn mref(mid: usize) -> Self {
        Ref(MarkRef {
            ref_type: RefType::Direct(mid),
            is_pushed: true,
            size: None,
        })
    }

    pub fn delta_ref(start_mid: usize, end_mid: usize) -> Self {
        Ref(MarkRef {
            ref_type: RefType::Delta(start_mid, end_mid),
            is_pushed: true,
            size: None,
        })
    }

    pub fn mref_literal(mid: usize) -> Self {
        Ref(MarkRef {
            ref_type: RefType::Direct(mid),
            is_pushed: false,
            size: None,
        })
    }

    pub fn delta_ref_literal(start_mid: usize, end_mid: usize) -> Self {
        Ref(MarkRef {
            ref_type: RefType::Delta(start_mid, end_mid),
            is_pushed: false,
            size: None,
        })
    }

    pub fn padded_back(size: usize, blocks: Vec<Asm>) -> Self {
        PaddedBlock {
            size,
            padding: 0x00,
            blocks,
            side: PadSide::Back,
        }
    }

    pub fn padded_front(size: usize, blocks: Vec<Asm>) -> Self {
        PaddedBlock {
            size,
            padding: 0x00,
            blocks,
            side: PadSide::Front,
        }
    }

    pub fn padded_back_with(size: usize, padding: u8, blocks: Vec<Asm>) -> Self {
        PaddedBlock {
            size,
            padding,
            blocks,
            side: PadSide::Back,
        }
    }

    pub fn padded_front_with(size: usize, padding: u8, blocks: Vec<Asm>) -> Self {
        PaddedBlock {
            size,
            padding,
            blocks,
            side: PadSide::Front,
        }
    }
}

#[macro_export]
macro_rules! data {
    ($bytes:literal) => {{
        use hex_literal::hex;
        Asm::Data(hex!($bytes).into())
    }};
}

/// A shorthand for the [evm_asm_vec] macro, see its documentation for more details.
#[macro_export]
macro_rules! evm_asm {
    // Capture everything into a tuple and forward it to the internal handler
    ($($tt:tt)*) => {{
        let mut result = Vec::new();
        evm_asm_vec!(result; $($tt)*);
        result
    }};
}

/// A macro for filling a [Vec] with EVM [Asm] instruction variants.
///
/// Rules:
/// - Shorthands for all [Asm] variants are supported directly. In order of precedence:
///     - `Mark($expr)` -> `Asm::Mark($expr)`
///     - `Data($expr)` -> `Asm::Data($expr)`
///     - `PaddedBlock { $a:expr, $b:expr, $c:expr, $d: expr }` -> `Asm::PaddedBlock { size: $a, padding: $b, blocks: $c, side: $d }`
///     - `Ref($expr)` -> `Asm::Ref($expr)`
///     - `Op($expr)` -> `Asm::Op($expr)`
///     - `Asm::$variant($($expr:expr),*)` -> `Asm::$variant($($expr),*)`
///     - `VAR $var:ident` -> `$var`
///     - `$head:expr` -> `Asm::Op($head)`
///
/// **Example**
/// ```rust
/// use evm_glue::{assembly::{*, Asm::*}, opcodes::{*, Opcode::*}, utils::*, evm_asm, evm_asm_vec};
/// use hex_literal::hex;
///
/// let mut runtime_marks = MarkTracker::default();
/// let empty_revert = runtime_marks.next_mark();
///
/// let push0_var = Asm::Op(PUSH0);
/// let runtime = vec![
///     // Load x, y
///     push0_var.clone(),
///     Asm::Op(CALLDATALOAD),   // x
///     Asm::Op(PUSH1(hex!("20"))),
///     Asm::Op(CALLDATALOAD),   // x, y
///     // Add and check for overflow
///     Asm::Op(DUP2),           // x, y, x
///     Asm::Op(ADD),            // x, r
///     Asm::Op(DUP1),           // x, r, r
///     Asm::Op(SWAP2),          // r, r, x
///     Asm::Op(GT),             // r, x > r
///     Asm::mref(empty_revert), // r, x > r, l
///     Asm::Op(JUMPI),          // r
///     // Return result.
///     Asm::Op(MSIZE),
///     Asm::Op(MSTORE),
///     Asm::Op(MSIZE),
///     Asm::Op(PUSH0),
///     Asm::Op(RETURN),
///     // Revert
///     Asm::Mark(empty_revert),
///     Asm::Op(JUMPDEST),
///     Asm::Op(PUSH0),
///     Asm::Op(PUSH0),
///     Asm::Op(REVERT),
/// ];
/// let runtime_macro = evm_asm!(
///     // Load x, y
///     VAR push0_var,
///     CALLDATALOAD,            // x
///     PUSH1(hex!("20")),
///     CALLDATALOAD,            // x, y
///     // Add and check for overflow
///     DUP2,                    // x, y, x    
///     ADD,                     // x, r       
///     DUP1,                    // x, r, r    
///     SWAP2,                   // r, r, x    
///     GT,                      // r, x > r   
///     Asm::mref(empty_revert), // r, x > r, l
///     JUMPI,                   // r          
///     // Return result.
///     MSIZE,
///     MSTORE,
///     MSIZE,
///     PUSH0,
///     RETURN,
///     // Revert
///     Mark(empty_revert),
///     JUMPDEST,
///     PUSH0,
///     PUSH0,
///     REVERT,
/// );
/// assert_eq!(runtime_macro, runtime);
/// ```
#[macro_export]
macro_rules! evm_asm_vec {
    // Non-opcode variant matchers take precedent
    ($res:ident; Mark($expr:expr) $(, $($rest:tt)*)?) => {
        $res.push(Asm::Mark($expr));
        evm_asm_vec!($res; $($($rest)*)?);
    };
    ($res:ident; Data($lit:literal) $(, $($rest:tt)*)?) => {
        $res.push(data!($lit));
        evm_asm_vec!($res; $($($rest)*)?);
    };
    ($res:ident; PaddedBlock { $a:expr, $b:expr, $c:expr, $d: expr } $(, $($rest:tt)*)?) => {
        $res.push(Asm::PaddedBlock { size: $a, padding: $b, blocks: $c, side: $d });
        evm_asm_vec!($res; $($($rest)*)?);
    };
    ($res:ident; Ref($expr:expr) $(, $($rest:tt)*)?) => {
        $res.push(Ref($expr));
        evm_asm_vec!($res; $($($rest)*)?);
    };
    ($res:ident; Op($expr:expr) $(, $($rest:tt)*)?) => {
        $res.push(Op($expr));
        evm_asm_vec!($res; $($($rest)*)?);
    };
    // Match any qualified Asm variants
    ($res:ident; Asm::$variant:ident($($expr:expr),*) $(, $($rest:tt)*)?) => {
        $res.push(Asm::$variant($($expr),*));
        evm_asm_vec!($res; $($($rest)*)?);
    };
    // Allow for passing in var idents.
    ($res:ident; VAR $var:ident $(, $($rest:tt)*)?) => {
        $res.push($var);
        evm_asm_vec!($res; $($($rest)*)?);
    };
    // Assume anything else is an opcode
    ($res:ident; $head:expr $(, $($rest:tt)*)?) => {
        // TODO: Could further pattern match to allow for ambiguous functions / other types of
        // exprs.
        $res.push(Op($head));
        evm_asm_vec!($res; $($($rest)*)?);
    };
    // Terminal case: all tokens have been consumed
    ($res:ident;) => {};
}
