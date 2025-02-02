use crate::opcodes::Opcode;
use hex;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MarkRef {
    pub ref_type: RefType,
    pub is_pushed: bool,
    pub set_size: Option<u8>,
}

impl MarkRef {
    fn base_size(&self) -> usize {
        if self.is_pushed {
            1
        } else {
            0
        }
    }
}

/// An atomic unit of assembly
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Asm {
    /// A well typed EVM opcode. `Opcode::UNKNOWN(u8)` for unrecognized instructions.
    Op(Opcode),
    /// Raw bytes.
    Data(Vec<u8>),
    /// A sizeless marker that can be referenced. Does not generate any code.
    Mark(usize),
    /// A reference to marks that'll resolve to the byte offset of the referenced mark(s).
    Ref(MarkRef),
}

impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Data(d) => write!(f, "0x{}", hex::encode(d)),
            Self::Op(i) => write!(f, "{}", i),
            Self::Mark(mid) => write!(f, "#{}:", mid),
            Self::Ref(MarkRef {
                ref_type,
                is_pushed,
                set_size,
            }) => {
                if *is_pushed {
                    let size_str = if let Some(size) = set_size {
                        format!("{}", size)
                    } else {
                        String::new()
                    };
                    write!(f, "PUSH{} {}", size_str, ref_type)
                } else {
                    let size_str = if let Some(size) = set_size {
                        format!("{}:", size)
                    } else {
                        String::new()
                    };
                    write!(f, "({}{})", size_str, ref_type)
                }
            }
        }
    }
}

impl Asm {
    pub fn base_size(&self) -> usize {
        match self {
            Self::Op(i) => i.len(),
            Self::Data(d) => d.len(),
            Self::Mark(_) => 0,
            Self::Ref(mref) => mref.base_size(),
        }
    }

    pub fn mref(mid: usize) -> Self {
        Self::Ref(MarkRef {
            ref_type: RefType::Direct(mid),
            is_pushed: true,
            set_size: None,
        })
    }

    pub fn delta_ref(start_mid: usize, end_mid: usize) -> Self {
        Self::Ref(MarkRef {
            ref_type: RefType::Delta(start_mid, end_mid),
            is_pushed: true,
            set_size: None,
        })
    }

    pub fn mref_literal(mid: usize) -> Self {
        Self::Ref(MarkRef {
            ref_type: RefType::Direct(mid),
            is_pushed: false,
            set_size: None,
        })
    }

    pub fn delta_ref_literal(start_mid: usize, end_mid: usize) -> Self {
        Self::Ref(MarkRef {
            ref_type: RefType::Delta(start_mid, end_mid),
            is_pushed: false,
            set_size: None,
        })
    }

    pub fn set_mref(ref_type: RefType, is_pushed: bool, set_size: u8) -> Self {
        Self::Ref(MarkRef {
            ref_type,
            is_pushed,
            set_size: Some(set_size),
        })
    }
}

#[macro_export]
macro_rules! data {
    ($bytes:literal) => {{
        use hex_literal::hex;
        $crate::assembly::Asm::Data(hex!($bytes).into())
    }};
}

/// A shorthand for the [evm_asm_vec] macro, see its documentation for more details.
#[macro_export]
macro_rules! evm_asm {
    // Capture everything into a tuple and forward it to the internal handler
    ($($tt:tt)*) => {{
        let mut result = ::std::vec::Vec::new();
        $crate::evm_asm_vec!(result; $($tt)*);
        result
    }};
}

/// A macro for filling a [Vec] with EVM [Asm] instruction variants.
///
/// Rules:
/// - Shorthands for all [Asm] variants are supported directly. In order of precedence:
///     - `Mark($expr)` -> `Asm::Mark($expr)`
///     - `Data($expr)` -> `Asm::Data($expr)`
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
        $res.push($crate::assembly::Asm::Mark($expr));
        $crate::evm_asm_vec!($res; $($($rest)*)?);
    };
    ($res:ident; Data($lit:literal) $(, $($rest:tt)*)?) => {
        $res.push($crate::data!($lit));
        $crate::evm_asm_vec!($res; $($($rest)*)?);
    };
    ($res:ident; Data($expr:expr) $(, $($rest:tt)*)?) => {
        $res.push($crate::assembly::Asm::Data($expr));
        $crate::evm_asm_vec!($res; $($($rest)*)?);
    };
    ($res:ident; Ref($expr:expr) $(, $($rest:tt)*)?) => {
        $res.push($crate::assembly::Asm::Ref($expr));
        $crate::evm_asm_vec!($res; $($($rest)*)?);
    };
    ($res:ident; Op($expr:expr) $(, $($rest:tt)*)?) => {
        $res.push($crate::assembly::Asm::Op($expr));
        $crate::evm_asm_vec!($res; $($($rest)*)?);
    };
    // Match any qualified Asm variants
    ($res:ident; Asm::$variant:ident($($expr:expr),*) $(, $($rest:tt)*)?) => {
        $res.push($crate::assembly::Asm::$variant($($expr),*));
        $crate::evm_asm_vec!($res; $($($rest)*)?);
    };
    // Allow for passing in var idents.
    ($res:ident; VAR $var:ident $(, $($rest:tt)*)?) => {
        $res.push($var);
        $crate::evm_asm_vec!($res; $($($rest)*)?);
    };
    // Assume anything else is an opcode
    ($res:ident; $head:expr $(, $($rest:tt)*)?) => {
        // TODO: Could further pattern match to allow for ambiguous functions / other types of
        // exprs.
        $res.push($crate::assembly::Asm::Op($head));
        $crate::evm_asm_vec!($res; $($($rest)*)?);
    };
    // Terminal case: all tokens have been consumed
    ($res:ident;) => {};
}
