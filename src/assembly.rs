use crate::opcodes::Opcode;
use crate::utils::debug_as_display;
use hex;
use std::fmt;

#[derive(Clone, Debug)]
pub enum ReferenceRepresentation {
    Pushed,
    Literal,
}

use ReferenceRepresentation::*;

debug_as_display!(ReferenceRepresentation);

impl ReferenceRepresentation {
    pub fn static_size(&self) -> usize {
        match self {
            Pushed => 1,
            Literal => 0,
        }
    }
}

#[derive(Clone, Debug)]
pub enum MarkReference {
    Direct(usize),
    Delta(usize, usize),
}

impl fmt::Display for MarkReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Direct(mid) => write!(f, "#{}", mid),
            Self::Delta(start_mid, end_mid) => write!(f, "#{}-#{}", end_mid, start_mid),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FullMarkRef {
    pub mref: MarkReference,
    pub rr: ReferenceRepresentation,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PadSide {
    Front,
    Back,
}

#[derive(Clone, Debug)]
pub enum StaticSizeAsm<T> {
    Op(Opcode),
    Data(Vec<u8>),
    Mark(usize),
    PaddedBlock {
        size: usize,
        padding: u8,
        blocks: Vec<T>,
        side: PadSide,
    },
}

use StaticSizeAsm::*;

impl<T> StaticSizeAsm<T> {
    pub fn static_size(&self) -> usize {
        match self {
            Op(i) => i.len(),
            Data(d) => d.len(),
            Mark(_) => 0,
            PaddedBlock { size, .. } => *size,
        }
    }
}

impl fmt::Display for StaticSizeAsm<Asm> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Data(d) => write!(f, "0x{}", hex::encode(d)),
            Op(i) => write!(f, "{}", i),
            Mark(mid) => write!(f, "#{}:", mid),
            _ => write!(f, "{:?}", self),
        }
    }
}

// Uglily nested so that `StaticSizeAsm` can be reused later, wanted to avoid having to redefine
// what are essentially the same 4 enum variants two times.
#[derive(Clone, Debug)]
pub enum Asm {
    Sized(StaticSizeAsm<Asm>),
    Unsized(FullMarkRef),
}

impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sized(block) => write!(f, "{}", block),
            Unsized(FullMarkRef { mref, rr }) => match rr {
                Literal => write!(f, "({})", mref),
                Pushed => write!(f, "PUSH {}", mref),
            },
        }
    }
}

use Asm::*;

impl Asm {
    pub fn static_size(&self) -> usize {
        match self {
            Sized(block) => block.static_size(),
            Unsized(FullMarkRef { rr, .. }) => rr.static_size(),
        }
    }

    pub fn data(d: Vec<u8>) -> Self {
        Sized(Data(d))
    }

    pub fn op(i: Opcode) -> Self {
        Sized(Op(i))
    }

    pub fn mark(mid: usize) -> Self {
        Sized(Mark(mid))
    }

    pub fn mref(mid: usize) -> Self {
        Unsized(FullMarkRef {
            mref: MarkReference::Direct(mid),
            rr: ReferenceRepresentation::Pushed,
        })
    }

    pub fn delta_ref(start_mid: usize, end_mid: usize) -> Self {
        Unsized(FullMarkRef {
            mref: MarkReference::Delta(start_mid, end_mid),
            rr: ReferenceRepresentation::Pushed,
        })
    }

    pub fn mref_literal(mid: usize) -> Self {
        Unsized(FullMarkRef {
            mref: MarkReference::Direct(mid),
            rr: ReferenceRepresentation::Literal,
        })
    }

    pub fn delta_ref_literal(start_mid: usize, end_mid: usize) -> Self {
        Unsized(FullMarkRef {
            mref: MarkReference::Delta(start_mid, end_mid),
            rr: ReferenceRepresentation::Literal,
        })
    }

    pub fn padded_back(size: usize, blocks: Vec<Asm>) -> Self {
        Sized(PaddedBlock {
            size,
            padding: 0x00,
            blocks,
            side: PadSide::Back,
        })
    }

    pub fn padded_front(size: usize, blocks: Vec<Asm>) -> Self {
        Sized(PaddedBlock {
            size,
            padding: 0x00,
            blocks,
            side: PadSide::Front,
        })
    }

    pub fn padded_back_with(size: usize, padding: u8, blocks: Vec<Asm>) -> Self {
        Sized(PaddedBlock {
            size,
            padding,
            blocks,
            side: PadSide::Back,
        })
    }

    pub fn padded_front_with(size: usize, padding: u8, blocks: Vec<Asm>) -> Self {
        Sized(PaddedBlock {
            size,
            padding,
            blocks,
            side: PadSide::Front,
        })
    }
}

#[macro_export]
macro_rules! data {
    ($bytes:literal) => {{
        use hex_literal::hex;
        Asm::data(hex!($bytes).into())
    }};
}
