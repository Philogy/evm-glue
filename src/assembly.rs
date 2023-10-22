use crate::opcodes::Opcode;
use crate::utils::debug_as_display;
use hex;
use std::fmt;

#[derive(Clone, Debug)]
pub enum RefRepr {
    Pushed,
    Literal,
}

use RefRepr::*;

debug_as_display!(RefRepr);

impl RefRepr {
    pub fn static_size(&self) -> usize {
        match self {
            Pushed => 1,
            Literal => 0,
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum PadSide {
    Front,
    Back,
}

#[derive(Clone, Debug)]
pub struct MarkRef {
    pub size: Option<usize>,
    pub ref_type: RefType,
    pub ref_repr: RefRepr,
}

#[derive(Clone, Debug)]
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
                ref_repr,
                size: maybe_size,
            }) => match (ref_repr, maybe_size) {
                (Literal, Some(size)) => write!(f, "[{}] ({})", size, ref_type),
                (Pushed, Some(size)) => write!(f, "PUSH{} {}", size, ref_type),
                (Literal, None) => write!(f, "({})", ref_type),
                (Pushed, None) => write!(f, "PUSH {}", ref_type),
            },
            _ => write!(f, "{:?}", self),
        }
    }
}

use Asm::*;

pub fn for_each_mref_mut<F>(asm: &mut Vec<Asm>, f: &mut F)
where
    F: FnMut(&mut MarkRef) -> (),
{
    asm.iter_mut().for_each(|block| match block {
        Ref(mref) => f(mref),
        PaddedBlock { blocks, .. } => for_each_mref_mut(blocks, f),
        _ => {}
    })
}

pub fn for_each_mref<F>(asm: &Vec<Asm>, f: &mut F)
where
    F: FnMut(&MarkRef) -> (),
{
    asm.iter().for_each(|block| match block {
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
            Ref(MarkRef { ref_repr, .. }) => ref_repr.static_size(),
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            Ref(MarkRef { ref_repr, size, .. }) => size.map(|x| x + ref_repr.static_size()),
            _ => Some(self.static_size()),
        }
    }

    pub fn mref(mid: usize) -> Self {
        Ref(MarkRef {
            ref_type: RefType::Direct(mid),
            ref_repr: RefRepr::Pushed,
            size: None,
        })
    }

    pub fn delta_ref(start_mid: usize, end_mid: usize) -> Self {
        Ref(MarkRef {
            ref_type: RefType::Delta(start_mid, end_mid),
            ref_repr: RefRepr::Pushed,
            size: None,
        })
    }

    pub fn mref_literal(mid: usize) -> Self {
        Ref(MarkRef {
            ref_type: RefType::Direct(mid),
            ref_repr: RefRepr::Literal,
            size: None,
        })
    }

    pub fn delta_ref_literal(start_mid: usize, end_mid: usize) -> Self {
        Ref(MarkRef {
            ref_type: RefType::Delta(start_mid, end_mid),
            ref_repr: RefRepr::Literal,
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
