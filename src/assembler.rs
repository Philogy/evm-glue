use crate::assembly::{
    Asm, FullMarkRef, MarkReference as MRef, PadSide, ReferenceRepresentation as RR, StaticSizeAsm,
};

fn get_last_data(asm: &mut Vec<Asm>) -> Option<&mut Vec<u8>> {
    match asm.last_mut() {
        Some(Asm::Sized(StaticSizeAsm::Data(ref mut d))) => Some(d),
        _ => None,
    }
}

pub fn reduce_asm(asm: &Vec<Asm>) -> Vec<Asm> {
    let mut reduced_asm = Vec::new();
    for block in asm {
        match block {
            Asm::Sized(sized) => match sized {
                StaticSizeAsm::Op(op) => {
                    let new_bytes: &mut Vec<u8> = match get_last_data(&mut reduced_asm) {
                        Some(d) => d,
                        None => {
                            reduced_asm.push(Asm::data(Vec::new()));
                            get_last_data(&mut reduced_asm).unwrap()
                        }
                    };
                    op.append_to(new_bytes);
                }
                StaticSizeAsm::Data(d) => match get_last_data(&mut reduced_asm) {
                    Some(last_d) => last_d.extend(d),
                    None => reduced_asm.push(block.clone()),
                },
                _ => reduced_asm.push(block.clone()),
            },
            Asm::Unsized(_) => reduced_asm.push(block.clone()),
        }
    }
    return reduced_asm;
}

struct MarkMap<T>(Vec<Option<T>>);

impl<T: Default + Clone> MarkMap<T> {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn add(&mut self, idx: usize) -> bool {
        self.set(idx, T::default())
    }

    fn set(&mut self, idx: usize, value: T) -> bool {
        let arr: &mut Vec<Option<T>> = &mut self.0;

        if idx >= arr.len() {
            arr.resize(idx, None);
            arr.push(Some(value));
            true
        } else {
            let new_set = arr[idx].is_none();
            arr[idx] = Some(value);
            new_set
        }
    }

    fn get(&self, idx: usize) -> &Option<T> {
        match self.0.get(idx) {
            Some(v) => v,
            None => &None,
        }
    }

    fn get_direct(&self, idx: usize) -> T {
        self.get(idx)
            .clone()
            .expect("Contained invalid mark reference")
    }

    fn contains(&self, idx: usize) -> bool {
        if let Some(Some(_)) = self.0.get(idx) {
            true
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub enum AssembleError {
    DuplicateMark(usize),
    InvalidMarkReference(usize),
    BytecodeExceedsPadding,
}

fn get_and_validate_marks(asm: &Vec<Asm>, marks: &mut MarkMap<usize>) -> Result<(), AssembleError> {
    for block in asm {
        if let Asm::Sized(iblock) = block {
            match iblock {
                StaticSizeAsm::Mark(mid) => {
                    if !marks.add(*mid) {
                        return Err(AssembleError::DuplicateMark(*mid));
                    }
                }
                StaticSizeAsm::PaddedBlock { blocks, .. } => {
                    get_and_validate_marks(blocks, marks)?;
                }
                _ => {}
            }
        }
    }

    Ok(())
}

pub enum SizedAsm {
    FixedSize(StaticSizeAsm<SizedAsm>),
    VarSized { full_ref: FullMarkRef, size: usize },
}

impl SizedAsm {
    pub fn size(&self) -> usize {
        match self {
            Self::FixedSize(ss_asm) => ss_asm.static_size(),
            Self::VarSized { size, full_ref } => size + full_ref.rr.static_size(),
        }
    }
}

type AssembleResult<T> = Result<T, AssembleError>;

fn validate_refs<F>(asm: &Vec<Asm>, validate_mid: &F) -> AssembleResult<()>
where
    F: Fn(usize) -> AssembleResult<()>,
{
    for block in asm {
        match block {
            Asm::Unsized(FullMarkRef { mref, .. }) => match mref {
                MRef::Direct(mid) => validate_mid(*mid)?,
                MRef::Delta(start_mid, end_mid) => {
                    validate_mid(*start_mid)?;
                    validate_mid(*end_mid)?;
                }
            },
            Asm::Sized(StaticSizeAsm::PaddedBlock { blocks, .. }) => {
                validate_refs(&blocks, validate_mid)?
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn validate_asm(asm: &Vec<Asm>) -> AssembleResult<()> {
    let mut marks: MarkMap<usize> = MarkMap::new();

    get_and_validate_marks(asm, &mut marks)?;

    validate_refs(asm, &|mid: usize| {
        if marks.contains(mid) {
            Ok(())
        } else {
            Err(AssembleError::InvalidMarkReference(mid))
        }
    })
}

fn validate_padding(asm: &Vec<SizedAsm>) -> AssembleResult<usize> {
    asm.iter().fold(Ok(0), |maybe_offset, block| {
        let offset = maybe_offset?;
        if let SizedAsm::FixedSize(StaticSizeAsm::PaddedBlock { size, blocks, .. }) = block {
            let full_size = validate_padding(blocks)?;
            if full_size > *size {
                return Err(AssembleError::BytecodeExceedsPadding);
            }
        }
        Ok(offset + block.size())
    })
}

fn get_total_refs(asm: &Vec<Asm>) -> usize {
    asm.iter()
        .map(|block| match block {
            Asm::Unsized(FullMarkRef { .. }) => 1,
            Asm::Sized(StaticSizeAsm::PaddedBlock { blocks, .. }) => get_total_refs(blocks),
            _ => 0,
        })
        .sum()
}

fn set_asm_size(asm: &Vec<Asm>, size: usize) -> Vec<SizedAsm> {
    asm.iter()
        .map(|block| match block {
            Asm::Sized(ss_asm) => SizedAsm::FixedSize(match ss_asm {
                StaticSizeAsm::PaddedBlock {
                    size,
                    padding,
                    blocks,
                    side,
                } => StaticSizeAsm::PaddedBlock {
                    size: *size,
                    padding: *padding,
                    blocks: set_asm_size(blocks, *size),
                    side: side.clone(),
                },
                StaticSizeAsm::Op(i) => StaticSizeAsm::Op(*i),
                StaticSizeAsm::Mark(mid) => StaticSizeAsm::Mark(*mid),
                StaticSizeAsm::Data(d) => StaticSizeAsm::Data(d.clone()),
            }),
            Asm::Unsized(full_ref) => SizedAsm::VarSized {
                full_ref: full_ref.clone(),
                size,
            },
        })
        .collect()
}

// Making assembly "sized" should maybe be a type conversion, whereby the refs in the base `Asm`
// enum do not have sized but they do in their "sized" counterpart. This would allow for compile
// time type checking to differentiate between sized/unsized, however this would seemingly also
// lead to a lot of code duplication
pub fn size_asm(asm: &Vec<Asm>) -> Vec<SizedAsm> {
    let total_static_size: usize = asm.iter().map(|b| b.static_size()).sum();
    let total_refs = get_total_refs(asm);

    let mut min_ref_size = 1;

    while (1 << (8 * min_ref_size)) - 1 < total_static_size + min_ref_size * total_refs {
        min_ref_size += 1;
    }

    return set_asm_size(asm, min_ref_size);
}

/// Expects `asm` to have been validated with
fn build_mark_map(mmap: &mut MarkMap<usize>, asm: &Vec<SizedAsm>, start_offset: usize) -> usize {
    asm.iter().fold(start_offset, |offset, block| {
        if let SizedAsm::FixedSize(ss_asm) = block {
            if let StaticSizeAsm::Mark(mid) = ss_asm {
                mmap.set(*mid, offset);
            } else if let StaticSizeAsm::PaddedBlock { blocks, .. } = ss_asm {
                build_mark_map(mmap, blocks, offset);
            }
        }
        offset + block.size()
    })
}

fn assemble(bytecode: &mut Vec<u8>, mmap: &MarkMap<usize>, asm: &Vec<SizedAsm>) {
    for block in asm {
        match block {
            SizedAsm::VarSized {
                full_ref: FullMarkRef { mref, rr },
                size,
            } => {
                let value: usize = match mref {
                    MRef::Direct(mid) => mmap.get_direct(*mid),
                    MRef::Delta(start_mid, end_mid) => {
                        mmap.get_direct(*end_mid) - mmap.get_direct(*start_mid)
                    }
                };
                if let RR::Pushed = rr {
                    bytecode.push((0x60 + size - 1).try_into().unwrap());
                }
                let bytes = value.to_be_bytes();
                if bytes.len() > *size {
                    bytecode.extend(&bytes[(bytes.len() - size)..]);
                } else {
                    // Add padding
                    bytecode.resize((size - bytes.len()) - bytecode.len(), 0);
                    bytecode.extend(bytes);
                }
            }
            SizedAsm::FixedSize(ss_asm) => match ss_asm {
                StaticSizeAsm::Op(op) => op.append_to(bytecode),
                StaticSizeAsm::Data(data) => bytecode.extend(data),
                StaticSizeAsm::Mark(_) => {}
                StaticSizeAsm::PaddedBlock {
                    size,
                    padding,
                    blocks,
                    side,
                } => {
                    match side {
                        PadSide::Back => {
                            let len_before = bytecode.len();
                            assemble(bytecode, mmap, blocks);
                            // Add padding
                            bytecode.resize(size + len_before, *padding);
                        }
                        PadSide::Front => {
                            let mut block_code: Vec<u8> = Vec::with_capacity(*size);
                            assemble(&mut block_code, mmap, blocks);
                            bytecode.resize(bytecode.len() + size - block_code.len(), *padding);
                            bytecode.extend(block_code);
                        }
                    }
                }
            },
        }
    }
}

// Expects validated assembly, TODO: Fix validation to recurse into padded blocks
fn assemble_sized(asm: &Vec<SizedAsm>) -> Result<Vec<u8>, AssembleError> {
    let mut mmap: MarkMap<usize> = MarkMap::new();
    // PaddedBlock size validation already done in `build_mark_map`.
    let end_offset = build_mark_map(&mut mmap, asm, 0);
    let mut bytecode: Vec<u8> = Vec::with_capacity(end_offset);

    assemble(&mut bytecode, &mmap, asm);

    Ok(bytecode)
}

pub fn assemble_full(asm: &Vec<Asm>) -> Result<Vec<u8>, AssembleError> {
    validate_asm(asm)?;

    let sized = size_asm(&asm);

    validate_padding(&sized)?;

    // TODO: Add reference push size reducer e.g. PUSH #2 => PUSH2 0x0027 => PUSH1 0x27
    assemble_sized(&sized)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::assembly::Asm;
    use crate::data;
    use hex;
    use hex_literal::hex as hx;

    use crate::opcodes::Opcode::*;

    #[test]
    fn test_reduce() {
        let asm = vec![
            Asm::delta_ref(0, 1),
            Asm::op(DUP1),
            Asm::mref(0),
            Asm::op(RETURNDATASIZE),
            Asm::op(CODECOPY),
            Asm::op(RETURNDATASIZE),
            Asm::op(RETURN),
            Asm::mark(0),
            Asm::op(PUSH0),
            Asm::op(CALLDATALOAD),
            Asm::op(PUSH1(hx!("20"))),
            Asm::op(CALLDATALOAD),
            Asm::op(ADD),
            Asm::op(MSIZE),
            Asm::op(MSTORE),
            Asm::op(MSIZE),
            Asm::op(PUSH0),
            Asm::op(RETURN),
            Asm::mark(1),
            Asm::padded_back(64, vec![data!("010203")]),
        ];
        for block in &asm {
            println!("{}", block);
        }

        let reduced = reduce_asm(&asm);

        println!("{:?}", reduced);

        println!();
        for block in &reduced {
            println!("{}", block);
        }

        let out = assemble_sized(&size_asm(&reduced)).unwrap();

        println!("\n\ncompiled: {}", hex::encode(out));
    }
}
