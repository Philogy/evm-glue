use crate::assembly::{for_each_mref, for_each_mref_mut, Asm, MarkRef, PadSide, RefRepr, RefType};

struct MarkMap(Vec<Option<usize>>);

impl MarkMap {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn set(&mut self, idx: usize, offset: usize) -> bool {
        let arr = &mut self.0;

        if idx >= arr.len() {
            arr.resize(idx, None);
            arr.push(Some(offset));
            true
        } else {
            let new_set = arr[idx].is_none();
            arr[idx] = Some(offset);
            new_set
        }
    }

    fn get(&self, idx: usize) -> Option<usize> {
        match self.0.get(idx) {
            Some(v) => *v,
            None => None,
        }
    }

    fn get_unchecked(&self, idx: usize) -> usize {
        self.get(idx).expect("Contained invalid mark reference")
    }
}

impl MarkMap {
    fn lookup_value(&self, ref_type: &RefType) -> usize {
        match ref_type {
            RefType::Direct(mid) => self.get_unchecked(*mid),
            RefType::Delta(start_mid, end_mid) => {
                self.get_unchecked(*end_mid) - self.get_unchecked(*start_mid)
            }
        }
    }
}

#[derive(Debug)]
pub enum AssembleError {
    DuplicateMark(usize),
    InvalidMarkReference(usize),
    DeltaStartAfterEnd(usize, usize),
    BytecodeExceedsPadding,
}

fn get_and_validate_marks(
    mmap: &mut MarkMap,
    asm: &[Asm],
    start_offset: usize,
) -> Result<usize, AssembleError> {
    asm.iter().try_fold(start_offset, |offset, block| {
        if let Asm::Mark(mid) = block {
            if !mmap.set(*mid, offset) {
                return Err(AssembleError::DuplicateMark(*mid));
            }
        } else if let Asm::PaddedBlock { blocks, .. } = block {
            build_mark_map(mmap, blocks, offset);
        }
        Ok(offset + block.size().unwrap_or(0))
    })
}

pub type AssembleResult<T> = Result<T, AssembleError>;

fn validate_refs<F>(asm: &Vec<Asm>, validate_mid: &F) -> AssembleResult<()>
where
    F: Fn(usize) -> AssembleResult<usize>,
{
    for block in asm {
        match block {
            Asm::Ref(MarkRef { ref_type, .. }) => match ref_type {
                RefType::Direct(mid) => {
                    validate_mid(*mid)?;
                }
                RefType::Delta(start_mid, end_mid) => {
                    let start_offset = validate_mid(*start_mid)?;
                    let end_offset = validate_mid(*end_mid)?;
                    if end_offset < start_offset {
                        return Err(AssembleError::DeltaStartAfterEnd(*start_mid, *end_mid));
                    }
                }
            },
            Asm::PaddedBlock { blocks, .. } => validate_refs(blocks, validate_mid)?,
            _ => {}
        }
    }

    Ok(())
}

pub fn validate_asm(asm: &Vec<Asm>) -> AssembleResult<()> {
    let mut mmap = MarkMap::new();

    get_and_validate_marks(&mut mmap, asm, 0)?;

    validate_refs(asm, &|mid: usize| {
        mmap.get(mid)
            .ok_or(AssembleError::InvalidMarkReference(mid))
    })
}

/// Assumes that the size for all refs in `asm` has been set.
fn validate_padding(asm: &[Asm]) -> AssembleResult<usize> {
    asm.iter().try_fold(0, |offset, block| {
        if let Asm::PaddedBlock { size, blocks, .. } = block {
            let full_size = validate_padding(blocks)?;
            if full_size > *size {
                return Err(AssembleError::BytecodeExceedsPadding);
            }
        }
        Ok(offset + block.size().expect("Unsized block in asm"))
    })
}

fn get_total_refs(asm: &Vec<Asm>) -> usize {
    let mut total_refs = 0;
    for_each_mref(asm, &mut |_| total_refs += 1);
    total_refs
}

fn set_minimum_ref_size(asm: &mut Vec<Asm>) {
    let total_static_size: usize = asm.iter().map(|b| b.static_size()).sum();
    let total_refs = get_total_refs(asm);

    let mut min_ref_size = 1;

    // Maximum offset that can be represented is 2**(8 * min_ref_size).
    // Total minimum code size is the base static size + the bytes required to store all offsets
    // (min_ref_size * total_refs).
    while (1 << (8 * min_ref_size)) < total_static_size + min_ref_size * total_refs {
        min_ref_size += 1;
    }

    for_each_mref_mut(asm, &mut |mref| mref.size = Some(min_ref_size));
}

/// Expects `Asm` to have been validated with `validate_asm` and that the sizes for all refs have
/// been set.
fn build_mark_map(mmap: &mut MarkMap, asm: &[Asm], start_offset: usize) -> usize {
    asm.iter().fold(start_offset, |offset, block| {
        if let Asm::Mark(mid) = block {
            mmap.set(*mid, offset);
        } else if let Asm::PaddedBlock { blocks, .. } = block {
            build_mark_map(mmap, blocks, offset);
        }
        offset + block.size().unwrap_or(0)
    })
}

fn assemble(bytecode: &mut Vec<u8>, mmap: &MarkMap, asm: &Vec<Asm>) {
    for block in asm {
        match block {
            Asm::Ref(MarkRef {
                size: maybe_size,
                ref_type,
                ref_repr,
            }) => {
                let size = maybe_size.expect("Unsized block in asm");
                let value: usize = mmap.lookup_value(ref_type);
                if let RefRepr::Pushed = ref_repr {
                    bytecode.push((0x60 + size - 1).try_into().unwrap());
                }
                // TODO: Make value => bytes + padding more elegant.
                let bytes = value.to_be_bytes();
                if bytes.len() > size {
                    bytecode.extend(&bytes[(bytes.len() - size)..]);
                } else {
                    // Add padding
                    bytecode.resize((size - bytes.len()) - bytecode.len(), 0);
                    bytecode.extend(bytes);
                }
            }
            Asm::Op(op) => op.append_to(bytecode),
            Asm::Data(data) => bytecode.extend(data),
            Asm::Mark(_) => {}
            Asm::PaddedBlock {
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
        }
    }
}

fn min_ref_size(offset: usize) -> usize {
    let mut ref_size = 1;
    while (1 << (8 * ref_size)) - 1 < offset {
        ref_size += 1;
    }
    ref_size
}

fn minimize_ref_sizes(asm: &mut [Asm]) -> (MarkMap, usize) {
    let mut mmap = MarkMap::new();
    let mut total_size: usize = 0;

    let mut remaining_changes = true;
    while remaining_changes {
        remaining_changes = false;
        total_size = build_mark_map(&mut mmap, asm, 0);
        for_each_mref_mut(asm, &mut |mref| {
            let required_size = min_ref_size(mmap.lookup_value(&mref.ref_type));
            match mref.size {
                Some(inner_size) if inner_size == required_size => {}
                _ => {
                    mref.size = Some(required_size);
                    remaining_changes = true;
                }
            }
        });
    }

    (mmap, total_size)
}

pub fn assemble_full(asm: &mut Vec<Asm>, minimize_refs: bool) -> AssembleResult<Vec<u8>> {
    validate_asm(asm)?;

    let (mmap, total_size) = if minimize_refs {
        minimize_ref_sizes(asm)
    } else {
        set_minimum_ref_size(asm);

        let mut mmap = MarkMap::new();
        let total_size = build_mark_map(&mut mmap, asm, 0);

        (mmap, total_size)
    };

    validate_padding(asm)?;

    let mut bytecode = Vec::with_capacity(total_size);

    assemble(&mut bytecode, &mmap, asm);

    Ok(bytecode)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::assembly::Asm;
    use crate::data;
    use hex;
    use hex_literal::hex as hx;

    use crate::opcodes::Opcode::*;
    use Asm::*;

    #[test]
    fn test_reduce() {
        let asm = vec![
            Asm::delta_ref(0, 1),
            Op(DUP1),
            Asm::mref(0),
            Op(RETURNDATASIZE),
            Op(CODECOPY),
            Op(RETURNDATASIZE),
            Op(RETURN),
            Mark(0),
            Op(PUSH0),
            Op(CALLDATALOAD),
            Op(PUSH1(hx!("20"))),
            Op(CALLDATALOAD),
            Op(ADD),
            Op(MSIZE),
            Op(MSTORE),
            Op(MSIZE),
            Op(PUSH0),
            Op(RETURN),
            Mark(1),
            Asm::padded_back(64, vec![data!("010203")]),
        ];
        for block in &asm {
            println!("{}", block);
        }

        let out = assemble_full(&mut asm.clone(), true).unwrap();

        println!("\n\ncompiled: {}", hex::encode(&out));

        assert_eq!(out, assemble_full(&mut asm.clone(), false).unwrap());
    }

    #[test]
    fn test_adjusting_offset() {
        let asm = vec![
            Mark(0),
            Op(JUMPDEST),
            Data(vec![0; 256]),
            Asm::mref(0),
            Mark(1),
            Asm::mref(1),
        ];

        assert_eq!(assemble_full(&mut asm.clone(), true).unwrap(), hx!("5b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006000610103"));
        assert_eq!(assemble_full(&mut asm.clone(), false).unwrap(), hx!("5b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000610000610104"));
    }
}
