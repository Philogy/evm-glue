use crate::assembly::{Asm, PadSide, RefRepr, RefType};

fn get_last_data(asm: &mut Vec<Asm>) -> Option<&mut Vec<u8>> {
    match asm.last_mut() {
        Some(Asm::Data(ref mut d)) => Some(d),
        _ => None,
    }
}

pub fn reduce_asm(asm: &Vec<Asm>) -> Vec<Asm> {
    let mut reduced_asm = Vec::new();
    for block in asm {
        match block {
            Asm::Op(op) => {
                let new_bytes: &mut Vec<u8> = match get_last_data(&mut reduced_asm) {
                    Some(d) => d,
                    None => {
                        reduced_asm.push(Asm::Data(Vec::new()));
                        get_last_data(&mut reduced_asm).unwrap()
                    }
                };
                op.append_to(new_bytes);
            }
            Asm::Data(d) => match get_last_data(&mut reduced_asm) {
                Some(last_d) => last_d.extend(d),
                None => reduced_asm.push(block.clone()),
            },
            _ => reduced_asm.push(block.clone()),
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
        match block {
            Asm::Mark(mid) => {
                if !marks.add(*mid) {
                    return Err(AssembleError::DuplicateMark(*mid));
                }
            }
            Asm::PaddedBlock { blocks, .. } => {
                get_and_validate_marks(blocks, marks)?;
            }
            _ => {}
        }
    }

    Ok(())
}

type AssembleResult<T> = Result<T, AssembleError>;

fn validate_refs<F>(asm: &Vec<Asm>, validate_mid: &F) -> AssembleResult<()>
where
    F: Fn(usize) -> AssembleResult<()>,
{
    for block in asm {
        match block {
            Asm::Ref { ref_type, .. } => match ref_type {
                RefType::Direct(mid) => validate_mid(*mid)?,
                RefType::Delta(start_mid, end_mid) => {
                    validate_mid(*start_mid)?;
                    validate_mid(*end_mid)?;
                }
            },
            Asm::PaddedBlock { blocks, .. } => validate_refs(&blocks, validate_mid)?,
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

/// Assumes that the size for all refs in `asm` has been set.
fn validate_padding(asm: &Vec<Asm>) -> AssembleResult<usize> {
    asm.iter().fold(Ok(0), |maybe_offset, block| {
        let offset = maybe_offset?;
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
    asm.iter()
        .map(|block| match block {
            Asm::Ref { .. } => 1,
            Asm::PaddedBlock { blocks, .. } => get_total_refs(blocks),
            _ => 0,
        })
        .sum()
}

fn set_size_of_all_refs(asm: &mut Vec<Asm>, new_size: usize) {
    asm.iter_mut().for_each(|block| match block {
        Asm::PaddedBlock { blocks, .. } => set_size_of_all_refs(blocks, new_size),
        Asm::Ref { size, .. } => *size = Some(new_size),
        _ => {}
    });
}

pub fn set_minimum_ref_size(asm: &mut Vec<Asm>) {
    let total_static_size: usize = asm.iter().map(|b| b.static_size()).sum();
    let total_refs = get_total_refs(asm);

    let mut min_ref_size = 1;

    // Maximum offset that can be represented is 2^(8 * min_ref_size) - 1.
    // Total minimum code size is the base static size + the bytes required to store all offsets
    // (min_ref_size * total_refs).
    while (1 << (8 * min_ref_size)) - 1 < total_static_size + min_ref_size * total_refs {
        min_ref_size += 1;
    }

    return set_size_of_all_refs(asm, min_ref_size);
}

/// Expects `Asm` to have been validated with `validate_asm` and that the sizes for all refs have
/// been set.
fn build_mark_map(mmap: &mut MarkMap<usize>, asm: &Vec<Asm>, start_offset: usize) -> usize {
    asm.iter().fold(start_offset, |offset, block| {
        if let Asm::Mark(mid) = block {
            mmap.set(*mid, offset);
        } else if let Asm::PaddedBlock { blocks, .. } = block {
            build_mark_map(mmap, blocks, offset);
        }
        offset + block.size().expect("Unsized block in asm")
    })
}

fn assemble(bytecode: &mut Vec<u8>, mmap: &MarkMap<usize>, asm: &Vec<Asm>) {
    for block in asm {
        match block {
            Asm::Ref {
                size: maybe_size,
                ref_type,
                ref_repr,
            } => {
                let size = maybe_size.expect("Unsized block in asm");
                let value: usize = match ref_type {
                    RefType::Direct(mid) => mmap.get_direct(*mid),
                    RefType::Delta(start_mid, end_mid) => {
                        mmap.get_direct(*end_mid) - mmap.get_direct(*start_mid)
                    }
                };
                if let RefRepr::Pushed = ref_repr {
                    bytecode.push((0x60 + size - 1).try_into().unwrap());
                }
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

// Expects validated assembly, TODO: Fix validation to recurse into padded blocks
fn assemble_sized(asm: &Vec<Asm>) -> Result<Vec<u8>, AssembleError> {
    let mut mmap: MarkMap<usize> = MarkMap::new();
    // PaddedBlock size validation already done in `build_mark_map`.
    let end_offset = build_mark_map(&mut mmap, asm, 0);
    let mut bytecode: Vec<u8> = Vec::with_capacity(end_offset);

    assemble(&mut bytecode, &mmap, asm);

    Ok(bytecode)
}

pub fn assemble_full(asm: &mut Vec<Asm>) -> Result<Vec<u8>, AssembleError> {
    validate_asm(&asm)?;

    set_minimum_ref_size(asm);

    validate_padding(&asm)?;

    // TODO: Add reference push size reducer e.g. PUSH #2 => PUSH2 0x0027 => PUSH1 0x27
    assemble_sized(&asm)
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
        let mut asm = vec![
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

        let reduced = reduce_asm(&asm);

        println!("{:?}", reduced);

        println!();
        for block in &reduced {
            println!("{}", block);
        }

        let out = assemble_full(&mut asm).unwrap();

        println!("\n\ncompiled: {}", hex::encode(out));
    }
}
