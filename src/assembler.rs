use crate::assembly::{Asm, MarkRef, RefType};

#[derive(Debug)]
struct MarkMap(Vec<Option<usize>>);

impl MarkMap {
    /// Builds a mark map assuming all references use the same `ref_extra_bytes`
    fn build(asm: &[Asm], ref_extra_bytes: u8) -> (Self, usize) {
        let mark_map_size = asm
            .iter()
            .filter_map(|chunk| match chunk {
                Asm::Mark(id) => Some(*id),
                _ => None,
            })
            .max()
            .map(|max_id| max_id + 1)
            .unwrap_or_default();
        let mut inner_mark_map = vec![None; mark_map_size];
        let total_size = asm.iter().enumerate().fold(0, |offset, (index, chunk)| {
            let new_offset = match chunk {
                Asm::Ref(_) => offset + chunk.size() + ref_extra_bytes as usize,
                Asm::Mark(id) => {
                    #[cfg(feature = "sanity-checks")]
                    if inner_mark_map[*id].is_some() {
                        panic!("Mark with duplicate id {} at index {}", id, index);
                    }
                    inner_mark_map[*id] = Some(offset);
                    offset
                }
                _ => offset + chunk.size(),
            };

            new_offset
        });

        (Self(inner_mark_map), total_size)
    }

    fn set_mark_offset(&mut self, index: usize, offset: usize) -> bool {
        let prev_offset = self.0[index];
        self.0[index] = Some(offset);
        prev_offset != Some(offset)
    }

    fn get_offset(&self, index: usize, id: usize) -> usize {
        self.0.get(id).and_then(|value| *value).unwrap_or_else(|| {
            #[cfg(feature = "sanity-checks")]
            {
                panic!("Reference to nonexistent mark {} at index {}", id, index)
            };

            #[cfg(not(feature = "sanity-checks"))]
            Default::default()
        })
    }

    fn lookup_rt(&self, index: usize, rt: &RefType) -> usize {
        match rt {
            RefType::Delta(start_id, end_id) => {
                let start_offset = self.get_offset(index, *start_id);
                let end_offset = self.get_offset(index, *end_id);
                #[cfg(feature = "sanity-checks")]
                if end_offset < start_offset {
                    panic!(
                        "Delta reference at {} has end offset {} (id: {}) before start {} (id: {})",
                        index, end_offset, end_id, start_offset, start_id
                    );
                }
                end_offset.wrapping_sub(start_offset)
            }
            RefType::Direct(id) => self.get_offset(index, *id),
        }
    }
}

fn max_ref_extra_bytes(known_size: usize, total_refs: usize) -> u8 {
    let mut ref_extra_bytes: u8 = 1;
    while 1 << (8 * ref_extra_bytes) < known_size + total_refs * (ref_extra_bytes as usize) {
        ref_extra_bytes += 1;
    }
    ref_extra_bytes
}

#[derive(Debug, PartialEq, Eq)]
pub enum AssembleError {
    InvalidSetSize { chunk_index: usize },
}

pub fn assemble_maximized(asm: &[Asm], allow_push0: bool) -> Result<Vec<u8>, AssembleError> {
    let total_refs = asm
        .iter()
        .filter(|chunk| matches!(chunk, Asm::Ref(_)))
        .count();
    let known_size: usize = asm.iter().map(|chunk| chunk.size()).sum();
    let max_ref_extra_bytes: u8 = max_ref_extra_bytes(known_size, total_refs);

    let (mark_map, total_size) = MarkMap::build(asm, max_ref_extra_bytes);
    let mut final_code = Vec::with_capacity(total_size);

    asm.iter().enumerate().try_for_each(|(i, chunk)| {
        match chunk {
            Asm::Op(op) => op.append_to(&mut final_code),
            Asm::Data(data) => final_code.extend(data),
            Asm::Mark(_) => {}
            Asm::Ref(MarkRef {
                ref_type,
                is_pushed,
                set_size,
            }) => {
                let value = mark_map.lookup_rt(i, ref_type);
                let ref_extra_bytes = set_size.unwrap_or(max_ref_extra_bytes);
                let min_extra_bytes = value_to_ref_extra_bytes(value, allow_push0);
                if ref_extra_bytes < min_extra_bytes {
                    return Err(AssembleError::InvalidSetSize { chunk_index: i });
                }
                if *is_pushed {
                    final_code.push(max_ref_extra_bytes + 0x5f);
                }
                let be_bytes = value.to_be_bytes();
                final_code.extend(&be_bytes[be_bytes.len() - max_ref_extra_bytes as usize..]);
            }
        };
        Ok(())
    })?;

    Ok(final_code)
}

const MAX_CHANGES: usize = 10_000;

pub fn assemble_minimized(asm: &[Asm], allow_push0: bool) -> Result<Vec<u8>, AssembleError> {
    let (mark_map, total_size) =
        {
            let total_refs = asm
                .iter()
                .filter(|chunk| matches!(chunk, Asm::Ref(_)))
                .count();
            let known_size: usize = asm.iter().map(|chunk| chunk.size()).sum();
            let ref_extra_bytes: u8 = max_ref_extra_bytes(known_size, total_refs);
            let (mut mark_map, mut total_size) = MarkMap::build(asm, ref_extra_bytes);

            let mut made_a_change = true;
            let mut change_count = 1;

            while made_a_change {
                (total_size, made_a_change) = asm.iter().enumerate().fold(
                    (0, false),
                    |(offset, made_a_change), (i, chunk)| match chunk {
                        Asm::Ref(MarkRef {
                            ref_type, set_size, ..
                        }) => {
                            let extra_size = set_size.map_or_else(
                                || {
                                    value_to_ref_extra_bytes(
                                        mark_map.lookup_rt(i, ref_type),
                                        allow_push0,
                                    ) as usize
                                },
                                |_| 0,
                            );

                            (offset + chunk.size() + extra_size, made_a_change)
                        }
                        #[allow(clippy::identity_op)]
                        Asm::Mark(id) => (
                            offset + 0,
                            made_a_change || mark_map.set_mark_offset(*id, offset),
                        ),
                        _ => (offset + chunk.size(), made_a_change),
                    },
                );
                change_count += 1;
                assert!(
                    change_count <= MAX_CHANGES,
                    "Max changes exceeded, likely infinite loop, report bug"
                );
            }

            (mark_map, total_size)
        };

    let mut final_code = Vec::with_capacity(total_size);

    asm.iter().enumerate().try_for_each(|(i, chunk)| {
        match chunk {
            Asm::Op(op) => op.append_to(&mut final_code),
            Asm::Data(data) => final_code.extend(data),
            Asm::Mark(_) => {}
            Asm::Ref(MarkRef {
                ref_type,
                is_pushed,
                set_size,
            }) => {
                let value = mark_map.lookup_rt(i, ref_type);
                let min_extra_bytes = value_to_ref_extra_bytes(value, allow_push0);
                let ref_extra_bytes = set_size.unwrap_or_else(|| min_extra_bytes);
                if ref_extra_bytes < min_extra_bytes {
                    return Err(AssembleError::InvalidSetSize { chunk_index: i });
                }
                if *is_pushed {
                    final_code.push(ref_extra_bytes + 0x5f);
                }
                let be_bytes = value.to_be_bytes();
                final_code.extend(&be_bytes[be_bytes.len() - ref_extra_bytes as usize..]);
            }
        };
        Ok(())
    })?;

    Ok(final_code)
}

fn value_to_ref_extra_bytes(value: usize, allow_push0: bool) -> u8 {
    match (value, allow_push0) {
        (0, true) => 0,
        (0, false) => 1,
        (x, _) => x.checked_ilog2().unwrap() as u8 / 8 + 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::assembly::Asm;
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
        ];
        for block in &asm {
            println!("{}", block);
        }

        let min_out = assemble_minimized(&asm, true);
        let max_out = assemble_maximized(&asm, false);

        assert_eq!(min_out, max_out);
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

        let min_out_push0 = assemble_minimized(&asm, true).unwrap();
        let min_out = assemble_minimized(&asm, false).unwrap();
        let max_out = assemble_maximized(&asm, false).unwrap();

        assert_eq!(min_out_push0, hx!("5b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005f610102"), "minimized not equal");
        assert_eq!(min_out, hx!("5b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006000610103"), "minimized not equal");
        assert_eq!(max_out, hx!("5b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000610000610104"), "maximized not equal");
    }

    #[test]
    fn test_value_to_ref_extra_bytes() {
        assert_eq!(value_to_ref_extra_bytes(0, true), 0);
        assert_eq!(value_to_ref_extra_bytes(0, false), 1);

        assert_eq!(value_to_ref_extra_bytes(1, false), 1);
        assert_eq!(value_to_ref_extra_bytes(2, false), 1);
        assert_eq!(value_to_ref_extra_bytes(3, false), 1);
        assert_eq!(value_to_ref_extra_bytes(4, false), 1);
        assert_eq!(value_to_ref_extra_bytes(5, false), 1);
        assert_eq!(value_to_ref_extra_bytes(6, false), 1);
        assert_eq!(value_to_ref_extra_bytes(7, false), 1);
        assert_eq!(value_to_ref_extra_bytes(8, false), 1);
        assert_eq!(value_to_ref_extra_bytes(256, false), 2);
        assert_eq!(value_to_ref_extra_bytes(65535, false), 2);
        assert_eq!(value_to_ref_extra_bytes(65536, false), 3);

        assert_eq!(value_to_ref_extra_bytes(1, true), 1);
        assert_eq!(value_to_ref_extra_bytes(2, true), 1);
        assert_eq!(value_to_ref_extra_bytes(3, true), 1);
        assert_eq!(value_to_ref_extra_bytes(4, true), 1);
        assert_eq!(value_to_ref_extra_bytes(5, true), 1);
        assert_eq!(value_to_ref_extra_bytes(6, true), 1);
        assert_eq!(value_to_ref_extra_bytes(7, true), 1);
        assert_eq!(value_to_ref_extra_bytes(8, true), 1);
        assert_eq!(value_to_ref_extra_bytes(256, true), 2);
        assert_eq!(value_to_ref_extra_bytes(65536, true), 3);
    }
}
