use hex;
use std::fmt;

macro_rules! push_extra_data {
    ($self:expr, $stream:expr, $( $push:ident ),*) => {
        match $self {
            $(
                Opcode::$push(bytes) => $stream.extend_from_slice(bytes),
            )*
            _ => {}
        }
    };
}

macro_rules! push_match_formats {
    ( $( $push:ident ),*) => {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                $(
                    Self::$push(d) => write!(f, "{} 0x{}", stringify!($push), hex::encode(&d)),
                )*
                _ => write!(f, "{:?}", self)
            }
        }
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Opcode {
    STOP,
    // Basic Arithmetic 0x01 - 0x0b
    ADD,
    MUL,
    SUB,
    DIV,
    SDIV,
    MOD,
    SMOD,
    ADDMOD,
    MULMOD,
    EXP,
    SIGNEXTEND,
    // Logic & Bitwise Operations 0x10 - 0x1d
    LT,
    GT,
    SLT,
    SGT,
    EQ,
    ISZERO,
    AND,
    OR,
    XOR,
    NOT,
    BYTE,
    SHL,
    SHR,
    SAR,
    // Keccak 0x20
    SHA3,
    // Call context introspection 0x30 - 0x3f
    ADDRESS,
    BALANCE,
    ORIGIN,
    CALLER,
    CALLVALUE,
    CALLDATALOAD,
    CALLDATASIZE,
    CALLDATACOPY,
    CODESIZE,
    CODECOPY,
    GASPRICE,
    EXTCODESIZE,
    EXTCODECOPY,
    RETURNDATASIZE,
    RETURNDATACOPY,
    EXTCODEHASH,
    // Block context introspection 0x40 - 0x4a
    BLOCKHASH,
    COINBASE,
    TIMESTAMP,
    NUMBER,
    PREVRANDAO,
    GASLIMIT,
    CHAINID,
    SELFBALANCE,
    BASEFEE,
    BLOBHASH,
    BLOBBASEFEE,
    // Control flow, Storage & Memory manipulation, runtime inspection 0x50 - 0x5e
    POP,
    MLOAD,
    MSTORE,
    MSTORE8,
    SLOAD,
    SSTORE,
    JUMP,
    JUMPI,
    PC,
    MSIZE,
    GAS,
    JUMPDEST,
    TLOAD,
    TSTORE,
    MCOPY,
    // Static pushes 0x5f - 0x6f
    PUSH0,
    PUSH1([u8; 1]),
    PUSH2([u8; 2]),
    PUSH3([u8; 3]),
    PUSH4([u8; 4]),
    PUSH5([u8; 5]),
    PUSH6([u8; 6]),
    PUSH7([u8; 7]),
    PUSH8([u8; 8]),
    PUSH9([u8; 9]),
    PUSH10([u8; 10]),
    PUSH11([u8; 11]),
    PUSH12([u8; 12]),
    PUSH13([u8; 13]),
    PUSH14([u8; 14]),
    PUSH15([u8; 15]),
    PUSH16([u8; 16]),
    PUSH17([u8; 17]),
    PUSH18([u8; 18]),
    PUSH19([u8; 19]),
    PUSH20([u8; 20]),
    PUSH21([u8; 21]),
    PUSH22([u8; 22]),
    PUSH23([u8; 23]),
    PUSH24([u8; 24]),
    PUSH25([u8; 25]),
    PUSH26([u8; 26]),
    PUSH27([u8; 27]),
    PUSH28([u8; 28]),
    PUSH29([u8; 29]),
    PUSH30([u8; 30]),
    PUSH31([u8; 31]),
    PUSH32([u8; 32]),
    // Dups 0x80 - 0x8f
    DUP1,
    DUP2,
    DUP3,
    DUP4,
    DUP5,
    DUP6,
    DUP7,
    DUP8,
    DUP9,
    DUP10,
    DUP11,
    DUP12,
    DUP13,
    DUP14,
    DUP15,
    DUP16,
    // Swaps 0x90 - 0x9f
    SWAP1,
    SWAP2,
    SWAP3,
    SWAP4,
    SWAP5,
    SWAP6,
    SWAP7,
    SWAP8,
    SWAP9,
    SWAP10,
    SWAP11,
    SWAP12,
    SWAP13,
    SWAP14,
    SWAP15,
    SWAP16,
    // Logging 0xa0 - 0xa4
    LOG0,
    LOG1,
    LOG2,
    LOG3,
    LOG4,
    // Calls 0xf0 - 0xff
    CREATE,
    CALL,
    CALLCODE,
    RETURN,
    DELEGATECALL,
    CREATE2,
    STATICCALL,
    REVERT,
    INVALID,
    SELFDESTRUCT,

    UNKNOWN(u8),
}

impl std::str::FromStr for Opcode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "stop" => Ok(Self::STOP),
            // Basic Arithmetic 0x01 - 0x0b
            "add" => Ok(Self::ADD),
            "mul" => Ok(Self::MUL),
            "sub" => Ok(Self::SUB),
            "div" => Ok(Self::DIV),
            "sdiv" => Ok(Self::SDIV),
            "mod" => Ok(Self::MOD),
            "smod" => Ok(Self::SMOD),
            "addmod" => Ok(Self::ADDMOD),
            "mulmod" => Ok(Self::MULMOD),
            "exp" => Ok(Self::EXP),
            "signextend" => Ok(Self::SIGNEXTEND),
            // Logic & Bitwise Operations 0x10 - 0x1d
            "lt" => Ok(Self::LT),
            "gt" => Ok(Self::GT),
            "slt" => Ok(Self::SLT),
            "sgt" => Ok(Self::SGT),
            "eq" => Ok(Self::EQ),
            "iszero" => Ok(Self::ISZERO),
            "and" => Ok(Self::AND),
            "or" => Ok(Self::OR),
            "xor" => Ok(Self::XOR),
            "not" => Ok(Self::NOT),
            "byte" => Ok(Self::BYTE),
            "shl" => Ok(Self::SHL),
            "shr" => Ok(Self::SHR),
            "sar" => Ok(Self::SAR),
            // Keccak 0x20
            "sha3" => Ok(Self::SHA3),
            // Call context introspection 0x30 - 0x3f
            "address" => Ok(Self::ADDRESS),
            "balance" => Ok(Self::BALANCE),
            "origin" => Ok(Self::ORIGIN),
            "caller" => Ok(Self::CALLER),
            "callvalue" => Ok(Self::CALLVALUE),
            "calldataload" => Ok(Self::CALLDATALOAD),
            "calldatasize" => Ok(Self::CALLDATASIZE),
            "calldatacopy" => Ok(Self::CALLDATACOPY),
            "codesize" => Ok(Self::CODESIZE),
            "codecopy" => Ok(Self::CODECOPY),
            "gasprice" => Ok(Self::GASPRICE),
            "extcodesize" => Ok(Self::EXTCODESIZE),
            "extcodecopy" => Ok(Self::EXTCODECOPY),
            "returndatasize" => Ok(Opcode::RETURNDATASIZE),
            "returndatacopy" => Ok(Opcode::RETURNDATACOPY),
            "extcodehash" => Ok(Self::EXTCODEHASH),
            // Block context introspection 0x40 - 0x4a
            "blockhash" => Ok(Self::BLOCKHASH),
            "coinbase" => Ok(Self::COINBASE),
            "timestamp" => Ok(Self::TIMESTAMP),
            "number" => Ok(Self::NUMBER),
            "prevrandao" => Ok(Self::PREVRANDAO),
            "gaslimit" => Ok(Self::GASLIMIT),
            "chainid" => Ok(Self::CHAINID),
            "selfbalance" => Ok(Self::SELFBALANCE),
            "basefee" => Ok(Self::BASEFEE),
            "blobhash" => Ok(Self::BLOBHASH),
            "blobbasefee" => Ok(Self::BLOBBASEFEE),
            // Control flow, Storage & Memory manipulation, runtime inspection 0x50 - 0x5e
            "pop" => Ok(Self::POP),
            "mload" => Ok(Self::MLOAD),
            "mstore" => Ok(Self::MSTORE),
            "mstore8" => Ok(Self::MSTORE8),
            "sload" => Ok(Self::SLOAD),
            "sstore" => Ok(Self::SSTORE),
            "jump" => Ok(Self::JUMP),
            "jumpi" => Ok(Self::JUMPI),
            "pc" => Ok(Self::PC),
            "msize" => Ok(Self::MSIZE),
            "gas" => Ok(Self::GAS),
            "jumpdest" => Ok(Self::JUMPDEST),
            "tload" => Ok(Self::TLOAD),
            "tstore" => Ok(Self::TSTORE),
            "mcopy" => Ok(Self::MCOPY),
            // Static pushes 0x5f - 0x6f
            "push0" => Ok(Self::PUSH0),
            // Dups 0x80 - 0x8f
            "dup1" => Ok(Self::DUP1),
            "dup2" => Ok(Self::DUP2),
            "dup3" => Ok(Self::DUP3),
            "dup4" => Ok(Self::DUP4),
            "dup5" => Ok(Self::DUP5),
            "dup6" => Ok(Self::DUP6),
            "dup7" => Ok(Self::DUP7),
            "dup8" => Ok(Self::DUP8),
            "dup9" => Ok(Self::DUP9),
            "dup10" => Ok(Self::DUP10),
            "dup11" => Ok(Self::DUP11),
            "dup12" => Ok(Self::DUP12),
            "dup13" => Ok(Self::DUP13),
            "dup14" => Ok(Self::DUP14),
            "dup15" => Ok(Self::DUP15),
            "dup16" => Ok(Self::DUP16),
            // Swaps 0x90 - 0x9f
            "swap1" => Ok(Self::SWAP1),
            "swap2" => Ok(Self::SWAP2),
            "swap3" => Ok(Self::SWAP3),
            "swap4" => Ok(Self::SWAP4),
            "swap5" => Ok(Self::SWAP5),
            "swap6" => Ok(Self::SWAP6),
            "swap7" => Ok(Self::SWAP7),
            "swap8" => Ok(Self::SWAP8),
            "swap9" => Ok(Self::SWAP9),
            "swap10" => Ok(Self::SWAP10),
            "swap11" => Ok(Self::SWAP11),
            "swap12" => Ok(Self::SWAP12),
            "swap13" => Ok(Self::SWAP13),
            "swap14" => Ok(Self::SWAP14),
            "swap15" => Ok(Self::SWAP15),
            "swap16" => Ok(Self::SWAP16),
            // Logging 0xa0 - 0xa4
            "log0" => Ok(Self::LOG0),
            "log1" => Ok(Self::LOG1),
            "log2" => Ok(Self::LOG2),
            "log3" => Ok(Self::LOG3),
            "log4" => Ok(Self::LOG4),
            // Calls 0xf0 - 0xff
            "create" => Ok(Self::CREATE),
            "call" => Ok(Self::CALL),
            "callcode" => Ok(Self::CALLCODE),
            "return" => Ok(Self::RETURN),
            "delegatecall" => Ok(Self::DELEGATECALL),
            "create2" => Ok(Self::CREATE2),
            "staticcall" => Ok(Self::STATICCALL),
            "revert" => Ok(Self::REVERT),
            "invalid" => Ok(Self::INVALID),
            "selfdestruct" => Ok(Self::SELFDESTRUCT),
            _ => Err(()),
        }
    }
}

impl Opcode {
    pub fn byte(&self) -> u8 {
        match &self {
            Self::STOP => 0x00,
            // Basic Arithmetic 0x01 - 0x0b
            Self::ADD => 0x01,
            Self::MUL => 0x02,
            Self::SUB => 0x03,
            Self::DIV => 0x04,
            Self::SDIV => 0x05,
            Self::MOD => 0x06,
            Self::SMOD => 0x07,
            Self::ADDMOD => 0x08,
            Self::MULMOD => 0x09,
            Self::EXP => 0x0A,
            Self::SIGNEXTEND => 0x0B,
            // Logic & Bitwise Operations 0x10 - 0x1d
            Self::LT => 0x10,
            Self::GT => 0x11,
            Self::SLT => 0x12,
            Self::SGT => 0x13,
            Self::EQ => 0x14,
            Self::ISZERO => 0x15,
            Self::AND => 0x16,
            Self::OR => 0x17,
            Self::XOR => 0x18,
            Self::NOT => 0x19,
            Self::BYTE => 0x1A,
            Self::SHL => 0x1B,
            Self::SHR => 0x1C,
            Self::SAR => 0x1D,
            // Keccak 0x20
            Self::SHA3 => 0x20,
            // Call context introspection 0x30 - 0x3f
            Self::ADDRESS => 0x30,
            Self::BALANCE => 0x31,
            Self::ORIGIN => 0x32,
            Self::CALLER => 0x33,
            Self::CALLVALUE => 0x34,
            Self::CALLDATALOAD => 0x35,
            Self::CALLDATASIZE => 0x36,
            Self::CALLDATACOPY => 0x37,
            Self::CODESIZE => 0x38,
            Self::CODECOPY => 0x39,
            Self::GASPRICE => 0x3A,
            Self::EXTCODESIZE => 0x3B,
            Self::EXTCODECOPY => 0x3C,
            Self::RETURNDATASIZE => 0x3D,
            Self::RETURNDATACOPY => 0x3E,
            Self::EXTCODEHASH => 0x3F,
            // Block context introspection 0x40 - 0x4a
            Self::BLOCKHASH => 0x40,
            Self::COINBASE => 0x41,
            Self::TIMESTAMP => 0x42,
            Self::NUMBER => 0x43,
            Self::PREVRANDAO => 0x44,
            Self::GASLIMIT => 0x45,
            Self::CHAINID => 0x46,
            Self::SELFBALANCE => 0x47,
            Self::BASEFEE => 0x48,
            Self::BLOBHASH => 0x49,
            Self::BLOBBASEFEE => 0x4a,
            // Control flow, Storage & Memory manipulation, runtime inspection 0x50 - 0x5e
            Self::POP => 0x50,
            Self::MLOAD => 0x51,
            Self::MSTORE => 0x52,
            Self::MSTORE8 => 0x53,
            Self::SLOAD => 0x54,
            Self::SSTORE => 0x55,
            Self::JUMP => 0x56,
            Self::JUMPI => 0x57,
            Self::PC => 0x58,
            Self::MSIZE => 0x59,
            Self::GAS => 0x5A,
            Self::JUMPDEST => 0x5B,
            Self::TLOAD => 0x5C,
            Self::TSTORE => 0x5D,
            Self::MCOPY => 0x5E,
            // Static pushes 0x5f - 0x6f
            Self::PUSH0 => 0x5F,
            Self::PUSH1(_) => 0x60,
            Self::PUSH2(_) => 0x61,
            Self::PUSH3(_) => 0x62,
            Self::PUSH4(_) => 0x63,
            Self::PUSH5(_) => 0x64,
            Self::PUSH6(_) => 0x65,
            Self::PUSH7(_) => 0x66,
            Self::PUSH8(_) => 0x67,
            Self::PUSH9(_) => 0x68,
            Self::PUSH10(_) => 0x69,
            Self::PUSH11(_) => 0x6A,
            Self::PUSH12(_) => 0x6B,
            Self::PUSH13(_) => 0x6C,
            Self::PUSH14(_) => 0x6D,
            Self::PUSH15(_) => 0x6E,
            Self::PUSH16(_) => 0x6F,
            Self::PUSH17(_) => 0x70,
            Self::PUSH18(_) => 0x71,
            Self::PUSH19(_) => 0x72,
            Self::PUSH20(_) => 0x73,
            Self::PUSH21(_) => 0x74,
            Self::PUSH22(_) => 0x75,
            Self::PUSH23(_) => 0x76,
            Self::PUSH24(_) => 0x77,
            Self::PUSH25(_) => 0x78,
            Self::PUSH26(_) => 0x79,
            Self::PUSH27(_) => 0x7A,
            Self::PUSH28(_) => 0x7B,
            Self::PUSH29(_) => 0x7C,
            Self::PUSH30(_) => 0x7D,
            Self::PUSH31(_) => 0x7E,
            Self::PUSH32(_) => 0x7F,
            // Dups 0x80 - 0x8f
            Self::DUP1 => 0x80,
            Self::DUP2 => 0x81,
            Self::DUP3 => 0x82,
            Self::DUP4 => 0x83,
            Self::DUP5 => 0x84,
            Self::DUP6 => 0x85,
            Self::DUP7 => 0x86,
            Self::DUP8 => 0x87,
            Self::DUP9 => 0x88,
            Self::DUP10 => 0x89,
            Self::DUP11 => 0x8A,
            Self::DUP12 => 0x8B,
            Self::DUP13 => 0x8C,
            Self::DUP14 => 0x8D,
            Self::DUP15 => 0x8E,
            Self::DUP16 => 0x8F,
            // Swaps 0x90 - 0x9f
            Self::SWAP1 => 0x90,
            Self::SWAP2 => 0x91,
            Self::SWAP3 => 0x92,
            Self::SWAP4 => 0x93,
            Self::SWAP5 => 0x94,
            Self::SWAP6 => 0x95,
            Self::SWAP7 => 0x96,
            Self::SWAP8 => 0x97,
            Self::SWAP9 => 0x98,
            Self::SWAP10 => 0x99,
            Self::SWAP11 => 0x9A,
            Self::SWAP12 => 0x9B,
            Self::SWAP13 => 0x9C,
            Self::SWAP14 => 0x9D,
            Self::SWAP15 => 0x9E,
            Self::SWAP16 => 0x9F,
            // Logging 0xa0 - 0xa4
            Self::LOG0 => 0xA0,
            Self::LOG1 => 0xA1,
            Self::LOG2 => 0xA2,
            Self::LOG3 => 0xA3,
            Self::LOG4 => 0xA4,
            Self::CREATE => 0xF0,
            Self::CALL => 0xF1,
            Self::CALLCODE => 0xF2,
            Self::RETURN => 0xF3,
            Self::DELEGATECALL => 0xF4,
            Self::CREATE2 => 0xF5,
            Self::STATICCALL => 0xFA,
            Self::REVERT => 0xFD,
            Self::INVALID => 0xFE,
            Self::SELFDESTRUCT => 0xFF,

            Self::UNKNOWN(b) => *b,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::PUSH1(_) => 2,
            Self::PUSH2(_) => 3,
            Self::PUSH3(_) => 4,
            Self::PUSH4(_) => 5,
            Self::PUSH5(_) => 6,
            Self::PUSH6(_) => 7,
            Self::PUSH7(_) => 8,
            Self::PUSH8(_) => 9,
            Self::PUSH9(_) => 10,
            Self::PUSH10(_) => 11,
            Self::PUSH11(_) => 12,
            Self::PUSH12(_) => 13,
            Self::PUSH13(_) => 14,
            Self::PUSH14(_) => 15,
            Self::PUSH15(_) => 16,
            Self::PUSH16(_) => 17,
            Self::PUSH17(_) => 18,
            Self::PUSH18(_) => 19,
            Self::PUSH19(_) => 20,
            Self::PUSH20(_) => 21,
            Self::PUSH21(_) => 22,
            Self::PUSH22(_) => 23,
            Self::PUSH23(_) => 24,
            Self::PUSH24(_) => 25,
            Self::PUSH25(_) => 26,
            Self::PUSH26(_) => 27,
            Self::PUSH27(_) => 28,
            Self::PUSH28(_) => 29,
            Self::PUSH29(_) => 30,
            Self::PUSH30(_) => 31,
            Self::PUSH31(_) => 32,
            Self::PUSH32(_) => 33,
            _ => 1,
        }
    }

    pub fn is_empty(&self) -> bool {
        false
    }

    pub fn append_to(&self, stream: &mut Vec<u8>) {
        stream.push(self.byte());

        push_extra_data!(
            self, stream, PUSH1, PUSH2, PUSH3, PUSH4, PUSH5, PUSH6, PUSH7, PUSH8, PUSH9, PUSH10,
            PUSH11, PUSH12, PUSH13, PUSH14, PUSH15, PUSH16, PUSH17, PUSH18, PUSH19, PUSH20, PUSH21,
            PUSH22, PUSH23, PUSH24, PUSH25, PUSH26, PUSH27, PUSH28, PUSH29, PUSH30, PUSH31, PUSH32
        );
    }
}

impl fmt::Display for Opcode {
    push_match_formats!(
        PUSH1, PUSH2, PUSH3, PUSH4, PUSH5, PUSH6, PUSH7, PUSH8, PUSH9, PUSH10, PUSH11, PUSH12,
        PUSH13, PUSH14, PUSH15, PUSH16, PUSH17, PUSH18, PUSH19, PUSH20, PUSH21, PUSH22, PUSH23,
        PUSH24, PUSH25, PUSH26, PUSH27, PUSH28, PUSH29, PUSH30, PUSH31, PUSH32
    );
}
