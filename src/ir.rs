#[derive(Debug, Clone)]
pub struct Module {

}


#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    body: Vec<Instr>
}

#[derive(Debug, Clone)]
pub enum Instr {
    // Binary Operations
    Assign {
        index: TempIndex,
        v: Value
    },
    Add {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Sub {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Mul {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Div {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Mod {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Return {
        v: Option<Value>
    },
    Label(LabelIndex),
    Jump(LabelIndex),
    Jnz {
        cond: Value,
        to: LabelIndex,
        otherwise: LabelIndex
    },
    /// If `into` is Some(_) then it returns something and
    /// the result is used later
    Call {
        name: String,
        args: Vec<Value>,
        into: Option<TempIndex>
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Const {
        t: Type,
        v: u64
    },
    Temp {
        t: Type,
        i: TempIndex
    },
}

#[derive(Debug, Clone)]
pub enum Type {
    U64,
    Char,
    Bool
}

pub type TempIndex = usize;
pub type LabelIndex = usize;
