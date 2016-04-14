use env::{Env, SymbolIndex};

pub type CellIndex = usize;

pub const NIL_INDEX: CellIndex = 0;

#[derive(Copy, Clone, Debug)]
pub enum CellType {
    Number(i32),
    Symbol(SymbolIndex),
    Cons(CellIndex),
    Free,
}

#[derive(Copy, Clone, Debug)]
pub struct Cell {
    pub val: CellType,
    tail: CellIndex,
}
impl Cell {
    pub fn empty() -> Cell {
        Cell {
            val: CellType::Free,
            tail: NIL_INDEX,
        }
    }
    fn new(val: CellType, tail: CellIndex) -> Self {
        Cell {
            val: val,
            tail: tail,
        }
    }
}

#[derive(Debug)]
pub struct CellStorage<'a> {
    free_index: CellIndex,
    cells: &'a mut [Cell],
}
impl<'a> CellStorage<'a> {
    pub fn new(buf: &mut [Cell]) -> CellStorage {
        CellStorage {
            cells: buf,
            free_index: NIL_INDEX + 1,
        }
    }
    pub fn alloc_cell(&mut self, val: CellType) -> CellIndex {
        if self.free_index == NIL_INDEX {
            panic!("Exhausted cell storage!");
        } else {
            let idx = self.free_index;
            self.free_index = self.cells[idx].tail;
            self.cells[idx].val = val;
            self.cells[idx].tail = NIL_INDEX;
            idx
        }
    }
    pub fn free_cell(&mut self, idx: CellIndex) {
        match self.cells[idx].val {
            CellType::Number(_) | CellType::Symbol(_) => {
                self.cells[idx] = Cell::new(CellType::Free, self.free_index);
                self.free_index = idx;
            }
            CellType::Cons(head) => {
                self.free_cell(head);
                let tail = self.cells[idx].tail;
                self.free_cell(tail);
                self.cells[idx] = Cell::new(CellType::Free, self.free_index);
                self.free_index = idx;
            }
            _ => {}
        }
    }

    pub fn get(&self, idx: CellIndex) -> Cell {
        self.cells[idx]
    }
    pub fn set_tail(&mut self, idx: CellIndex, tail: CellIndex) {
        self.cells[idx].tail = tail;
    }
    pub fn val_of(&self, idx: CellIndex) -> CellType {
        self.cells[idx].val
    }
    pub fn tail_of(&self, idx: CellIndex) -> CellIndex {
        self.cells[idx].tail
    }
}

macro_rules! car {
    ($exp: expr, $storage: expr) => {
        if let CellType::Cons(head) = $storage.get($exp).val {
            head
        } else {
            panic!("Not a cons cell")
        }
    }
}

macro_rules! cdr {
    ($exp: expr, $storage: expr) => {
        if let CellType::Cons(_) = $storage.val_of($exp) {
            $storage.tail_of($exp)
        } else {
            NIL_INDEX
        }
    }
}

pub fn is_cons(exp: CellIndex, cells: &CellStorage) -> bool {
    match cells.val_of(exp) {
        CellType::Cons(_) => true,
        _ => false,
    }
}

pub fn is_unary(exp: CellIndex, cells: &CellStorage) -> bool {
    is_cons(cdr!(exp, cells), cells) && cdr!(cdr!(exp, cells), cells) == NIL_INDEX
}

pub fn is_binary(exp: CellIndex, cells: &CellStorage) -> bool {
    is_cons(cdr!(exp, cells), cells) && is_cons(cdr!(cdr!(exp, cells), cells), cells) &&
    (cdr!(cdr!(cdr!(exp, cells), cells), cells) == NIL_INDEX)
}

pub fn print_exp(idx: CellIndex, storage: &CellStorage, env: &Env) {
    if idx == NIL_INDEX {
        print!("()");
    } else {
        match storage.get(idx).val {
            CellType::Symbol(sym) => {
                print!("{}", env.get_sym(sym));
            }
            CellType::Number(n) => {
                print!("{}", n);
            }
            CellType::Cons(_) => print_list(idx, storage, env),
            _ => {}
        }
    }
}

pub fn print_list(idx: CellIndex, storage: &CellStorage, env: &Env) {
    print!("(");
    let mut exp = idx;

    print_exp(car!(exp, storage), storage, env);
    exp = cdr!(exp, storage);

    while let CellType::Cons(head) = storage.val_of(exp) {
        print!(" ");
        print_exp(head, storage, env);
        exp = cdr!(exp, storage);
    }

    if exp != NIL_INDEX {
        print!(" . ");
        print_exp(exp, storage, env);
    }
    print!(")");
}
