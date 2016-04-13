use std::io;
use std::io::prelude::*;

#[derive(Copy, Clone, Debug)]
enum CellType {
    Number(i32),
    Symbol(usize),
    Cons(usize),
    Free,
}

#[derive(Copy, Clone, Debug)]
struct Cell {
    val: CellType,
    tail: usize,
}
impl Cell {
    fn empty() -> Cell {
        Cell {
            val: CellType::Free,
            tail: NIL_INDEX,
        }
    }
    fn new(val: CellType, tail: usize) -> Self {
        Cell {
            val: val,
            tail: tail,
        }
    }
}

struct Env {
    symbols: Vec<String>,
}
impl Env {
    fn new() -> Env {
        Env { symbols: Vec::new() }
    }

    fn add_sym(&mut self, name: String) -> usize {
        match self.symbols.iter().position(|s| &name == s) {
            Some(idx) => idx,
            None => {
                self.symbols.push(name);
                self.symbols.len() - 1
            }
        }
    }
}

struct DefaultNS {
    add: usize,
    sub: usize,
    mul: usize,
    div: usize,
    modu: usize,
    cons: usize,
    hd: usize,
    tl: usize,
    quote: usize,
}
impl DefaultNS {
    fn new(env: &mut Env) -> Self {
        DefaultNS {
            add: env.add_sym("add".to_string()),
            sub: env.add_sym("sub".to_string()),
            mul: env.add_sym("mul".to_string()),
            div: env.add_sym("div".to_string()),
            modu: env.add_sym("mod".to_string()),
            cons: env.add_sym("cons".to_string()),
            hd: env.add_sym("hd".to_string()),
            tl: env.add_sym("tl".to_string()),
            quote: env.add_sym("'".to_string()),
        }
    }
}

const NIL_INDEX: usize = 0;

#[derive(Debug, PartialEq)]
enum Token {
    LeftParen,
    RightParen,
    Dot,
    Quote,
    Number(String),
    Symbol(String),
}

struct Parser<'a> {
    input: &'a Vec<u8>,
    pos: usize,
    env: &'a mut Env,
}
impl<'a> Parser<'a> {
    fn eol(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn parse_sexp(&mut self, storage: &mut CellStorage) -> usize {
        match self.next_token() {
            Token::Number(str_num) => {
                let nval = str_num.parse::<i32>().unwrap();
                storage.alloc_cell(CellType::Number(nval))
            }
            Token::Symbol(name) => storage.alloc_cell(CellType::Symbol(self.env.add_sym(name))),
            tok @ _ => {
                assert_eq!(tok, Token::LeftParen);
                let exps = self.parse_sexps(storage);
                assert_eq!(self.next_token(), Token::RightParen);
                exps
            }
        }
    }

    fn parse_sexps(&mut self, storage: &mut CellStorage) -> usize {
        match self.peek_ch() {
            ')' => NIL_INDEX,
            _ => {
                let car = self.parse_sexp(storage);
                self.consume_whitespace();
                let cdr = if self.peek_ch() == '.' {
                    self.next_token();
                    self.parse_sexp(storage)
                } else {
                    self.parse_sexps(storage)
                };
                let idx = storage.alloc_cell(CellType::Cons(car));
                storage.cells[idx].tail = cdr;
                idx
            }
        }
    }

    fn peek_ch(&self) -> char {
        self.input[self.pos] as char
    }

    fn next_ch(&mut self) -> char {
        let ch = self.input[self.pos];
        self.pos += 1;
        return ch as char;
    }

    fn next_token(&mut self) -> Token {
        self.consume_whitespace();
        let ch = self.peek_ch();
        match ch {
            '(' => {
                self.next_ch();
                Token::LeftParen
            }
            ')' => {
                self.next_ch();
                Token::RightParen
            }
            '.' => {
                self.next_ch();
                Token::Dot
            }
            '\'' => {
                self.next_ch();
                // Token::Quote
                Token::Symbol(String::from("'"))
            }
            _ => {
                if ch.is_digit(10) {
                    Token::Number(self.consume_while(|c| c.is_digit(10)))
                } else if ch.is_alphanumeric() {
                    Token::Symbol(self.consume_while(char::is_alphanumeric))
                } else {
                    panic!("Syntax error: at '{}'!", ch);
                }
            }
        }
    }

    fn consume_while<F>(&mut self, test: F) -> String
        where F: Fn(char) -> bool
    {
        let mut res = String::new();

        while !self.eol() && test(self.peek_ch()) {
            res.push(self.next_ch());
        }

        return res;
    }

    fn consume_whitespace(&mut self) {
        self.consume_while(char::is_whitespace);
    }
}

#[derive(Debug)]
struct CellStorage<'a> {
    free_index: usize,
    cells: &'a mut [Cell],
}
impl<'a> CellStorage<'a> {
    fn new(buf: &mut [Cell]) -> CellStorage {
        CellStorage {
            cells: buf,
            free_index: NIL_INDEX + 1,
        }
    }
    fn alloc_cell(&mut self, val: CellType) -> usize {
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
    fn free_cell(&mut self, idx: usize) {
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

    fn get(&self, idx: usize) -> Cell {
        self.cells[idx]
    }
    fn val_of(&self, idx: usize) -> CellType {
        self.cells[idx].val
    }
    fn tail_of(&self, idx: usize) -> usize {
        self.cells[idx].tail
    }
}

fn print_exp(idx: usize, storage: &CellStorage, env: &Env) {
    if idx == NIL_INDEX {
        print!("()");
    } else {
        match storage.cells[idx].val {
            CellType::Symbol(sym) => {
                print!("{}", env.symbols[sym]);
            }
            CellType::Number(n) => {
                print!("{}", n);
            }
            CellType::Cons(_) => print_list(idx, storage, env),
            _ => {}
        }
    }
}

fn print_list(idx: usize, storage: &CellStorage, env: &Env) {
    print!("(");
    let mut exp = idx;
    let mut cell = storage.cells[exp];

    if let CellType::Cons(head) = cell.val {
        print_exp(head, storage, env);
        exp = cell.tail;
        cell = storage.cells[exp];
    }

    while let CellType::Cons(head) = cell.val {
        print!(" ");
        print_exp(head, storage, env);
        exp = cell.tail;
        cell = storage.cells[exp];
    }

    if exp != NIL_INDEX {
        print!(" . ");
        print_exp(exp, storage, env);
    }
    print!(")");
}

fn s_exp(buf: &Vec<u8>, storage: &mut CellStorage, env: &mut Env) -> usize {
    let mut parser = Parser {
        input: buf,
        pos: 0,
        env: env,
    };
    parser.parse_sexp(storage)
}

macro_rules! car {
    ($exp: expr, $cells: expr) => {
        if let CellType::Cons(head) = $cells[$exp].val {
            head
        } else {
            panic!("Not a cons cell")
        }
    }
}

macro_rules! cdr {
    ($exp: expr, $cells: expr) => {
        if let CellType::Cons(_) = $cells[$exp].val {
            $cells[$exp].tail
        } else {
            NIL_INDEX
        }
    }
}

fn is_atom(exp: usize, cells: &[Cell]) -> bool {
    match cells[exp].val {
        CellType::Number(_) | CellType::Symbol(_) => true,
        _ => false,
    }
}

fn is_cons(exp: usize, cells: &[Cell]) -> bool {
    match cells[exp].val {
        CellType::Cons(_) => true,
        _ => false,
    }
}

fn is_unary(exp: usize, cells: &[Cell]) -> bool {
    is_cons(cdr!(exp, cells), cells) && cdr!(cdr!(exp, cells), cells) == NIL_INDEX
}

fn is_binary(exp: usize, cells: &[Cell]) -> bool {
    is_cons(cdr!(exp, cells), cells) //&& is_cons(cdr!(cdr!(exp, cells), cells), cells)
        // && (cdr!(cdr!(cdr!(exp, cells), cells), cells) == NIL_INDEX)
}

enum SyntaxError {
    IllegalOperator,
    NonUnary,
    NotCons,
    NonBinary,
    NonNumeric,
    UnknownOperator,
}

fn eval(exp: usize,
        storage: &mut CellStorage,
        env: &mut Env,
        ns: &DefaultNS)
        -> Result<usize, u32> {
    let cell = storage.cells[exp];
    match cell.val {
        CellType::Number(_) | CellType::Symbol(_) => Ok(exp),
        CellType::Cons(head) => {
            if let CellType::Symbol(op) = storage.cells[head].val {
                if op == ns.quote {
                    if !is_unary(exp, storage.cells) {
                        Err(2)
                    } else {
                        Ok(car!(cdr!(exp, storage.cells), storage.cells))
                    }
                } else if op == ns.hd || op == ns.tl {
                    if !is_unary(exp, storage.cells) {
                        Err(2)
                    } else {
                        let res = try!(eval(car!(cdr!(exp, storage.cells), storage.cells),
                                            storage,
                                            env,
                                            ns));
                        if op == ns.hd {
                            Ok(car!(res, storage.cells))
                        } else {
                            Ok(cdr!(res, storage.cells))
                        }
                    }
                } else if op == ns.cons {
                    if !is_binary(exp, storage.cells) {
                        Err(6)
                    } else {
                        let head = try!(eval(car!(cdr!(exp, storage.cells), storage.cells),
                                           storage, env, ns));
                        let tail = try!(eval(car!(cdr!(cdr!(exp, storage.cells), storage.cells),
                                                  storage.cells),
                                             storage,
                                             env,
                                             ns));
                        let cons_cell = storage.alloc_cell(CellType::Cons(head));
                        storage.cells[cons_cell].tail = tail;
                        Ok(cons_cell)
                    }
                } else if op == ns.add || op == ns.sub || op == ns.mul || op == ns.div || op == ns.modu {
                    if !is_binary(exp, storage.cells) {
                        Err(3)
                    } else {
                        let lhs = try!(eval(car!(cdr!(exp, storage.cells), storage.cells),
                                            storage,
                                            env,
                                            ns));
                        let rhs = try!(eval(car!(cdr!(cdr!(exp, storage.cells), storage.cells),
                                                 storage.cells),
                                            storage,
                                            env,
                                            ns));

                        match (storage.val_of(lhs), storage.val_of(rhs)) {
                            (CellType::Number(a), CellType::Number(b)) => {
                                Ok(storage.alloc_cell(CellType::Number(if op == ns.add {
                                    a + b
                                } else if op == ns.sub {
                                    a - b
                                } else if op == ns.mul {
                                    a * b
                                } else if op == ns.div {
                                    a / b
                                } else {
                                    // if op == ns.modu
                                    a % b
                                })))
                            }
                            _ => Err(4),
                        }
                    }
                } else {
                    Err(5)
                }
            } else {
                Err(1)
            }
        }
        _ => if exp == NIL_INDEX {
            Ok(NIL_INDEX)
        } else {
            panic!("Invalid expression")
        }
    }
}


fn init_storage(buf: &mut [Cell]) -> CellStorage {
    for idx in 1..buf.len() - 1 {
        buf[idx].tail = idx + 1;
    }
    CellStorage::new(buf)
}

fn main() {
    let mut cells = [Cell::empty(); 64];
    let mut storage = init_storage(&mut cells);
    let mut env = Env::new();
    let ns = DefaultNS::new(&mut env);

    println!("An S-expression Evaluator.");
    let stdin = io::stdin();
    let mut buf = Vec::new();
    let mut input = stdin.lock();
    let mut output = io::stdout();

    loop {
        print!("[0] ");
        output.flush().unwrap();
        input.read_until(b'\n', &mut buf).unwrap();
        let idx = s_exp(&buf, &mut storage, &mut env);

        print_exp(idx, &mut storage, &env);
        match eval(idx, &mut storage, &mut env, &ns) {
            Ok(exp) => {
                print!(" ==> ");
                print_exp(exp, &storage, &env);
                storage.free_cell(exp);
                println!("");
            }
            _ => {}
        }

        storage.free_cell(idx);
        // println!("{:?}", &storage);
        buf.clear();
    }

    println!("End.");
}
