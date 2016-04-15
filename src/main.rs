use std::io;
use std::io::prelude::*;

#[macro_use]
mod cell;
mod env;
mod parser;
mod eval;

use cell::*;
use parser::*;
use env::*;
use eval::{EvalError, eval};

fn s_exp(input: &mut io::StdinLock,
         output: &mut io::Stdout,
         mut buf: &mut Vec<u8>,
         storage: &mut CellStorage,
         env: &mut Env)
         -> Option<CellIndex> {
    let mut parser = Parser::new(env);
    loop {
        print!("[{}] ", parser.nesting);
        output.flush().unwrap();
        if let Ok(n) = input.read_until(b'\n', &mut buf) {
            // Check for EOF
            if n == 0 {
                return None;
            }
        }
        // println!("{}", String::from_utf8_lossy(buf));
        match parser.parse(buf, storage) {
            Ok(idx) => return Some(idx),
            Err(ParseError::SyntaxError(ch)) => {
                println!("Syntax error at '{}'", ch);
                buf.clear();
            }
            _ => {}
        }
    }
}

fn init_storage(buf: &mut [Cell]) -> CellStorage {
    let count = buf.len() - 1;
    let mut storage = CellStorage::new(buf);
    for idx in 1..count {
        storage.set_tail(idx, idx + 1);
    }
    storage
}

fn display_err(err_type: EvalError, cells: &CellStorage, env: &Env) {
    print!("\nError: ");
    match err_type {
        EvalError::IllegalOperator => println!("illegal operator!"),
        EvalError::NonUnary => println!("non unary expression!"),
        EvalError::NotCons(exp) => {
            print_exp(exp, cells, env);
            println!(" does not evaluate to a cons pair!");
        }
        EvalError::NonBinary => println!("non binary expression!"),
        EvalError::NonNumeric => println!("non unary expression!"),
        EvalError::UnknownOperator(op) => println!("unknown operator '{}'", env.get_sym(op)),
    }
}

fn main() {
    let mut cells = [Cell::empty(); 64];
    let mut storage = init_storage(&mut cells);
    let mut env = Env::new();
    let ns = DefaultNS::new(&mut env);

    println!("An S-expression Evaluator.");
    let stdin = io::stdin();
    let mut buf = Vec::with_capacity(64);
    let mut input = stdin.lock();
    let mut output = io::stdout();

    loop {
        if let Some(idx) = s_exp(&mut input, &mut output, &mut buf, &mut storage, &mut env) {
            print_exp(idx, &mut storage, &env);
            match eval(idx, &mut storage, &mut env, &ns) {
                Ok(exp) => {
                    print!(" ==> ");
                    print_exp(exp, &storage, &env);
                    storage.free_cell(exp);
                    println!("");
                }
                Err(err_type) => {
                    display_err(err_type, &storage, &env);
                }
            }

            storage.free_cell(idx);
            // println!("{:?}", &storage);
            buf.clear();
        } else {
            break;
        }
    }

    println!("\nEnd.");
}
