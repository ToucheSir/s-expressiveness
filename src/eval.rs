use cell::*;
use env::{Env, DefaultNS, SymbolIndex};

#[derive(Debug)]
pub enum EvalError {
    IllegalOperator,
    NonUnary,
    NotCons(CellIndex),
    NonBinary,
    NonNumeric,
    UnknownOperator(SymbolIndex),
}

fn is_atom(exp: CellIndex, cells: &CellStorage) -> bool {
    match cells.val_of(exp) {
        CellType::Number(_) | CellType::Symbol(_) => true,
        _ => exp == NIL_INDEX,
    }
}

fn split_binary(exp: CellIndex, cells: &CellStorage) -> (CellIndex, CellIndex) {
    let head = car!(cdr!(exp, cells), cells);
    let tail = car!(cdr!(cdr!(exp, cells), cells), cells);
    (head, tail)
}

fn eval_cons(exp: CellIndex,
             cells: &mut CellStorage,
             env: &mut Env,
             ns: &DefaultNS)
             -> Result<CellIndex, EvalError> {
    if !is_binary(exp, cells) {
        Err(EvalError::NonBinary)
    } else {
        let (head, tail) = split_binary(exp, cells);
        let head = try!(eval(head, cells, env, ns));
        let tail = try!(eval(tail, cells, env, ns));
        let cons_cell = cells.alloc_cell(CellType::Cons(head));
        cells.set_tail(cons_cell, tail);
        Ok(cons_cell)
    }
}

fn eval_arithmetic(op: SymbolIndex,
                   exp: CellIndex,
                   cells: &mut CellStorage,
                   env: &mut Env,
                   ns: &DefaultNS)
                   -> Result<CellIndex, EvalError> {
    if !is_binary(exp, cells) {
        Err(EvalError::NonBinary)
    } else {
        let (head, tail) = split_binary(exp, cells);
        let lhs = try!(eval(head, cells, env, ns));
        let rhs = try!(eval(tail, cells, env, ns));
        match (cells.val_of(lhs), cells.val_of(rhs)) {
            (CellType::Number(a), CellType::Number(b)) => {
                Ok(cells.alloc_cell(CellType::Number(if op == ns.add {
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
            _ => Err(EvalError::NonNumeric),
        }
    }
}

pub fn eval(exp: CellIndex,
            cells: &mut CellStorage,
            env: &mut Env,
            ns: &DefaultNS)
            -> Result<CellIndex, EvalError> {
    let cell = cells.get(exp);
    if is_atom(exp, cells) {
        Ok(exp)
    } else if let CellType::Cons(head) = cell.val {
        if let CellType::Symbol(op) = cells.val_of(head) {
            if op == ns.quote {
                if !is_unary(exp, cells) {
                    Err(EvalError::NonUnary)
                } else {
                    Ok(car!(cdr!(exp, cells), cells))
                }
            } else if op == ns.hd || op == ns.tl {
                if !is_unary(exp, cells) {
                    Err(EvalError::NonUnary)
                } else {
                    let res = try!(eval(car!(cdr!(exp, cells), cells), cells, env, ns));
                    if !is_cons(res, cells) {
                        Err(EvalError::NotCons(exp))
                    } else if op == ns.hd {
                        Ok(car!(res, cells))
                    } else {
                        Ok(cdr!(res, cells))
                    }
                }
            } else if op == ns.cons {
                eval_cons(exp, cells, env, ns)
            } else if op == ns.add || op == ns.sub || op == ns.mul || op == ns.div || op == ns.modu {
                eval_arithmetic(op, exp, cells, env, ns)
            } else {
                Err(EvalError::UnknownOperator(op))
            }
        } else {
            Err(EvalError::IllegalOperator)
        }
    } else {
        panic!("Invalid expression")
    }
}
