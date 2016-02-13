use std::io;
use std::io::prelude::*;

#[derive(Debug)]
enum Cell {
    Number(i32),
    Symbol(u32),
    Cons(Box<Cell>, Box<Cell>),
    Nil
}

struct Env {
    symbols: Vec<String>
}
impl Env {
    fn new() -> Env {
        Env {
            symbols: Vec::new()
        }
    }

    fn add_sym(&mut self, name: String) -> u32 {
        self.symbols.push(name);
        return (self.symbols.len() - 1) as u32;
    }
}

struct Parser {
    input: String,
    pos: usize,
    env: Env
}

#[derive(Debug, PartialEq)]
enum Token {
    LeftParen,
    RightParen,
    Dot,
    Quote,
    Number(String),
    Symbol(String)
}

impl Parser {
    fn eol(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn parse_sexp(&mut self) -> Cell {
        match self.next_token() {
            Token::Number(str_num) => Cell::Number(str_num.parse::<i32>().unwrap()),
            Token::Symbol(name) => Cell::Symbol(self.env.add_sym(name)),
            tok @ _ => {
                assert_eq!(tok, Token::LeftParen);
                let exps = self.parse_sexps();
                assert_eq!(self.next_token(), Token::RightParen);
                exps
            }
        }
    }

    fn parse_sexps(&mut self) -> Cell {
        Cell::Nil
    }

    fn peek_ch(&self) -> char {
        self.input[self.pos..].chars().next().unwrap()
    }

    fn next_ch(&mut self) -> char {
        let mut iter = self.input[self.pos..].char_indices();
        let (_, ch) = iter.next().unwrap();
        let (next_pos, _) = iter.next().unwrap_or((1, ' '));
        self.pos += next_pos;
        return ch;
    }

    fn next_token(&mut self) -> Token {
        self.consume_whitespace();
        let ch = self.peek_ch();
        match ch {
            '(' => {
                self.next_ch();
                Token::LeftParen
            },
            ')' => {
                self.next_ch();
                Token::RightParen
            },
            '.' => {
                self.next_ch();
                Token::Dot
            },
            '\'' => {
                self.next_ch();
                Token::Quote
            },
            _ => if ch.is_digit(10) {
                Token::Number(self.consume_while(|c| c.is_digit(10)))
            } else if ch.is_alphanumeric() {
                Token::Symbol(self.consume_while(char::is_alphanumeric))
            } else {
                panic!("Syntax error: at '{}'!", ch);
            }
        }
    }

    fn consume_while<F>(&mut self, test: F) -> String where F: Fn(char) -> bool {
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

fn s_exp(text: String) -> Cell {
    let mut parser = Parser {
        input: text,
        pos: 0,
        env: Env::new()
    };
    return parser.parse_sexp();
}

fn main() {
    println!("An S-expression Evaluator.");
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let exp = s_exp(line.unwrap());
        println!("{:?}", exp);
        // println!("{}", line.unwrap());
    }
    println!("End.");
}
