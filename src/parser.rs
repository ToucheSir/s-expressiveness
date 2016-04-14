use env::Env;
use cell::*;

#[derive(Debug, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,
    Dot,
    Number(String),
    Symbol(String),
}
struct TokenStream<'a> {
    input: &'a Vec<u8>,
    pos: usize,
}

impl<'a> TokenStream<'a> {
    fn new(input: &'a Vec<u8>) -> Self {
        TokenStream {
            input: input,
            pos: 0,
        }
    }

    fn eol(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn peek_ch(&self) -> char {
        self.input[self.pos] as char
    }

    fn next_ch(&mut self) -> char {
        let ch = self.input[self.pos];
        self.pos += 1;
        return ch as char;
    }

    fn peek_token(&mut self) -> Option<Token> {
        let old_pos = self.pos;
        let tok = self.next_token();
        self.pos = old_pos;
        tok
    }

    fn next_token(&mut self) -> Option<Token> {
        self.consume_whitespace();
        if self.eol() {
            None
        } else {
            let ch = self.peek_ch();
            Some(match ch {
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
            })
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


pub struct Parser<'a> {
    pub nesting: usize,
    env: &'a mut Env,
    exp: CellIndex,
    exps: CellIndex,
}
impl<'a> Parser<'a> {
    pub fn new(env: &'a mut Env) -> Self {
        Parser {
            env: env,
            nesting: 0,
            exp: NIL_INDEX,
            exps: NIL_INDEX,
        }
    }

    pub fn parse(&mut self, input: &Vec<u8>, storage: &mut CellStorage) -> Option<CellIndex> {
        let mut tokens = TokenStream::new(input);
        self.parse_sexp(&mut tokens, storage)
    }

    fn parse_sexp(&mut self,
                  tokens: &mut TokenStream,
                  storage: &mut CellStorage)
                  -> Option<CellIndex> {
        match tokens.next_token() {
            Some(Token::Number(str_num)) => {
                let nval = str_num.parse::<i32>().unwrap();
                self.exp = storage.alloc_cell(CellType::Number(nval));
                Some(self.exp)
            }
            Some(Token::Symbol(name)) => {
                self.exp = storage.alloc_cell(CellType::Symbol(self.env.add_sym(name)));
                Some(self.exp)
            }
            Some(tok) => {
                assert_eq!(tok, Token::LeftParen);
                self.nesting += 1;
                match self.parse_sexps(tokens, storage) {
                    Some(exps) => {
                        self.nesting -= 1;
                        match tokens.next_token() {
                            Some(Token::RightParen) => {
                                self.exp = exps;
                                Some(self.exp)
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn parse_sexps(&mut self,
                   tokens: &mut TokenStream,
                   storage: &mut CellStorage)
                   -> Option<CellIndex> {
        match tokens.peek_token() {
            Some(Token::RightParen) => Some(NIL_INDEX),
            _ => {
                match self.parse_sexp(tokens, storage) {
                    Some(car) => {
                        match if let Some(Token::Dot) = tokens.peek_token() {
                            tokens.next_token();
                            self.parse_sexp(tokens, storage)
                        } else {
                            self.parse_sexps(tokens, storage)
                        } {
                            Some(cdr) => {
                                let idx = storage.alloc_cell(CellType::Cons(car));
                                storage.set_tail(idx, cdr);
                                self.exps = idx;
                                Some(idx)
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
        }
    }
}
