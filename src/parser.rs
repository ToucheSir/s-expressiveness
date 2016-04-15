use env::Env;
use cell::*;

#[derive(Debug, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,
    Dot,
    Number(String),
    Symbol(String),
    EOL,
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

    fn peek_token(&mut self) -> Result<Token, ParseError> {
        let old_pos = self.pos;
        let tok = self.next_token();
        self.pos = old_pos;
        tok
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        self.consume_whitespace();
        if self.eol() {
            Ok(Token::EOL)
        } else {
            let ch = self.peek_ch();
            match ch {
                '(' => {
                    self.next_ch();
                    Ok(Token::LeftParen)
                }
                ')' => {
                    self.next_ch();
                    Ok(Token::RightParen)
                }
                '.' => {
                    self.next_ch();
                    Ok(Token::Dot)
                }
                '\'' => {
                    self.next_ch();
                    // Token::Quote
                    Ok(Token::Symbol(String::from("'")))
                }
                _ => {
                    if ch.is_digit(10) {
                        Ok(Token::Number(self.consume_while(|c| c.is_digit(10))))
                    } else if ch.is_alphanumeric() {
                        Ok(Token::Symbol(self.consume_while(char::is_alphanumeric)))
                    } else {
                        Err(ParseError::SyntaxError(ch))
                    }
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

pub enum ParseError {
    SyntaxError(char),
    UnbalancedBraces(u32),
}

pub struct Parser<'a> {
    pub nesting: u32,
    env: &'a mut Env,
}
impl<'a> Parser<'a> {
    pub fn new(env: &'a mut Env) -> Self {
        Parser {
            env: env,
            nesting: 0,
        }
    }

    pub fn parse(&mut self,
                 input: &Vec<u8>,
                 storage: &mut CellStorage)
                 -> Result<CellIndex, ParseError> {
        self.nesting = 0;
        let mut tokens = TokenStream::new(input);
        self.parse_sexp(&mut tokens, storage)
    }

    fn next_tok(&mut self, tokens: &mut TokenStream) -> Result<Token, ParseError> {
        match tokens.next_token() {
            err @ Err(ParseError::SyntaxError(_)) => {
                self.nesting = 0;
                err
            }
            res @ _ => res,
        }
    }

    fn peek_tok(&mut self, tokens: &mut TokenStream) -> Result<Token, ParseError> {
        match tokens.peek_token() {
            err @ Err(ParseError::SyntaxError(_)) => {
                self.nesting = 0;
                err
            }
            res @ _ => res,
        }
    }

    fn parse_sexp(&mut self,
                  tokens: &mut TokenStream,
                  storage: &mut CellStorage)
                  -> Result<CellIndex, ParseError> {
        match try!(self.next_tok(tokens)) {
            Token::Number(str_num) => {
                let nval = str_num.parse::<i32>().unwrap();
                Ok(storage.alloc_cell(CellType::Number(nval)))
            }
            Token::Symbol(name) => Ok(storage.alloc_cell(CellType::Symbol(self.env.add_sym(name)))),
            Token::LeftParen => {
                self.nesting += 1;
                self.parse_sexps(tokens, storage).and_then(|exps| {
                    self.nesting -= 1;
                    self.next_tok(tokens).map(|val| {
                        assert_eq!(val, Token::RightParen);
                        exps
                    })
                })
            }
            _ => Err(ParseError::UnbalancedBraces(self.nesting)),
        }
    }

    fn parse_sexps(&mut self,
                   tokens: &mut TokenStream,
                   storage: &mut CellStorage)
                   -> Result<CellIndex, ParseError> {
        if let Token::RightParen = try!(self.peek_tok(tokens)) {
            Ok(NIL_INDEX)
        } else {
            self.parse_sexp(tokens, storage).and_then(|car| {
                self.peek_tok(tokens)
                    .and_then(|tok| {
                        if let Token::Dot = tok {
                            let _ = try!(self.next_tok(tokens));
                            self.parse_sexp(tokens, storage)
                        } else {
                            self.parse_sexps(tokens, storage)
                        }
                    })
                    .map(|cdr| {
                        let idx = storage.alloc_cell(CellType::Cons(car));
                        storage.set_tail(idx, cdr);
                        idx
                    })
            })
        }
    }
}
