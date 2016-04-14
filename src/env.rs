pub type SymbolIndex = usize;

pub struct Env {
    symbols: Vec<String>,
}
impl Env {
    pub fn new() -> Env {
        Env { symbols: Vec::new() }
    }

    pub fn add_sym(&mut self, name: String) -> SymbolIndex {
        match self.symbols.iter().position(|s| &name == s) {
            Some(idx) => idx,
            None => {
                self.symbols.push(name);
                self.symbols.len() - 1
            }
        }
    }

    pub fn get_sym(&self, sym: SymbolIndex) -> &String {
        &self.symbols[sym]
    }
}

pub struct DefaultNS {
    pub add: SymbolIndex,
    pub sub: SymbolIndex,
    pub mul: SymbolIndex,
    pub div: SymbolIndex,
    pub modu: SymbolIndex,
    pub cons: SymbolIndex,
    pub hd: SymbolIndex,
    pub tl: SymbolIndex,
    pub quote: SymbolIndex,
}
impl DefaultNS {
    pub fn new(env: &mut Env) -> Self {
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
