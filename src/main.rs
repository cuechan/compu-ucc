#![recursion_limit = "8192"]
mod parser;

// use simple_logger;
use itertools::Itertools;
use log;
use log::{debug, error, info, trace, warn};
use nom::number::complete as number;
use nom::Parser;
use parser::Label;
use parser::Numeric;
use parser::SyntaxElement;
use petgraph::csr::IndexType;
use petgraph::data::Build;
use petgraph::dot::{Config, Dot};
use petgraph::graph::{DiGraph, Node, NodeIndex, UnGraph};
use petgraph::visit::NodeRef;
use petgraph::{Directed, Graph};
use prettytable::{row, Table};
use std::arch::x86_64;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::fs::read_to_string;
use std::io::{stdin, Read};
use std::ops::Deref;
use std::process::exit;
use std::vec;

type CfGraph = DiGraph<String, String>;

const OPCODE_BITS: u32 = 7;
const UCSTEPS_BITS: u32 = 6;
const FLAG_Z: u64 = 0b01;
const FLAG_C: u64 = 0b10;

fn read_lines_from_stdin() -> String {
    let mut result = String::new();
    stdin().read_to_string(&mut result).unwrap();
    result
}

fn main() {
    simplelog::TermLogger::init(
        log::LevelFilter::Debug,
        simplelog::Config::default(),
        simplelog::TerminalMode::Stderr,
        simplelog::ColorChoice::Auto,
    )
    .unwrap();

    let input = read_lines_from_stdin();

    let (r, ast) = parser::parse_block.parse(&input).unwrap();
    if r.len() > 0 {
        error!("remaining: '{}'", r);
        panic!("see previous messages");
    }

    info!("AST:");
    info!("{:#?}", ast);

    let uct = build_uc_table(ast);

    for x in uct {
        println!("{:04x} ", x);
    }
}

fn ast_to_cfg(t: SyntaxElement) -> CfGraph {
    let mut g: CfGraph = CfGraph::new();
    let root_node = iterate_to_cfg(&t, &mut g);
    g
}

fn iterate_to_cfg(t: &SyntaxElement, g: &mut CfGraph) -> NodeIndex {
    // let mut nb = vec![];

    // g.add_node(t.clone());
    debug!("processing {:?}", t);

    // g.add_edge(NodeIndex::new(parent), i, ());

    match t {
        SyntaxElement::Block(b) => {
            let mut bl = b.clone();
            if !bl.is_empty() {
                let f = bl.remove(0);
                let nc = iterate_to_cfg(&f, g);

                iterate_to_cfg(&SyntaxElement::Block(bl.to_vec()), g)
            } else {
                g.add_node(format!("_end"))
            }
        }
        SyntaxElement::Enable(a) => {
            let i = g.add_node(format!("enable {}", a.join(", ")));
            // g.add_edge(i, String::from("enable"));
            i
        }
        SyntaxElement::If(cond, flag, b) => {
            let i = g.add_node(format!("if ({} == {})", flag, cond));

            // True block
            let cb = iterate_to_cfg(b, g);
            // g.add_edge(i, cb, String::from("true"));
            // False block
            // g.add_edge(i, parent, String::from("false"));
            i
        }
        _ => {
            error!("SyntaxElement: {:?} not implemented", t);
            unimplemented!();
        }
    }
}

fn expand_ast(t: SyntaxElement) -> SyntaxElement {
    let f = iterate_expand(&t);
    f
}

fn iterate_expand(t: &SyntaxElement) -> SyntaxElement {
    let mut nb = vec![];

    if let SyntaxElement::Block(block) = t {
        for i in block {
            debug!("processing {:?}", i);

            match i {
                SyntaxElement::Define(k, v) => nb.push(SyntaxElement::Define(k.clone(), *v)),
                SyntaxElement::InstructionDeclaration { mnemoric, opcode } => {
                    nb.push(SyntaxElement::InstructionDeclaration {
                        mnemoric: mnemoric.clone(),
                        opcode: *opcode,
                    })
                }
                SyntaxElement::Enable(a) => nb.push(SyntaxElement::Enable(a.clone())),
                SyntaxElement::If(i, flag, b) => {
                    nb.push(SyntaxElement::If(
                        *i,
                        flag.clone(),
                        Box::new(iterate_expand(b)),
                    ));
                    nb.push(SyntaxElement::If(
                        !*i,
                        flag.clone(),
                        Box::new(iterate_expand(b)),
                    ));
                }
                _ => unimplemented!(),
            };
        }
    };

    SyntaxElement::Block(nb)
}

#[derive(Debug, Clone)]
struct Flags {
    carry: Option<bool>,
    zero: Option<bool>,
}

#[derive(Debug, Clone)]
struct Context {
    define_table: HashMap<String, Numeric>,
    rev_define_table: HashMap<Numeric, String>,
    instructions: HashMap<String, u64>,
    uct: HashMap<u16, u32>,
    flags: Vec<Flags>,
    instr: Option<(String, u64)>,
    ui_ctr: u64,
}

impl Context {
    fn new() -> Self {
        Self {
            define_table: init_defines().0,
            instructions: HashMap::new(),
            uct: init_uct(),
            flags: vec![Flags {
                carry: None,
                zero: None,
            }],
            instr: None,
            ui_ctr: 0,
            rev_define_table: init_defines().1,
        }
    }

    fn instruction_name(&self, opcode: u64) -> String {
        match self.instructions.iter().find(|(_, v)| **v == opcode) {
            Some((_, v)) => v.to_string(),
            None => "undefined".to_string()
        }
    }
}

fn build_uc_table(tree: SyntaxElement) -> Vec<u32> {
    let mut ctx = Context::new();

    // prefill table
    for i in 0..(2_u64.pow(15)) {
        ctx.uct.insert(i as u16, 0);
    }

    if let SyntaxElement::Block(b) = tree {
        process_block(&mut ctx, &b);
    }

    let mut uct: Vec<u32> = Vec::new();

    let mut table = Table::new();
    let pt_format = prettytable::format::FormatBuilder::new()
        .column_separator('|')
        .borders('|')
        .separators(
            &[prettytable::format::LinePosition::Top, prettytable::format::LinePosition::Bottom],
            prettytable::format::LineSeparator::new('-', '+', '+', '+'),
        )
        .padding(1, 1)
        .build();

    table.set_format(pt_format);
    table.set_titles(row!["step", "opcode", "Z", "C", "enables"]);

    for k in ctx.uct.keys().sorted() {
        let v = ctx.uct.get(k).unwrap();

        table.add_row(row![
            format!("{:2x}", (k >> 2) & 0b111111),
            format!("{} (0x{:04x})", ctx.instruction_name((k >> 8) as u64) , (k >> 8)),
            if (k >> 1) & 0b1 == 1 { "Z" } else { "-" },
            if (k) & 0b1 == 1 { "C" } else { "-" },
            int_to_bitidx(*v)
                .iter()
                .map(|x| { ctx.rev_define_table.get(&(*x as u64)).unwrap() })
                .join(" | ")
        ]);

        info!(
            "{:04x} = 0x{:04x}, {:02x}, {}, {}, {:?}",
            k,
            (k >> 8),
            (k >> 2) & 0b111111,
            if (k >> 1) & 0b1 == 1 { "Z" } else { "-" },
            if (k) & 0b1 == 1 { "C" } else { "-" },
            int_to_bitidx(*v)
                .iter()
                .map(|x| { ctx.rev_define_table.get(&(*x as u64)).unwrap() })
                .join(" | ")
        );
        uct.push(*v);
    }

    // eprin.printstd();
    uct
}

fn int_to_bitidx(n: u32) -> Vec<u8> {
    let mut v = vec![];
    for b in 0..31 {
        if ((n >> b) & 1) == 1 {
            v.push(b)
        }
    }
    v
}

#[test]
fn test_int_to_bitidx() {
    assert_eq!(int_to_bitidx(5), vec![0, 2]);
    assert_eq!(int_to_bitidx(34952), vec![3, 7, 11, 15]);
}

fn process_block(mut ctx: &mut Context, block: &Vec<SyntaxElement>) {
    for i in block {
        debug!("processing {:?}", i);

        match i {
            SyntaxElement::Step => {
                ctx.ui_ctr += 1;
            },
            SyntaxElement::Define(k, v) => {
                if let Some(v) = ctx.define_table.insert(k.to_string(), *v) {
                    warn!("Definition '{}' is overwritten", k);
                }
            }
            SyntaxElement::InstructionDeclaration { mnemoric, opcode } => {
                ctx.ui_ctr = 0;
                ctx.instr = Some((mnemoric.to_owned(), *opcode));
                ctx.instructions.insert(mnemoric.to_string(), *opcode);
            }
            SyntaxElement::Enable(a) => {
                if let Some((mnemoric, opcode)) = ctx.instr.clone() {
                    for def in a {
                        if let Some(v) = ctx.define_table.get(def) {
                            let keys = build_keys(opcode, ctx.ui_ctr, ctx.flags.last().unwrap());

                            for addr in keys {
                                if let Some(x) = ctx.uct.get(&addr.try_into().unwrap()) {
                                    ctx.uct.insert(addr.try_into().unwrap(), 1 << *v as u32 | x);
                                } else {
                                    ctx.uct.insert(addr.try_into().unwrap(), 1 << *v as u32);
                                }
                            }
                        } else {
                            error!("Identifier '{}' is not known or defined", def);
                            panic!();
                        }
                    }
                } else {
                    error!("'enable' not allowd without previous 'instruction' preamble");
                    panic!();
                }
            }
            SyntaxElement::If(i, flag, b) => {
                let oldflags = ctx.flags.last().unwrap();
                let mut newflags: Flags = oldflags.clone();

                match flag.as_str() {
                    "carry" => {
                        // get flags from previous context
                        newflags.carry = Some(oldflags.carry.unwrap_or(*i) & i);
                        ctx.flags.push(newflags.clone());
                        if let SyntaxElement::Block(ref bx) = **b {
                            process_block(ctx, bx);
                        }
                        ctx.flags.pop().unwrap();
                        // ctx.ui_ctr -= 1;
                    }
                    "zero" => {
                        newflags.zero = Some(oldflags.zero.unwrap_or(*i) & i);
                        ctx.flags.push(newflags);

                        if let SyntaxElement::Block(ref bx) = **b {
                            process_block(ctx, bx);
                        }
                        ctx.flags.pop().unwrap();
                        // ctx.ui_ctr -= 1;
                    }
                    _ => {
                        error!("unknown flag '{}'", flag);
                        panic!();
                    }
                }
            }
            _ => unimplemented!(),
        }
    }
}

fn build_keys(opcode: u64, uc_ctr: u64, flags: &Flags) -> HashSet<u64> {
    let mut entries = HashSet::new();

    for i in 0..4 {
        entries.insert(((opcode << 8) | (uc_ctr << 2) | i));
    }

    if let Some(flag_c) = flags.carry {
        if flag_c {
            // carry flag must be true
            entries.remove(&((opcode << 8) | (uc_ctr << 2) | 0b00));
            entries.remove(&((opcode << 8) | (uc_ctr << 2) | 0b10));
        } else {
            // carry flag must be false
            entries.remove(&((opcode << 8) | (uc_ctr << 2) | 0b01));
            entries.remove(&((opcode << 8) | (uc_ctr << 2) | 0b11));
        }
    }

    if let Some(flag_z) = flags.zero {
        if flag_z {
            // flag must be true
            entries.remove(&((opcode << 8) | (uc_ctr << 2) | 0b00));
            entries.remove(&((opcode << 8) | (uc_ctr << 2) | 0b01));
        } else {
            // flag must be false
            entries.remove(&((opcode << 8) | (uc_ctr << 2) | 0b10));
            entries.remove(&((opcode << 8) | (uc_ctr << 2) | 0b11));
        }
    }

    entries
}

fn init_defines() -> (HashMap<String, u64>, HashMap<Numeric, String>) {
    let mut defs: HashMap<String, u64> = HashMap::new();
    let mut rdefs: HashMap<Numeric, String> = HashMap::new();

    defs.insert("HLT".to_string(), 0);
    defs.insert("RST_MICROINST".to_string(), 1);
    defs.insert("INC_MICROINST".to_string(), 2);
    defs.insert("INC_PC".to_string(), 8);
    defs.insert("EN_PC".to_string(), 9);

    for (k, v) in defs.iter() {
        rdefs.insert(*v, k.to_string());
    }

    (defs, rdefs)
}

fn init_uct() -> HashMap<u16, u32> {
    let mut uct: HashMap<u16, u32> = HashMap::new();

    // for i in 0..(2u32.pow(15)) {
    //     uct.insert(i as u16, 0);
    // }

    uct
}
