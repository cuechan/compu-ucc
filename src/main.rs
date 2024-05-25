#![allow(unused)]
use nom;
use nom::bytes::complete::{tag, tag_no_case};
use nom::combinator::{map_res, opt, success};
use nom::multi::{fold_many1, separated_list1, many0};
use nom::sequence::{tuple, separated_pair, preceded, delimited, terminated};
use nom::IResult;
use nom::Parser;
use nom::character::complete::{
    newline,
    digit1,
    multispace0,
    alpha1,
    space1,
    alphanumeric1,
    hex_digit1,
    space0,
    multispace1,
    char as nomchar
};
use nom::character;
use nom::branch::{self, alt};
use nom::bytes;
use petgraph::{Graph, Directed};
use petgraph::data::Build;
use petgraph::dot::{Config, Dot};
use petgraph::graph::DiGraph;
use std::arch::x86_64;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::io::{stdin, Read};
use std::ops::Deref;
use std::process::exit;
use std::vec;
use nom::number::complete as number;
use log::{error, warn, info, debug, trace};
use log;
use simple_logger;
use itertools::Itertools;


type Label = String;
type Numeric = u64;
type CfGraph = Graph<A, usize, Directed, usize>;

const FLAG_Z: u64 = 0b01;
const FLAG_C: u64 = 0b10;


fn parse_dec_number(input: &str) -> IResult<&str, Numeric> {
    map_res(digit1, |x: &str| Numeric::from_str_radix(x, 10)).parse(input)
}

#[test]
fn test_parse_dec_number() {
    assert_eq!(parse_dec_number("123"), Ok(("", 123)));
}


fn parse_hex_number(input: &str) -> IResult<&str, Numeric> {
    tuple((
        tag("0x"),
        map_res(
            hex_digit1,
            |x| Numeric::from_str_radix(x, 16)
        )
    )).map(|x| x.1)
    .parse(input)
}


#[test]
fn test_parse_hex_number() {
    assert_eq!(parse_hex_number("0xab32"), Ok(("", 0xab32)));
}


// fn parse_binary_digit(input: &str) -> IResult<&str, u64> {
// }

fn parse_number(input: &str) -> IResult<&str, Numeric> {
    alt((
        parse_dec_number,
        parse_hex_number
    )).parse(input)
}



fn parse_literal(input: &str) -> IResult<&str, A> {
    let (input, n) = digit1(input)?;
    Ok((input, A::Literal(n.parse().unwrap())))
}


fn parse_allowed_chars(input: &str) -> IResult<&str, &str> {
    alt((
        alphanumeric1,
        tag("_"),
        tag("-"),
    ))(input)
}

fn parse_identifier(input: &str) -> IResult<&str, String> {

    // FnMut(String) -> Result<String, String>
    // let allowed_chars = ;

    let (r, f) = fold_many1(
        parse_allowed_chars,
        String::new,
        |mut accs: String, item| {
            accs.push_str(&item);
            accs
        }
    ).parse(input)?;

    Ok((r, f))
}


#[test]
fn test_parse_identifier() {
    assert_eq!(parse_identifier("FooBar"), Ok(("", "FooBar".to_string())));
    assert_eq!(parse_identifier("Foo_Bar"), Ok(("", "Foo_Bar".to_string())));
    assert_eq!(parse_identifier("FOO-BAR"), Ok(("", "FOO-BAR".to_string())));
}


fn parse_keyword_enable(input: &str) -> IResult<&str, A> {
    let (r, (_, _, _, f, _)) = tuple((
        space0,
        tag_no_case("enable"),
        space1,
        separated_list1(space1, parse_identifier),
        space0
    )).parse(input)?;

    Ok((r, A::Enable(f)))
}

#[test]
fn test_parse_enable() {
    assert_eq!(
        parse_keyword_enable("enable FOO"),
        Ok((
            "",
            A::Enable(vec!["FOO".to_string()])
        ))
    );


    assert_eq!(parse_keyword_enable(" enable FOO"), Ok(("", A::Enable(vec!["FOO".to_string()]))));
    assert_eq!(parse_keyword_enable("enable  FOO"), Ok(("", A::Enable(vec!["FOO".to_string()]))));
}


fn parse_instruction_declaration(input: &str) -> IResult<&str, A> {
    // instruction JIZ = 42
    let (r, (_, _, _, a)) = tuple((
        space0,
        tag_no_case("instruction"),
        space1,
        parse_assignment
    ))(input)?;

    Ok((r, A::InstructionDeclaration {
        mnemoric: a.0,
        opcode: a.1
    }))
}


#[test]
fn test_parse_instruction_declaration() {
    assert_eq!(
        parse_instruction_declaration("instruction JIZ = 42"),
        Ok(("", A::InstructionDeclaration {
            mnemoric: "JIZ".to_string(),
            opcode: 42,
        }))
    );
    assert_eq!(
        parse_instruction_declaration("  instruction   JIZ  =   42  "),
        Ok(("", A::InstructionDeclaration {
            mnemoric: "JIZ".to_string(),
            opcode: 42,
        }))
    );
    assert_eq!(
        parse_instruction_declaration("instruction JIZ=42"),
        Ok(("", A::InstructionDeclaration {
            mnemoric: "JIZ".to_string(),
            opcode: 42,
        }))
    );
}



fn parse_assignment(input: &str) -> IResult<&str, Assigment> {
    // instruction JIZ = 42
    let (r, (k, v)) = delimited(
        opt(space1),
        separated_pair(
            delimited(space0, parse_identifier, space0),
            character::complete::char('='),
            delimited(space0, parse_number, space0),
        ),
        opt(space1)
    )(input)?;

    Ok((r, (k.to_string(), v)))
}


#[test]
fn test_parse_assignment() {
    assert_eq!(
        parse_assignment("EN_INCR = 1"),
        Ok((
            "",
            (
                "EN_INCR".to_string(),
                1
            )
        ))
    );
    assert_eq!(
        parse_assignment("  foo  =  123  "),
        Ok((
            "",
            (
                "foo".to_string(),
                123
            )
        ))
    );
}


fn parse_define(input: &str) -> IResult<&str, A> {
    // instruction JIZ = 42
    let (r, a) = preceded(
        delimited(
            space0,
            tag_no_case("define"),
            space1,
        ),
        parse_assignment
    )(input)?;


    Ok((r, A::Define(a.0, a.1)))
}

#[test]
fn test_parse_define() {
    assert_eq!(
        parse_define("define EN_INCR = 1"),
        Ok((
            "",
            A::Define(
                "EN_INCR".to_string(),
                1
            )
        ))
    );
    assert_eq!(
        parse_define("define  foo  =  123  "),
        Ok((
            "",
            A::Define(
                "foo".to_string(),
                123
            )
        ))
    );
}


fn parse_condition(input: &str) -> IResult<&str, (bool, String)> {
    let (r, (c, f)) = tuple((
        opt(alt((
            terminated(
                tag_no_case("!"),
                space0
            ),
            terminated(
                tag_no_case("not"),
                space1
            )
        ))),
        alpha1
    )).parse(input)?;


    Ok((r, (c.is_some(), f.to_string())))
}


fn parse_if(input: &str) -> IResult<&str, A> {
    let (r, (_, _, (i, c), _, f)) = tuple((
        tag_no_case("if"),
        space1,
        parse_condition,
        space1,
        delimited(
            tag("{"),
            parse_block,
            tag("}")
        )
    )).parse(input)?;

    Ok((r, A::If(!i, c, Box::new(f))))
}


#[derive(Debug, PartialEq, Eq)]
struct InstructionDeclaration {

}

// fn parse_mnemoric(input: &str) -> IResult<&str, A> {
//     let (input, mut m) = tuple((
//         alpha1,
//         separated_list0(space1, alphanumeric1)
//     ))(input)?;

//     Ok((input, A::Mnemoric(
//         m.0.to_string(),
//         m.1.iter_mut().map(|s| s.to_string()).collect()
//     )))
// }


fn parse_block(input: &str) -> IResult<&str, A> {
    let (r, f) = many0(delimited(
        opt(multispace0),
        alt((
            parse_define,
            parse_instruction_declaration,
            parse_keyword_enable,
            parse_if,
        )),
        opt(multispace0)
    )).parse(&input)?;

    Ok((r, A::Block(f)))
}


#[derive(Debug, Clone, PartialEq, Eq)]
enum A {
    Block(Vec<A>),
    Enable(Vec<String>),
    Define(String, u64),
    InstructionDeclaration{
        mnemoric: String,
        opcode: Numeric,
    },
    If(bool, String, Box<A>),
    Literal(u64),
    Not,
}


type Assigment = (String, Numeric);


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
        simplelog::ColorChoice::Auto
    );

    let input = read_lines_from_stdin();

    let (r, t) = parse_block.parse(&input).unwrap();
    if r.len() > 0 {
        error!("remaining: '{}'", r);
        exit(1);
    }

    info!("AST:");
    info!("{:#?}", t);

    // let expanded_ast = expand_ast(t);

    let g = ast_to_cfg(t);

    println!("{:#?}", Dot::with_config(&g, &[]));

    // build_uc_table(expanded_ast)
}



fn ast_to_cfg(t: A) ->  CfGraph {
    let mut g: CfGraph = Graph::new();

    let f = iterate_to_cfg(&t, g);
    g
}

fn iterate_to_cfg(t: &A, mut g: CfGraph) {
    let mut nb = vec![];

    if let A::Block(block) = t {

        for i in block {
            g.add_node(i.clone());

            debug!("processing {:?}", i);

            match i {
                A::Define(k, v) => nb.push(A::Define(k.clone(), *v)),
                A::InstructionDeclaration { mnemoric, opcode } => nb.push(A::InstructionDeclaration { mnemoric: mnemoric.clone(), opcode: *opcode }),
                A::Enable(a) => nb.push(A::Enable(a.clone())),
                A::If(i, flag, b) => {
                    Box::new(iterate_to_cfg(b, g));
                },
                _ => unimplemented!()
            };
        }
    };
}




fn expand_ast(t: A) -> A {
    let f = iterate_expand(&t);
    f
}

fn iterate_expand(t: &A) -> A {
    let mut nb = vec![];

    if let A::Block(block) = t {
        for i in block {
            debug!("processing {:?}", i);

            match i {
                A::Define(k, v) => nb.push(A::Define(k.clone(), *v)),
                A::InstructionDeclaration { mnemoric, opcode } => nb.push(A::InstructionDeclaration { mnemoric: mnemoric.clone(), opcode: *opcode }),
                A::Enable(a) => nb.push(A::Enable(a.clone())),
                A::If(i, flag, b) => {
                    nb.push(A::If(*i, flag.clone(), Box::new(iterate_expand(b))));
                    nb.push(A::If(!*i, flag.clone(), Box::new(iterate_expand(b))));
                },
                _ => unimplemented!()
            };
        }
    };

    A::Block(nb)
}

#[derive(Debug, Clone)]
struct Flags {
    carry: Option<bool>,
    zero: Option<bool>
}


#[derive(Debug, Clone)]
struct Context {
    define_table: HashMap<String, Numeric>,
    rev_define_table: HashMap<Numeric, String>,
    instructions: HashMap<String, Vec<String>>,
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
            flags: vec![Flags {carry: None, zero: None}],
            instr: None,
            ui_ctr: 0,
            rev_define_table: init_defines().1
        }
    }
}

fn build_uc_table(tree: A) {
    let mut ctx = Context::new();

    if let A::Block(mut b) = tree {
        process_block(&mut ctx, &b);
    }

    for k in ctx.uct.keys().sorted() {
        let v = ctx.uct.get(k).unwrap();
        // println!(
        //     "{:07b} {:06b} {:02b} => {:b}",
        //     (k >> 8),
        //     (k >> 2) & 0b111111,
        //     (k) & 0b11,
        //     v
        // );

        info!(
            "{:04x} = 0x{:04x}, {:02x}, {}, {}, {:?};",
            k,
            (k >> 8),
            (k >> 2) & 0b111111,
            if (k >> 1) & 0b1 == 1 {"Z"} else {"-"},
            if (k) & 0b1 == 1 {"C"} else {"-"},
            int_to_bitidx(*v)
                .iter()
                .map(|x| {
                    ctx.rev_define_table.get(&(*x as u64)).unwrap()
                })
                .join(" | ")
        );

        // wrMicrInst(LDA, 0, 1, 0, INC_MICROINST, EN_PC, WR_MAR);
        // wrMicrInst(opcode, microstepp, zeroflag, carryflag, allepinsdieanseinsollen, ...);
        // println!(
        //     "writeMicroCodeBin({}, {}, {}, {}, {}); // {}",
        //     (k >> 8),
        //     (k >> 2) & 0b111111,
        //     (k >> 1) & 0b1,
        //     (k) & 0b1,
        //     v,
        //     int_to_bitidx(*v)
        //         .iter()
        //         .map(|x| {
        //             ctx.rev_define_table.get(&(*x as u64)).unwrap()
        //         })
        //         .join(" | ")
        // );
    }
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


fn process_block(mut ctx: &mut Context, block: &Vec<A>) {
    for i in block {
        debug!("processing {:?}", i);

        match i {
            A::Define(k, v) => {
                if let Some(v) = ctx.define_table.insert(k.to_string(), *v) {
                    warn!("Definition '{}' is overwritten", k);
                }
            },
            A::InstructionDeclaration { mnemoric, opcode } => {
                ctx.ui_ctr = 1;
                ctx.instr = Some((mnemoric.to_owned(), *opcode));
            },
            A::Enable(a) => {
                if let Some((mnemoric, opcode)) = ctx.instr.clone() {
                    for def in a {
                        if let Some(v) = ctx.define_table.get(def) {

                            let keys = build_keys(
                                opcode,
                                ctx.ui_ctr,
                                ctx.flags.last().unwrap()
                            );

                            for addr in keys {
                                if let Some(x) = ctx.uct.get(&addr.try_into().unwrap()) {
                                    ctx.uct.insert(
                                        addr.try_into().unwrap(),
                                        1 << *v as u32 | x
                                    );
                                }
                                else {
                                    ctx.uct.insert(
                                        addr.try_into().unwrap(),
                                        1 << *v as u32
                                    );
                                }
                            }
                        } else {
                            error!("Identifier '{}' is not known or defined", def);
                            panic!();
                        }
                    }
                    ctx.ui_ctr += 1;
                } else {
                    error!("'enable' not allowd without previous 'instruction' preamble");
                    panic!();
                }
            },
            A::If(i, flag, b) => {
                let mut oldflags = ctx.flags.last().unwrap();
                let mut newflags: Flags = oldflags.clone();

                match flag.as_str() {
                    "carry" => {
                        newflags.carry = Some(oldflags.carry.unwrap_or(*i) & i);
                        ctx.flags.push(newflags.clone());
                        if let A::Block(ref bx) = **b {
                            process_block(ctx, bx);
                        }
                        ctx.flags.pop();


                        newflags.carry = Some(!newflags.carry.unwrap());
                        ctx.flags.push(newflags);
                        if let A::Block(ref bx) = **b {
                            process_block(ctx, bx);
                        }
                        ctx.flags.pop();
                    },
                    "zero" => {
                        newflags.zero = Some(oldflags.zero.unwrap_or(*i) & i);
                        ctx.flags.push(newflags);

                        if let A::Block(ref bx) = **b {
                            process_block(ctx, bx);
                        }
                        ctx.flags.pop().expect("no more context on stack");
                    },
                    _ => {
                        error!("unknown flag '{}'", flag);
                        panic!();
                    }
                }



            }
            _ => unimplemented!()
        }
    }
}

fn build_keys(opcode: u64, uc_ctr: u64, flags: &Flags) -> HashSet<u64> {
    let mut entries = HashSet::new();

    for i in 0..4 {
        entries.insert(((opcode << 8) | (uc_ctr << 2) | i ));
    }

    if let Some(flag_c) = flags.carry {
        if flag_c {
            // carry flag must be true
            entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b00 ));
            entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b10 ));
        } else {
            // carry flag must be false
            entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b01 ));
            entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b11 ));
        }
    }

    if let Some(flag_z) = flags.zero {
        if flag_z {
            // flag must be true
            entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b00 ));
            entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b01 ));
        } else {
            // flag must be false
            entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b10 ));
            entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b11 ));
        }
    }

    entries
}


fn init_defines() -> (HashMap<String, u64>, HashMap<Numeric, String>)  {
    let mut defs: HashMap<String, u64> = HashMap::new();
    let mut rdefs: HashMap<Numeric, String> = HashMap::new();

    defs.insert("HLT".to_string(), 0);
    defs.insert("RST_MICROINST".to_string(), 1);
    defs.insert("INC_MICROINST".to_string(), 2);
    defs.insert("EN_REG1".to_string(), 3);
    defs.insert("WR_REG1".to_string(), 4);
    defs.insert("SHIFT_REG1".to_string(), 5);
    defs.insert("EN_REG2".to_string(), 6);
    defs.insert("WR_REG2".to_string(), 7);
    defs.insert("SHIFT_REG2".to_string(), 8);
    defs.insert("WR_MAR".to_string(), 9);
    defs.insert("CE_RAM".to_string(), 10);
    defs.insert("WR_RAM".to_string(), 11);
    defs.insert("OE_RAM".to_string(), 12);
    defs.insert("SET_FLAGS_ALU".to_string(), 13);
    defs.insert("EN_A_ALU".to_string(), 14);
    defs.insert("WR_A_ALU".to_string(), 15);
    defs.insert("EN_AND_ALU".to_string(), 16);
    defs.insert("EN_OR_ALU".to_string(), 17);
    defs.insert("EN_XOR_ALU".to_string(), 18);
    defs.insert("EN_IR".to_string(), 19);
    defs.insert("WR_IR".to_string(), 20);
    defs.insert("INC_PC".to_string(), 21);
    defs.insert("EN_PC".to_string(), 22);
    defs.insert("LOAD_PC".to_string(), 23);
    defs.insert("CARRY_IN_ALU".to_string(), 24);
    defs.insert("EN_SHIFT_L_ALU".to_string(), 25);
    defs.insert("EN_SHIFT_R_ALU".to_string(), 26);
    defs.insert("EN_B_ALU".to_string(), 27);
    defs.insert("WR_B_ALU".to_string(), 28);
    defs.insert("EN_NOT_ALU".to_string(), 29);
    defs.insert("EN_SUM_ALU".to_string(), 30);
    defs.insert("EN_SUB_ALU".to_string(), 31);

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
