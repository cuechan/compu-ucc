#![allow(unused)]
use nom;
use nom::bytes::complete::{tag, tag_no_case};
use nom::combinator::{map_res, opt};
use nom::multi::{fold_many1, separated_list1, many0};
use nom::sequence::{tuple, separated_pair, preceded, delimited};
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
use std::arch::x86_64;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::io::{stdin, Read};
use std::vec;
use nom::number::complete as number;
use log::{error, warn, info, debug, trace};
use log;
use simple_logger;


type Label = String;
type Numeric = u64;


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





fn parse_if(input: &str) -> IResult<&str, A> {
    let (r, (_, _, i, _, f)) = tuple((
        tag_no_case("if"),
        space1,
        alphanumeric1,
        space1,
        delimited(
            tag("{"),
            parse_block,
            tag("}")
        )
    )).parse(input)?;

    Ok((r, A::If(i.to_string(), Box::new(f))))
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


#[derive(Debug, PartialEq, Eq)]
enum A {
    Block(Vec<A>),
    Enable(Vec<String>),
    Define(String, u64),
    InstructionDeclaration{
        mnemoric: String,
        opcode: Numeric,
    },
    If(String, Box<A>),
    Literal(u64),
}


type Assigment = (String, Numeric);


fn read_lines_from_stdin() -> String {
    let mut result = String::new();
    stdin().read_to_string(&mut result).unwrap();
    result
}


fn main() {
    simple_logger::init_with_level(log::Level::Trace);
    let input = read_lines_from_stdin();

    let (r, t) = parse_block.parse(&input).unwrap();

    println!("remaining: {:#?}", r);
    // println!("{:#?}", f);
    build_uc_table(t)
}


enum Flag {
    Carry = 1,
    Zero = 2
}


struct Context {
    define_table: HashMap<String, Numeric>,
    instructions: HashMap<String, Vec<String>>,
    uct: HashMap<u16, u32>,
    flags: (u32, u32),
    instr: Option<(String, u64)>,
    ui_ctr: u64,
}

impl Context {
    fn new() -> Self {
        Self {
            define_table: init_defines(),
            instructions: HashMap::new(),
            uct: HashMap::new(),
            flags: (0, 0),
            instr: None,
            ui_ctr: 0,
        }
    }
}

fn build_uc_table(tree: A) {
    let mut ctx = Context::new();

    if let A::Block(mut b) = tree {
        process_block(&mut ctx, b);
    }

    for (k, v) in ctx.uct {
        // println!(
        //     "{:07b} {:06b} {:02b} => {:b}",
        //     (k >> 8),
        //     (k >> 2) & 0b111111,
        //     (k) & 0b11,
        //     v
        // );

        println!(
            "// 0x{:04x}, {:02x}, {}, {}, {:032b};",
            (k >> 8),
            (k >> 2) & 0b111111,
            if (k >> 1) & 0b1 == 1 {"Z"} else {"-"},
            if (k) & 0b1 == 1 {"C"} else {"-"},
            v
        );

        // wrMicrInst(LDA, 0, 1, 0, INC_MICROINST, EN_PC, WR_MAR);
        // wrMicrInst(opcode, microstepp, zeroflag, carryflag, allepinsdieanseinsollen, ...);
        println!(
            "writeMicroCodeBin({}, {}, {}, {}, {});",
            (k >> 8),
            (k >> 2) & 0b111111,
            (k >> 1) & 0b1,
            (k) & 0b1,
            v
        );

        println!();
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


fn process_block(mut ctx: &mut Context, block: Vec<A>) {
    for i in block {
        debug!("processing {:?}", i);
        match i {
            A::Define(k, v) => {
                if let Some(v) = ctx.define_table.insert(k.to_string(), v) {
                    warn!("Definition '{}' is overwritten", k);
                }
            },
            A::InstructionDeclaration { mnemoric, opcode } => {
                ctx.instr = Some((mnemoric, opcode));
            },
            A::Enable(a) => {
                if let Some((mnemoric, opcode)) = ctx.instr.clone() {

                    for def in a {
                        if let Some(v) = ctx.define_table.get(&def) {
                            let keys = build_keys(
                                opcode,
                                ctx.ui_ctr,
                                ctx.flags.0 > 0,
                                ctx.flags.1 > 0
                            );

                            for addr in keys {
                                if let Some(x) = ctx.uct.get(&addr.try_into().unwrap()) {
                                    ctx.uct.insert(
                                        addr.try_into().unwrap(),
                                        *v as u32 | x
                                    );
                                }
                                else {
                                    ctx.uct.insert(
                                        addr.try_into().unwrap(),
                                        *v as u32
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
            A::If(flag, b) => {
                match flag.as_str() {
                    "carry" => {
                        ctx.flags.0 += 1;

                        if let A::Block(bx) = *b {
                            process_block(ctx, bx);
                        }
                    },
                    "zero" => {
                        ctx.flags.1 += 1;

                        if let A::Block(bx) = *b {
                            process_block(ctx, bx);
                        }

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

fn build_keys(opcode: u64, uc_ctr: u64, flag_c: bool, flag_z: bool) -> HashSet<u64> {
    let mut entries = HashSet::new();

    entries.insert( (opcode << 8) | (uc_ctr << 2) | 0b00 );
    entries.insert( (opcode << 8) | (uc_ctr << 2) | 0b01 );
    entries.insert( (opcode << 8) | (uc_ctr << 2) | 0b10 );
    entries.insert( (opcode << 8) | (uc_ctr << 2) | 0b11 );


    if flag_c {
        entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b00 ));
        entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b10 ));
    }
    if flag_z {
        entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b00 ));
        entries.remove( &((opcode << 8) | (uc_ctr << 2) | 0b01 ));
    }

    entries
}


fn init_defines() -> HashMap<String, u64>  {
    let mut defs: HashMap<String, u64> = HashMap::new();

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

    defs
}
