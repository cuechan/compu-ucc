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
use petgraph::data::Build;
use petgraph::dot::{Config, Dot};
use petgraph::graph::DiGraph;

pub type Label = String;
pub type Numeric = u64;


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



fn parse_literal(input: &str) -> IResult<&str, SyntaxElement> {
    let (input, n) = digit1(input)?;
    Ok((input, SyntaxElement::Literal(n.parse().unwrap())))
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


fn parse_keyword_enable(input: &str) -> IResult<&str, SyntaxElement> {
    let (r, (_, _, _, f, _)) = tuple((
        space0,
        tag_no_case("enable"),
        space1,
        separated_list1(space1, parse_identifier),
        space0
    )).parse(input)?;

    Ok((r, SyntaxElement::Enable(f)))
}

fn parse_keyword_step(input: &str) -> IResult<&str, SyntaxElement> {
    let (r, (_, f, _)) = tuple((
        space0,
        tag_no_case("step"),
        space0,
    )).parse(input)?;

    Ok((r, SyntaxElement::Step))
}

#[test]
fn test_parse_enable() {
    assert_eq!(
        parse_keyword_enable("enable FOO"),
        Ok((
            "",
            SyntaxElement::Enable(vec!["FOO".to_string()])
        ))
    );

    assert_eq!(parse_keyword_enable(" enable FOO"), Ok(("", SyntaxElement::Enable(vec!["FOO".to_string()]))));
    assert_eq!(parse_keyword_enable("enable  FOO"), Ok(("", SyntaxElement::Enable(vec!["FOO".to_string()]))));
}


fn parse_instruction_declaration(input: &str) -> IResult<&str, SyntaxElement> {
    // instruction JIZ = 42
    let (r, (_, _, _, a)) = tuple((
        space0,
        tag_no_case("instruction"),
        space1,
        parse_assignment
    ))(input)?;

    Ok((r, SyntaxElement::InstructionDeclaration {
        mnemoric: a.0,
        opcode: a.1
    }))
}


#[test]
fn test_parse_instruction_declaration() {
    assert_eq!(
        parse_instruction_declaration("instruction JIZ = 42"),
        Ok(("", SyntaxElement::InstructionDeclaration {
            mnemoric: "JIZ".to_string(),
            opcode: 42,
        }))
    );
    assert_eq!(
        parse_instruction_declaration("  instruction   JIZ  =   42  "),
        Ok(("", SyntaxElement::InstructionDeclaration {
            mnemoric: "JIZ".to_string(),
            opcode: 42,
        }))
    );
    assert_eq!(
        parse_instruction_declaration("instruction JIZ=42"),
        Ok(("", SyntaxElement::InstructionDeclaration {
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


fn parse_define(input: &str) -> IResult<&str, SyntaxElement> {
    // instruction JIZ = 42
    let (r, a) = preceded(
        delimited(
            space0,
            tag_no_case("define"),
            space1,
        ),
        parse_assignment
    )(input)?;


    Ok((r, SyntaxElement::Define(a.0, a.1)))
}

#[test]
fn test_parse_define() {
    assert_eq!(
        parse_define("define EN_INCR = 1"),
        Ok((
            "",
            SyntaxElement::Define(
                "EN_INCR".to_string(),
                1
            )
        ))
    );
    assert_eq!(
        parse_define("define  foo  =  123  "),
        Ok((
            "",
            SyntaxElement::Define(
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


fn parse_if(input: &str) -> IResult<&str, SyntaxElement> {
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

    Ok((r, SyntaxElement::If(!i, c, Box::new(f))))
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


pub fn parse_block(input: &str) -> IResult<&str, SyntaxElement> {
    let (r, f) = many0(delimited(
        opt(multispace0),
        alt((
            parse_keyword_step,
            parse_define,
            parse_instruction_declaration,
            parse_keyword_enable,
            parse_if,
        )),
        opt(multispace0)
    )).parse(&input)?;

    Ok((r, SyntaxElement::Block(f)))
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxElement {
    Block(Vec<SyntaxElement>),
    Enable(Vec<String>),
    Define(String, u64),
    InstructionDeclaration{
        mnemoric: String,
        opcode: Numeric,
    },
    If(bool, String, Box<SyntaxElement>),
    Literal(u64),
    Step,
    Not,
}

type Assigment = (String, Numeric);
