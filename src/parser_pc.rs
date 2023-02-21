use nom::{branch::alt, bytes::complete::tag, IResult, sequence::{delimited, pair}, character::complete::{space0, space1, alpha1, alphanumeric0, newline, multispace0, char}, error::{ParseError, Error, ErrorKind}, Parser, multi::{many0, many1}};

// Data types

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Decl {
    pub fn_name: String,
    pub args: Vec<String>,
    pub expr: Term
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    Num(i32),
    Var(String),
    // Neg(Box<Term>),
    Add(Box<Term>, Box<Term>),
    Sub(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
    Brn(Box<Term>, Box<Term>, Box<Term>),
    App(String, Vec<Term>)
}

// Parser

pub fn parse_program<'a>(input: &'a str) -> IResult<&'a str, Vec<Decl>> {
    let (rest, program) = many1(|s: &'a str| -> IResult<&'a str, Decl> {
        let (mut s, t) = decl(s)?;
        if let Ok((ia, _)) = empty_lines(s) {
            s = ia;
        }
        Ok((s, t))
    })(input)?;
    // TODO require empty
    // if rest.is_empty() {
    //     Ok(program)
    // } else {
        
    // }
    Ok(("", program))
}

pub fn empty_lines<'a>(input: &str) -> IResult<&str, ()> {
    let (input, _) = newline(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, ()))
}

fn decl<'a>(input: &'a str) -> IResult<&'a str, Decl> {
    let (input, fn_name) = identifier(input)?;
    let parse_args = many0(|s: &'a str| -> IResult<&'a str, String> {
        let (mut s, t) = identifier(s)?;
        if let Ok((ia, _)) = symbol(",")(s) {
            s = ia;
        }
        Ok((s, t))
    });
    let (input, args) = delimited(symbol("("), parse_args, symbol(")"))(input)?;
    let (input, _) = symbol("=")(input)?;
    let (input, expr) = expr(input)?;
    Ok((input, Decl {
        fn_name,
        args,
        expr
    }))
}

/**
 * Il parser delle espressioni implementa l'algoritmo precedence climbing
 */

pub fn expr(input: &str) -> IResult<&str, Term> {
    expr_helper(0, input)
}

fn expr_helper(priority: u8, input: &str) -> IResult<&str, Term> {
    let (mut input,  mut t1) = leaf(input)?;
    while let Ok((right_side, (op_cons, op_prio))) = operator(input) {
        if op_prio < priority {
            break;
        }
        // in precedence climbing si aggiunge 1 alla priorità solo se l'operatore matchato è left recursive
        // in questo caso sono tutti left recursive quindi aggiungo +1
        let (input2, t2) = expr_helper(op_prio + 1, right_side)?;
        t1 = op_cons(Box::new(t1.clone()), Box::new(t2));
        input = input2;
    }
    Ok((input, t1))
}

fn operator(input: &str) -> IResult<&str, (fn(Box<Term>, Box<Term>) -> Term, u8)> {
    let (input, op) = alt((
        symbol("+"),
        symbol("-"),
        symbol("*")
    ))(input)?;
    match op {
        "+" => Ok((input, (Term::Add, 1))),
        "-" => Ok((input, (Term::Sub, 1))),
        "*" => Ok((input, (Term::Mul, 2))),
        _   => panic!("The alternative on operators has matched something that was not an operator. This shouldn't happen.")
    }
}

fn leaf(input: &str) -> IResult<&str, Term> {
    alt((
        number,
        parenthesis,
        branch,
        application,
        variable
    ))(input)
}

fn number(input: &str) -> IResult<&str, Term> {
    let (input, n) = token(nom::character::complete::i32)(input)?;
    Ok((input, Term::Num(n)))
}

fn parenthesis(input: &str) -> IResult<&str, Term> {
    delimited(symbol("("), expr, symbol(")"))(input)
}

fn branch(input: &str) -> IResult<&str, Term> {
    let (input, _) = keyword("if")(input)?;
    let (input, pred) = expr(input)?;
    let (input, _) = keyword("then")(input)?;
    let (input, t1) = expr(input)?;
    let (input, _) = keyword("else")(input)?;
    let (input, t2) = expr(input)?;
    Ok((input, Term::Brn(Box::new(pred), Box::new(t1), Box::new(t2)))) 
}

fn variable(input: &str) -> IResult<&str, Term> {
    let (input, x) = token(identifier)(input)?;
    Ok((input, Term::Var(x)))
}

fn identifier(input: &str) -> IResult<&str, String> {
    // TODO _
    let (input, (fst, rest)) = pair(alpha1, alphanumeric0)(input)?;
    let x = fst.to_owned() + rest;
    let keywords = vec!["if", "then", "else"];
    if keywords.contains(&x.as_str()) {
        Err(nom::Err::Failure(Error {
            input,
            code: ErrorKind::Fail
        }))
    } else {
        Ok((input, x))
    }
}

fn application(input: &str) -> IResult<&str, Term> {
    let (input, name) = token(identifier)(input)?;
    let (input, list) = delimited(symbol("("), list_of_args, symbol(")"))(input)?;
    Ok((input, Term::App(name, list)))
}

fn list_of_args<'a>(input: &'a str) -> IResult<&str, Vec<Term>> {
    many0(|s: &'a str| -> IResult<&'a str, Term> {
        let (mut s, t) = expr(s)?;
        if let Ok((ia, _)) = symbol(",")(s) {
            s = ia;
        }
        Ok((s, t))
    })(input)
}

// Parsing utilities
fn keyword<'a>(sym: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(space0, tag(sym), space1)
}

fn symbol<'a>(sym: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    token(tag(sym))
}


fn token<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where 
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E> {
    delimited(space0, parser, space0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token() {
        assert_eq!(number("  \t  -5 \t\t\t"), Ok(("", Term::Num(-5))));
        assert_eq!(variable("  \t  x \t\t\t;"), Ok((";", Term::Var("x".to_owned()))));
        assert_eq!(variable(" x then"), Ok(("then", Term::Var("x".to_owned()))));
        assert!(number("  \t  - 5 \t\t\t").is_err());
        assert!(variable("(d").is_err());
    }

    #[test]
    fn test_symbol() {
        assert_eq!(symbol("if")("if x then 1 else 0"), Ok(("x then 1 else 0", "if")));
        assert_eq!(symbol("then")(" then 1 else 0"), Ok(("1 else 0", "then")));
        assert!(symbol("else")("1 else 0").is_err());
    }

    #[test]
    fn test_identifier() {
        assert_eq!(identifier("abc213"), Ok(("", "abc213".to_owned())));
        assert!(identifier("3abc213").is_err());
        assert_eq!(identifier("a21NS3S"), Ok(("", "a21NS3S".to_owned())));
        assert_eq!(identifier("a"), Ok(("", "a".to_owned())));
    }

    #[test]
    fn test_number() {
        assert_eq!(leaf("2)"), Ok((")", Term::Num(2))));
        assert_eq!(leaf("-15"), Ok(("", Term::Num(-15))));
        assert_eq!(leaf("6 + 65"), Ok(("+ 65", Term::Num(6))));
    }

    #[test]
    fn test_variable() {
        assert_eq!(leaf("hello world"), Ok(("world", Term::Var("hello".to_owned()))));
    }

    #[test]
    fn test_parenthesis() {
        assert_eq!(leaf("(7)"), Ok(("", Term::Num(7))));
        assert_eq!(leaf("(-15)"), Ok(("", Term::Num(-15))));
    }

    #[test]
    fn test_application() {
        assert_eq!(leaf("light()"), Ok(("", Term::App("light".to_owned(), vec![]))));
        assert_eq!(leaf("double(5)"), Ok(("", Term::App("double".to_owned(), vec![Term::Num(5)]))));
        assert_eq!(leaf("fib(6, 3)"), Ok(("", Term::App("fib".to_owned(), vec![Term::Num(6), Term::Num(3)]))));
    }

    #[test]
    fn test_branch() {
        assert_eq!(leaf("if x then 0 else 1"), Ok(("", Term::Brn(
            Box::new(Term::Var("x".to_owned())),
            Box::new(Term::Num(0)),
            Box::new(Term::Num(1))))));
            assert_eq!(leaf("if x then 0 else if y then 1 else 2"), Ok(("", Term::Brn(
                Box::new(Term::Var("x".to_owned())),
                Box::new(Term::Num(0)),
                Box::new(Term::Brn(
                    Box::new(Term::Var("y".to_owned())),
                    Box::new(Term::Num(1)),
                    Box::new(Term::Num(2))))))));
    }

    #[test]
    fn test_expr_5() {
        assert_eq!(expr("2 * 3"), Ok(("", Term::Mul(
            Box::new(Term::Num(2)),
            Box::new(Term::Num(3)))
        )));
        assert_eq!(expr("-2 * 3"), Ok(("", Term::Mul(
            Box::new(Term::Num(-2)),
            Box::new(Term::Num(3)))
        )));
        assert_eq!(expr("2 * -3"), Ok(("", Term::Mul(
            Box::new(Term::Num(2)),
            Box::new(Term::Num(-3))
        ))));
        assert_eq!(leaf("(2 * 3)"), Ok(("", Term::Mul(
            Box::new(Term::Num(2)),
            Box::new(Term::Num(3))
        ))));
        assert_eq!(expr("2 * 5 * k * w"), Ok(("", Term::Mul(
            Box::new(Term::Mul(
                Box::new(Term::Mul(
                    Box::new(Term::Num(2)), 
                    Box::new(Term::Num(5))
                )), 
                Box::new(Term::Var("k".to_owned()))
            )),
            Box::new(Term::Var("w".to_owned()))
        ))));
    }

    #[test]
    fn test_expr_4() {
        assert_eq!(expr("2 + 3"), Ok(("", Term::Add(
            Box::new(Term::Num(2)),
            Box::new(Term::Num(3)))
        )));
        assert_eq!(expr("-2 - 3"), Ok(("", Term::Sub(
            Box::new(Term::Num(-2)),
            Box::new(Term::Num(3)))
        )));
        assert_eq!(expr("2 + -3"), Ok(("", Term::Add(
            Box::new(Term::Num(2)),
            Box::new(Term::Num(-3))
        ))));
        assert_eq!(leaf("(5 + s)"), Ok(("", Term::Add(
            Box::new(Term::Num(5)),
            Box::new(Term::Var("s".to_owned()))
        ))));
        assert_eq!(expr("x + y * 2"), Ok(("", Term::Add(
            Box::new(Term::Var("x".to_owned())),
            Box::new(Term::Mul(
                Box::new(Term::Var("y".to_owned())),
                Box::new(Term::Num(2))))
        ))));
        assert_eq!(expr("x + y * 2 * 5 * k * w"), Ok(("", Term::Add(Box::new(Term::Var("x".to_owned())), 
        Box::new(Term::Mul(
            Box::new(Term::Mul(
                Box::new(Term::Mul(
                    Box::new(Term::Mul(
                        Box::new(Term::Var("y".to_owned())), 
                        Box::new(Term::Num(2))
                    )), 
                    Box::new(Term::Num(5))
                )), 
                Box::new(Term::Var("k".to_owned()))
            )), 
            Box::new(Term::Var("w".to_owned()))
        ))))));
    }

    #[test]
    fn test_decl() {
        let str = "fact(n) = if n then 1 else fact(n-1) * n";
        let expected = Decl {
            fn_name: "fact".to_owned(),
            args: vec!["n".to_owned()],
            expr: Term::Brn(
                Box::new(Term::Var("n".to_owned())), 
                Box::new(Term::Num(1)), 
                Box::new(Term::Mul(
                    Box::new(Term::App(
                        "fact".to_owned(), 
                        vec![Term::Sub(
                            Box::new(Term::Var("n".to_owned())),
                            Box::new(Term::Num(1))
                        )]
                    )),
                    Box::new(Term::Var("n".to_owned()))
                )))
        };
        assert_eq!(decl(str), Ok(("", expected.clone())));
        assert_eq!(decl("fact(n) = if n then 1 else fact(n-1) * n\nfact(n) = if n then 1 else fact(n-1) * n"), Ok(("\nfact(n) = if n then 1 else fact(n-1) * n", expected)));
    }
}