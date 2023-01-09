use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::combinator::all_consuming;
use nom::{
    IResult, 
    Parser
};
use nom::error::{
    ParseError, 
    Error, 
    ErrorKind
};
use nom::character::complete::{
    alphanumeric0,
    alpha1,
    space0,
    space1, newline
};
use nom::sequence::{
    pair, delimited
};
use nom::multi::many0;

#[derive(Debug, PartialEq, Clone)]
pub struct Decl {
    pub fn_name: String,
    pub args: Vec<String>,
    pub term: Term
}

pub fn parse_program<'a>(input: &'a str) -> IResult<&'a str, Vec<Decl>> {
    
    let (input, decl) = declaration(input)?;

    let (_, mut decls) = all_consuming(many0(|s: &'a str| -> IResult<&'a str, Decl> {
        let (s, _) = newline(s)?;
        declaration(s)
    }))(input)?;

    decls.insert(0, decl);
    Ok(("", decls))
}

fn declaration<'a>(input: &'a str) -> IResult<&'a str, Decl> {
    let (input, fn_name) = identifier(input)?;
    let (input, _) = symbol("(")(input)?;
    let (input, args) = many0(|s: &'a str| -> IResult<&'a str, String> {
        let (mut s, arg) = identifier(s)?;
        if let Ok((ia, _)) = symbol(",")(s) {
            s = ia;
        }
        Ok((s, arg))
    })(input)?;
    let (input, _) = symbol(")")(input)?;
    let (input, _) = symbol("=")(input)?;
    let (input, term) = expression(input)?;
    Ok((input, Decl{
        fn_name,
        args,
        term
    }))
}

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Num(i32),
    Var(String),
    Add(Box<Term>, Box<Term>),
    Sub(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
    Brn(Box<Term>, Box<Term>, Box<Term>),
    App(String, Vec<Term>)
}

fn expression(input: &str) -> IResult<&str, Term> {
    alt((
        branch,
        expression_1
    ))(input)
}

fn branch(input: &str) -> IResult<&str, Term> {
    let (input, _) = keyword("if")(input)?;
    let (input, predicate) = expression(input)?;
    let (input, _) = keyword("then")(input)?;
    let (input, t1) = expression(input)?;
    let (input, _) = keyword("else")(input)?;
    let (input, t2) = expression(input)?;
    Ok((input, Term::Brn(Box::new(predicate), Box::new(t1), Box::new(t2)))) 
}

// così com'è la differenza non ha associatività (devi mettere le parentesi)
// l'implementazione è left-associative e l'ho presa da un altro progetto
// potrebbe essere forse più carino fare un'implementazione più seria ma per ora evito
fn expression_1<'a>(input: &'a str) -> IResult<&'a str, Term> {
    let (input, t1) = expression_2(input)?;
    let try_add = |s: &'a str| -> IResult<&'a str, Term> {
        let (s, _) = symbol("+")(s)?;
        expression_1(s)
    };
    let try_sub = |s: &'a str| -> IResult<&'a str, Term> {
        let (s, _) = symbol("-")(s)?;
        expression_2(s) // TODO come mai qui è 2 e non 1?
    };
    if let Ok((input, t2)) = try_add(input) {
        Ok((input, Term::Add(Box::new(t1), Box::new(t2))))
    } else if let Ok((input, t2)) = try_sub(input) {
        Ok((input, Term::Sub(Box::new(t1), Box::new(t2))))
    } else {
        Ok((input, t1))
    }
}

fn expression_2<'a>(input: &'a str) -> IResult<&'a str, Term> {
    let try_add = |s: &'a str|-> IResult<&'a str, Term> {
        let (s, _) = symbol("*")(s)?;
        let (s, t) = expression_2(s)?;
        Ok((s, t))
    };
    let (input, t1) = atomic_expression(input)?;
    if let Ok((input, t2)) = try_add(input) {
        Ok((input, Term::Mul(Box::new(t1), Box::new(t2))))
    } else {
        Ok((input, t1))
    }
}

fn atomic_expression(input: &str) -> IResult<&str, Term> {
    let (input, t) = alt((
        function_call,
        variable,
        number,
        parenthesis_expression
    ))(input)?;
    Ok((input, t))
}

fn function_call<'a>(input: &'a str) -> IResult<&'a str, Term> {
    let (input, fn_name) = identifier(input)?;
    let (input, _) = symbol("(")(input)?;
    let (input, args) = many0(|s: &'a str| -> IResult<&'a str, Term> {
        let (mut s, t) = expression(s)?;
        if let Ok((ia, _)) = symbol(",")(s) {
            s = ia;
        }
        Ok((s, t))
    })(input)?;
    let (input, _) = symbol(")")(input)?;
    Ok((input, Term::App(fn_name, args)))
}

fn parenthesis_expression(input: &str) -> IResult<&str, Term> {
    let (input, _) = symbol("(")(input)?;
    let (input, t) = expression(input)?;
    let (input, _) = symbol(")")(input)?;
    Ok((input, t))
}

fn variable(input: &str) -> IResult<&str, Term> {
    let (input, x) = token(identifier)(input)?;
    Ok((input, Term::Var(x)))
}

fn number(input: &str) -> IResult<&str, Term> {
    let (input, n) = token(nom::character::complete::i32)(input)?;
    Ok((input, Term::Num(n)))
}

fn identifier(input: &str) -> IResult<&str, String> {
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
    fn test_declaration() {
        let test1 = Decl {
            fn_name: "fib".to_owned(),
            args: vec!["x".to_owned()],
            term: Term::Brn(
                Box::new(Term::Var("x".to_owned())),
                Box::new(Term::Num(1)),
                Box::new(Term::Brn(
                    Box::new(Term::Sub(
                        Box::new(Term::Var("x".to_owned())),
                        Box::new(Term::Num(1))
                    )),
                    Box::new(Term::Num(1)),
                    Box::new(Term::Add(
                        Box::new(Term::App(
                            "fib".to_owned(), 
                            vec![Term::Sub(
                                Box::new(Term::Var("x".to_owned())),
                                Box::new(Term::Num(1))
                            )]
                        )),
                        Box::new(Term::App(
                            "fib".to_owned(), 
                            vec![Term::Sub(
                                Box::new(Term::Var("x".to_owned())),
                                Box::new(Term::Num(2))
                            )]
                        ))
                    ))
                ))
            )
        };
        let expr1 = "fib(x) = if x then 1 else ( if x-1 then 1 else (fib(x-1) + fib(x-2)))";
        assert_eq!(declaration(expr1), Ok(("", test1)));
    }

    #[test]
    fn test_ident() {
        assert_eq!(identifier("abc213"), Ok(("", "abc213".to_owned())));
        assert!(identifier("3abc213").is_err());
        assert_eq!(identifier("a21NS3S"), Ok(("", "a21NS3S".to_owned())));
        assert_eq!(identifier("a"), Ok(("", "a".to_owned())));
    }

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
    fn test_branch() {
        let test1 = Term::Brn(
            Box::new(Term::Var("x".to_owned())),
            Box::new(Term::Num(1)),
            Box::new(Term::Var("y".to_owned()))
        );
        assert_eq!(branch("if x then 1 else y"), Ok(("", test1)));
        assert!(branch("1+2").is_err());
        assert!(branch("if 1 then2 else 3").is_err());
    }

    #[test]
    fn test_parenthesis_expression() {
        let test1 = Term::Num(1);
        assert_eq!(parenthesis_expression("(1)"), Ok(("", test1)));
        assert!(parenthesis_expression("()").is_err());
    }

    #[test]
    fn test_expression_2() {
        let test1 = Term::Mul(Box::new(Term::Var("x".to_owned())), Box::new(Term::Num(2)));
        assert_eq!(expression_2("x * 2"), Ok(("", test1)));
        let test2 = Term::Mul(Box::new(Term::Num(-5)), Box::new(Term::Num(-23)));
        assert_eq!(expression_2("-5*-23y"), Ok(("y", test2)));
    }

    #[test]
    fn test_expression_1() {
        let test1 = Term::Add(Box::new(Term::Var("x".to_owned())), Box::new(Term::Num(1)));
        assert_eq!(expression_1("x +1"), Ok(("", test1)));
        let test2 = Term::Sub(Box::new(Term::Num(-5)), Box::new(Term::Num(-23)));
        assert_eq!(expression_1("-5--23y"), Ok(("y", test2)));
        let test2 = Term::Sub(Box::new(Term::Var("x".to_owned())), Box::new(Term::Num(23)));
        assert_eq!(expression_1("x-23"), Ok(("", test2)));
    }

    #[test]
    fn test_expression_1_nested() {
        let test2 =
        Term::Sub(
            Box::new(Term::Num(3)),
            Box::new(Term::Add(
                Box::new(Term::Mul(
                    Box::new(Term::Num(4)),
                    Box::new(Term::Num(2))
                )),
                Box::new(Term::Num(3))
            ))
        );
        let expr2 = "3-(4 * 2+3)";
        assert_eq!(expression_1(expr2), Ok(("", test2)));

        let expr2 = "1+1+1+1+1+1+1+1+1+1";
        let test2 = Term::Add(
            Box::new(Term::Num(1)),
            Box::new(Term::Add(
                Box::new(Term::Num(1)),
                Box::new(Term::Add(
                    Box::new(Term::Num(1)),
                    Box::new(Term::Add(
                        Box::new(Term::Num(1)),
                        Box::new(Term::Add(
                            Box::new(Term::Num(1)),
                            Box::new(Term::Add(
                                Box::new(Term::Num(1)),
                                Box::new(Term::Add(
                                    Box::new(Term::Num(1)),
                                    Box::new(Term::Add(
                                        Box::new(Term::Num(1)),
                                        Box::new(Term::Add(
                                            Box::new(Term::Num(1)),
                                            Box::new(Term::Num(1))
                                        ))
                                    ))
                                ))
                            ))
                        ))
                    ))
                ))
            ))
        );
        assert_eq!(expression_1(expr2), Ok(("", test2)));
    }

    #[test]
    fn test_function_call() {
        let test1 = Term::App("sideeff".to_owned(), vec![]);
        let expr1 = "sideeff()";
        assert_eq!(function_call(expr1), Ok(("", test1)));

        let test1 = Term::App("fib".to_owned(), vec![Term::Num(10)]);
        let expr1 = "fib(10)";
        assert_eq!(function_call(expr1), Ok(("", test1)));

        let test1 = Term::App("ackermann".to_owned(), vec![Term::Num(4), Term::Num(5)]);
        let expr1 = "ackermann(4, 5)";
        assert_eq!(function_call(expr1), Ok(("", test1)));

        let test1 = Term::App("fn".to_owned(), vec![Term::Num(4), Term::Num(5), Term::Var("x".to_owned()), Term::Add(
            Box::new(Term::Num(1)), 
            Box::new(Term::Num(2))
        )]);
        let expr1 = "fn(4, 5  ,\tx,    1+   2)";
        assert_eq!(function_call(expr1), Ok(("", test1)));

        assert!(function_call("fn(x,").is_err())
    }

    #[test]
    fn test_expressions() {
        let test1 = Term::Mul(
            Box::new(Term::Var("x".to_owned())), 
            Box::new(Term::Num(3))
        );
        let expr1 = "x * 3";
        assert_eq!(expression(expr1), Ok(("", test1.clone())));

        let test2 = Term::Brn(
            Box::new(Term::Var("predicate".to_owned())), 
            Box::new(Term::Num(1)), 
            Box::new(test1.clone())
        );
        let expr2 = "if predicate then 1 else x*3";
        assert_eq!(expression(expr2), Ok(("", test2.clone())));
        
        let test3 = Term::Add(
            Box::new(Term::Num(1)), 
            Box::new(test2.clone())
        );
        let expr3 = "1 + (if predicate then 1 else x*3)";
        assert_eq!(expression(expr3), Ok(("", test3.clone())));

        let expr4 = "1 + (if predicate then1 else x*3)";
        assert_eq!(expression(expr4), Ok(("+ (if predicate then1 else x*3)", Term::Num(1))));

        let test5 = Term::Brn(
            Box::new(Term::Var("x".to_owned())),
            Box::new(Term::Num(1)),
            Box::new(Term::Mul(
                Box::new(Term::App(
                    "f".to_owned(), 
                    vec![Term::Sub(
                        Box::new(Term::Var("x".to_owned())),
                        Box::new(Term::Num(1))
                    )])),
                Box::new(Term::Var("x".to_owned()))
            ))
        );
        let expr5 = "if x then 1 else (f(x-1) * x)";
        assert_eq!(expression(expr5), Ok(("", test5)));
    }
}