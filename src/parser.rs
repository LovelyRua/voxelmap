use crate::astree;
use crate::error;
use crate::error::Error;
use logos::Logos;
use pomelo::pomelo;
use std::fs;
use std::path::Path;

pomelo! {
    %include {
        use logos::{Lexer, Logos};
        use crate::astree::{Boundary, Boundaries, Condition, Expression, FunctionType, Junction, Structure};
        use std::collections::HashMap;

        fn read_var(lex: &mut Lexer<Token>) -> Option<char> {
            lex.slice().chars().next()
        }
    }

    %token #[derive(Logos, Debug, PartialEq)]
        pub enum Token {};

    %type #[regex("s|x|y|z|r|ρ|θ|φ", read_var)] Var char;

    %type #[regex(r"@[\p{Letter}\p{Number}\p{Greek}_]+", |lex| String::from(lex.slice()))] Ident String;

    %type   #[token("set")]
            #[token("define")]
            #[token("assign")]
            Assign;

    %type   #[token("e", |_| std::f64::consts::E)]
            #[token("pi", |_| std::f64::consts::PI)]
            #[regex("½pi", |_| std::f64::consts::FRAC_PI_2)]
            #[regex("⅓pi", |_| std::f64::consts::FRAC_PI_3)]
            #[regex("¼pi", |_| std::f64::consts::FRAC_PI_4)]
            #[regex("⅙pi", |_| std::f64::consts::FRAC_PI_6)]
            #[regex("⅛pi", |_| std::f64::consts::FRAC_PI_8)]
            #[regex("(pi/2)", |_| std::f64::consts::FRAC_PI_2)]
            #[regex("(pi/3)", |_| std::f64::consts::FRAC_PI_3)]
            #[regex("(pi/4)", |_| std::f64::consts::FRAC_PI_4)]
            #[regex("(pi/6)", |_| std::f64::consts::FRAC_PI_6)]
            #[regex("(pi/8)", |_| std::f64::consts::FRAC_PI_8)]
            #[token("2pi", |_| std::f64::consts::TAU)]
            #[token("π", |_| std::f64::consts::PI)]
            #[regex("½π", |_| std::f64::consts::FRAC_PI_2)]
            #[regex("⅓π", |_| std::f64::consts::FRAC_PI_3)]
            #[regex("¼π", |_| std::f64::consts::FRAC_PI_4)]
            #[regex("⅙π", |_| std::f64::consts::FRAC_PI_6)]
            #[regex("⅛π", |_| std::f64::consts::FRAC_PI_8)]
            #[regex("(π/2)", |_| std::f64::consts::FRAC_PI_2)]
            #[regex("(π/3)", |_| std::f64::consts::FRAC_PI_3)]
            #[regex("(π/4)", |_| std::f64::consts::FRAC_PI_4)]
            #[regex("(π/6)", |_| std::f64::consts::FRAC_PI_6)]
            #[regex("(π/8)", |_| std::f64::consts::FRAC_PI_8)]
            #[token("2π", |_| std::f64::consts::TAU)]
            #[token("tau", |_| std::f64::consts::TAU)]
            #[token("τ", |_| std::f64::consts::TAU)]
            #[regex("√2\\s", |_| std::f64::consts::SQRT_2)]
            #[regex("√(2)", |_| std::f64::consts::SQRT_2)]
            #[regex(r"(?:\d*\.)?\d+", |lex| lex.slice().parse())]
            Float f64;

    %type #[token("+")] Sum;
    %type #[token("-")] Subtraction;
    %type #[token("/")] Quotient;
    %type #[token("*")] Product;
    %type #[token("^")] Power;

    %type   #[regex("=|<|>|≤|≥", read_var)]
            #[regex("<=", |_| '≤')]
            #[regex(">=", |_| '≥')]
            Qualifier char;

    %type   #[regex("⋀|⋁|⊻|⊼|⊽", read_var)]
            #[regex("⋂|∧|and|AND|&&", |_| '⋀')]
            #[regex("∪|∨|v|or|OR|\\|\\|", |_| '⋁')]
            #[regex("⩒|⩛|⊕|⩡|xor|XOR", |_| '⊻')]
            #[regex("⩃|nand|NAND", |_| '⊼')]
            #[regex("⩂|nor|NOR", |_| '⊽')]
            Junction char;

    %type   #[token("sin", |_| FunctionType::Sin)]
            #[token("cos", |_| FunctionType::Cos)]
            #[token("tan", |_| FunctionType::Tan)]
            #[token("sec", |_| FunctionType::Sec)]
            #[token("csc", |_| FunctionType::Csc)]
            #[token("cot", |_| FunctionType::Cot)]
            #[token("asin", |_| FunctionType::Asin)]
            #[token("acos", |_| FunctionType::Acos)]
            #[token("atan", |_| FunctionType::Atan)]
            #[token("sign", |_| FunctionType::Sign)]
            #[token("abs", |_| FunctionType::Abs)]
            #[token("sqrt", |_| FunctionType::Sqrt)]
            #[token("√", |_| FunctionType::Sqrt)]
            #[token("exp", |_| FunctionType::Exp)]
            #[token("ln", |_| FunctionType::Ln)]
            #[token("log", |_| FunctionType::Log)]
            #[token("neg", |_| FunctionType::Neg)]
            Function FunctionType;

    %type   #[token("(")] LParen;
    %type   #[token(")")] RParen;

    %type   #[token("{")] LBrace;
    %type   #[token("}")] RBrace;

    %type   #[regex(r"\n+")] LineEnd;

    %type   #[regex(r"\\\n", logos::skip)]
            #[regex("#.*\\n", logos::skip)]
            #[regex("//.*\\n", logos::skip)]
            #[regex(r"[ \t\f]+", logos::skip)]
            #[error]
            Error;

    %left Junction;
    %nonassoc Qualifier Assign;
    %left Sum Subtraction;
    %left Product Quotient;
    %right Power;
    %right Function;
    %left LineEnd;

    %type input Structure;
    input ::= boundaries(L) metajuncture(J) { (None,L,J) }
    input ::= LineEnd boundaries(L) metajuncture(J) { (None,L,J) }
    input ::= assignments(A) boundaries(L) metajuncture(J) { (Some(A),L,J) }
    input ::= LineEnd assignments(A) boundaries(L) metajuncture(J) { (Some(A),L,J) }

    %type boundary Boundary;
    boundary ::= expr(L) Qualifier(F) Var(V) Qualifier(S) expr(R) { Boundary::new(L,F,V,S,R)? }

    %type boundaries Boundaries;
    boundaries ::= boundary(A) LineEnd boundary(B) LineEnd boundary(C) LineEnd { [A,B,C] }

    %type assignment (String,Expression);
    assignment ::= Assign Ident(S) expr(E) { (S,E) }

    %type assignments HashMap<String,Expression>;
    assignments ::= assignment(A) LineEnd {
                                                let (k,v) = A;
                                                let mut m = HashMap::new();
                                                let ident_arg = Some(&m);
                                                if v.ident_dependencies(&ident_arg).is_ok() {
                                                    m.insert(k,v);
                                                }else{
                                                    eprintln!("Undefined reference in {}",k);
                                                }
                                                m
                                            }
    assignments ::= assignments(mut M) assignment(A) LineEnd {
                                                            let (k,v) = A;
                                                            let ident_arg = Some(&M);
                                                            if v.ident_dependencies(&ident_arg).is_ok() {
                                                                M.insert(k,v);
                                                            }else{
                                                                eprintln!("Undefined reference in {}",k);
                                                            }
                                                            M
                                                        }
    %type quality Condition;
    quality ::= expr(L) Qualifier(Q) expr (R) { Condition::new(Q,L,R) }

    %type juncture Junction;
    juncture ::= quality(Q) { Junction::singleton(Q) }
    juncture ::= juncture(L) Junction(J) juncture(R) { Junction::meta(J,L,R) }
    juncture ::= LBrace juncture(J) RBrace { J }

    %type metajuncture Junction;
    metajuncture ::= juncture(J) { J }
    metajuncture ::= metajuncture(M) LineEnd { M }
    metajuncture ::= metajuncture(L) LineEnd metajuncture(R) { Junction::meta('⋀',L,R) }

    %type expr Expression;
    expr ::= expr(L) Sum expr(R) { Expression::operation('+',L,R) }
    expr ::= expr(L) Subtraction expr(R) { Expression::operation('-',L,R) }
    expr ::= expr(L) Product expr(R) { Expression::operation('*',L,R) }
    expr ::= expr(L) Quotient expr(R) { Expression::operation('/',L,R) }
    expr ::= expr(L) Power expr(R) { Expression::operation('^',L,R) }
    expr ::= Function(F) expr(A) { Expression::function(F,A) }
    expr ::= LParen expr(E) RParen { E }
    expr ::= Subtraction expr(E) { Expression::function(FunctionType::Neg,E) }
    expr ::= Var(V) { Expression::var(V) }
    expr ::= Float(F) { Expression::float(F) }
    expr ::= Ident(S) { Expression::ident(S) }
}

pub fn parse<P: AsRef<Path> + std::fmt::Display>(
    source: &str,
    file: P,
    output_dir: Option<P>,
    debug: bool,
) -> Result<astree::Structure, error::Error> {
    let lex = parser::Token::lexer(source);

    let mut p = parser::Parser::new();

    let mut line_ends = false;

    let mut reason = "parsing";

    for (token, span) in lex.spanned() {
        if debug {
            println!("{:?}", token);
        }
        if token == parser::Token::Error {
            reason = "tokenizing";
        } else if token == parser::Token::LineEnd {
            if line_ends {
                continue;
            } else {
                line_ends = true;
            }
        } else {
            line_ends = false;
        }
        if p.parse(token).is_err() {
            let mut line = 1;
            let mut col = 1;
            for (index, _) in source.match_indices('\n') {
                if index > span.start {
                    break;
                }
                line += 1;
                col = span.start - index;
            }
            let token_val = if line_ends {
                r"\n"
            } else {
                source.get(span).unwrap()
            };
            eprintln!(
                "{}:{}:{}: Error {} \"{}\"",
                file, line, col, reason, token_val
            );
            if let Some(the_dir) = output_dir {
                fs::remove_dir(the_dir)?;
            }
            return Err(Error::ParserError);
        }
    }

    match p.end_of_input() {
        Ok(result) => Ok(result),
        Err(_) => {
            eprintln!("{}: Unexpected end of file", file);
            if let Some(the_dir) = output_dir {
                fs::remove_dir(the_dir)?;
            }
            Err(Error::ParserError)
        }
    }
}
