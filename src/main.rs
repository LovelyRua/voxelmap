use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
use logos::Logos;
use pomelo::pomelo;
use std::collections::HashMap;
use std::fs;
use std::io::{Error, ErrorKind, Read};

mod astree;
mod error;

macro_rules! scale_message {
    ($n:ident) => {
        Err(format!("<{}> is not a positive integer", $n))
    };
}

fn io_error(err: Error, path: &str) -> String {
    match err.kind() {
        ErrorKind::NotFound => format!("{} not found", path),
        ErrorKind::PermissionDenied => format!("Permission to read {} denied", path),
        _ => format!("Unexpected error accessing {}", path),
    }
}

pomelo! {
    %include {
        use logos::{Lexer, Logos};
        use crate::astree::{Condition, Expression, FunctionType, Junction};
        use std::collections::HashMap;

        #[derive(Debug)]
        pub struct Boundary{
            pub var: char,
            pub min: Expression,
            pub max: Expression,
        }

        impl Boundary{
            pub fn new(
                l: Expression,
                lcond: char,
                var: char,
                rcond: char,
                r: Expression,
            ) -> Result<Self,()> {
                if var != 'x' && var != 'y' && var != 'z' {
                    return Err(());
                }
                if lcond == '<' || lcond == '≤' {
                    if rcond == '<' || rcond == '≤'{
                        let min = l;
                        let max = r;
                        return Ok(Boundary{var,min,max});
                    }
                    return Err(());
                }else if lcond == '>' || lcond == '≥'{
                    if rcond == '>' || rcond == '≥'{
                        let min = r;
                        let max = l;
                        return Ok(Boundary{var,min,max});
                    }
                    return Err(());
                }
                Err(())
            }
        }

        type Boundaries = [Boundary; 3];

        type Return = (Option<HashMap<String,Expression>>, Boundaries, Junction);

        fn read_var(lex: &mut Lexer<Token>) -> Option<char> {
            lex.slice().chars().next()
        }
    }

    %token #[derive(Logos, Debug, PartialEq)]
        pub enum Token {};

    %type #[regex("s|x|y|z|r|ρ|θ|φ", read_var)] Var char;

    %type #[regex(r"@[^@ \t\f\n]+", |lex| String::from(lex.slice()))] Ident String;

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
            #[regex(r"[+-]?(?:\d*\.)?\d+", |lex| lex.slice().parse())]
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
            Function FunctionType;

    %type   #[token("(")] LParen;
    %type   #[token(")")] RParen;

    %type   #[token("{")] LBrace;
    %type   #[token("}")] RBrace;

    %type   #[regex(r"\n+")] LineEnd;

    %type   #[regex("\\\\n", logos::skip)]
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

    %type input Return;
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
    expr ::= Var(V) { Expression::var(V) }
    expr ::= Float(F) { Expression::float(F) }
    expr ::= Ident(S) { Expression::ident(S) }
}

fn main() -> Result<(), error::Error> {
    use error::Error;

    let matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .arg(
            Arg::with_name("scale")
                .short("s")
                .long("scale")
                .help("The scale parameter for the object")
                .takes_value(true)
                .multiple(false)
                .value_name("N")
                .validator(|n: String| -> Result<(), String> {
                    match n.parse::<i32>() {
                        Ok(x) => {
                            if x > 0 {
                                Ok(())
                            } else {
                                scale_message!(n)
                            }
                        }
                        Err(_) => scale_message!(n),
                    }
                }),
        )
        .arg(
            Arg::with_name("FILE")
                .help("The file describing the shape to map")
                .required(true)
                .index(1)
                .validator(move |path: String| -> Result<(), String> {
                    match fs::File::open(&path) {
                        Ok(_) => Ok(()),
                        Err(error) => Err(io_error(error, &path)),
                    }
                }),
        )
        .get_matches();

    let scale = matches.value_of("scale").map(|s| s.parse::<i32>().unwrap());

    let mut object_description = fs::File::open(matches.value_of("FILE").unwrap()).unwrap();

    let mut data = String::new();

    if object_description.read_to_string(&mut data).is_ok() {
        let lex = parser::Token::lexer(&data);

        let mut p = parser::Parser::new();

        let mut line_ends = false;

        for token in lex {
            //println!("{:?}", token);
            if token == parser::Token::LineEnd {
                if line_ends {
                    continue;
                } else {
                    line_ends = true;
                }
            } else {
                line_ends = false;
            }
            p.parse(token)?;
        }

        let (assigns, limits, tree) = p.end_of_input()?;

        let idents = assigns.unwrap_or_default();
        let ident_arg = Some(&idents);
        //println!("\n{:?}", tree);
        //println!("\nRead {} bytes, scale is {}", size, scale.unwrap_or(1));
        let mut min_x: Option<i64> = None;
        let mut max_x: Option<i64> = None;
        let mut min_y: Option<i64> = None;
        let mut max_y: Option<i64> = None;
        let mut min_z: Option<i64> = None;
        let mut max_z: Option<i64> = None;

        let mut vars = HashMap::new();
        vars.insert('s', scale.unwrap_or(1) as f64);

        for limit in &limits {
            for dep in limit.min.var_dependencies(&ident_arg)? {
                if dep != 's' {
                    eprintln!("Boundaries can only refer to s, not {}", dep);
                    return Err(Error::IllegalVarInBoundary);
                }
            }
            for dep in limit.max.var_dependencies(&ident_arg)? {
                if dep != 's' {
                    eprintln!("Boundaries can only refer to s, not {}", dep);
                    return Err(Error::IllegalVarInBoundary);
                }
            }
            let var_arg = Some(&vars);
            let min = limit.min.eval(&ident_arg, &var_arg)?.floor() as i64;
            let max = limit.max.eval(&ident_arg, &var_arg)?.ceil() as i64;
            match limit.var {
                'x' => {
                    min_x = Some(min);
                    max_x = Some(max);
                }
                'y' => {
                    min_y = Some(min);
                    max_y = Some(max);
                }
                'z' => {
                    min_z = Some(min);
                    max_z = Some(max);
                }
                c => {
                    eprintln!("Bounded variables are x,y,z only, not {}", c);
                    return Err(Error::IllegarBoundedVar);
                }
            }
        }

        let mut unbounded = false;
        if min_x.is_none() || max_x.is_none() {
            unbounded = true;
            eprintln!("x is unbounded");
        }
        if min_y.is_none() || max_y.is_none() {
            unbounded = true;
            eprintln!("y is unbounded");
        }
        if min_z.is_none() || max_z.is_none() {
            unbounded = true;
            eprintln!("z is unbounded");
        }
        if unbounded {
            return Err(Error::UnboundedVar);
        }
    }

    Ok(())

    //println!("Scale was read and is <{}>", scale.unwrap_or(1));
}
