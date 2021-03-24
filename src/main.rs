use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
use logos::Logos;
use pomelo::pomelo;
use std::fs;
use std::io::{Error, ErrorKind, Read};

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

        #[derive(Debug, PartialEq)]
        pub enum FunctionType{
            Sin,
            Cos,
            Tan,
            Asin,
            Acos,
            Atan,
            Sign,
            Abs,
            Sqrt,
            Exp,
            Log(f64),
        }

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
            #[token("ln", |_| FunctionType::Log(1.0))]
            #[token("log", |_| FunctionType::Log(std::f64::consts::LN_10))]
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

    input ::= limits metajuncture;
    input ::= limits assignments metajuncture;
    limit ::= expr Qualifier Var Qualifier expr;
    limits ::= limit LineEnd limit LineEnd limit LineEnd;
    limits ::= LineEnd limits;
    assignment ::= Assign Ident expr;
    assignments ::= assignment LineEnd;
    assignments ::= assignment LineEnd assignments;
    quality ::= expr Qualifier expr;
    juncture ::= quality;
    juncture ::= juncture Junction juncture;
    juncture ::= LBrace juncture RBrace;
    metajuncture ::= juncture;
    metajuncture ::= metajuncture LineEnd;
    metajuncture ::= metajuncture LineEnd metajuncture;

    expr ::= expr Sum expr;
    expr ::= expr Subtraction expr;
    expr ::= expr Product expr;
    expr ::= expr Quotient expr;
    expr ::= expr Power expr;
    expr ::= Function expr;
    expr ::= LParen expr RParen;
    expr ::= Var;
    expr ::= Float;
    expr ::= Ident;
}

fn main() -> Result<(), ()> {
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

    //let scale = matches.value_of("scale").map(|s| s.parse::<i32>().unwrap());

    let mut object_description = fs::File::open(matches.value_of("FILE").unwrap()).unwrap();

    let mut data = String::new();

    if object_description.read_to_string(&mut data).is_ok() {
        let lex = parser::Token::lexer(&data);

        let mut p = parser::Parser::new();

        let mut line_ends = false;

        for token in lex {
            println!("{:?}", token);
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

        let tree = p.end_of_input()?;
        println!("\n{:?}", tree);
        //println!("\nRead {} bytes, scale is {}", size, scale.unwrap_or(1));
    }

    Ok(())

    //println!("Scale was read and is <{}>", scale.unwrap_or(1));
}
