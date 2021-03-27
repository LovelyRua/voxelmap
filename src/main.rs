use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
use logos::Logos;
use nbt::encode::write_gzip_compound_tag;
use nbt::{CompoundTag, Tag};
use pomelo::pomelo;
use rgb::*;
use std::collections::HashMap;
use std::fs;
use std::io::{Error, ErrorKind, Read};

mod astree;
mod error;

macro_rules! scale_message {
    ($n:ident) => {
        Err(format!("<{}> is not a valid scale value", $n))
    };
}

fn io_error(err: Error, path: &str) -> String {
    match err.kind() {
        ErrorKind::NotFound => format!("{} not found", path),
        ErrorKind::PermissionDenied => format!("Permission to access {} denied", path),
        ErrorKind::AlreadyExists => format!("{} already exists", path),
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
    expr ::= Subtraction expr(E) { Expression::function(FunctionType::Neg,E) }
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
                    if n.parse::<f64>().is_err() {
                        scale_message!(n)
                    } else {
                        Ok(())
                    }
                }),
        )
        .arg(
            Arg::with_name("block")
                .short("b")
                .long("block")
                .help("The minecraft block to be used in the Litematica output, defaults to minecraft:stone")
                .takes_value(true)
                .multiple(false),
        )
        .arg(
            Arg::with_name("offset")
                .short("o")
                .long("offset")
                .help("Offset the computation by half a block")
                .takes_value(false)
                .multiple(false),
        )
        .arg(
            Arg::with_name("debug")
                .short("d")
                .long("debug")
                .help("Show parsing steps")
                .takes_value(false)
                .multiple(false),
        )
        .arg(
            Arg::with_name("test")
                .short("t")
                .long("test")
                .help("Parses the input file, does not output")
                .takes_value(false)
                .multiple(false),
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
        .arg(
            Arg::with_name("OUTPUT_DIR")
                .help("The folder where the output images will be stored")
                .required(true)
                .index(2)
                .validator(move |path: String| -> Result<(), String> {
                    match fs::create_dir(&path) {
                        Ok(_) => Ok(()),
                        Err(error) => Err(io_error(error, &path)),
                    }
                })
                .conflicts_with("test"),
        )
        .get_matches();

    let scale = matches.value_of("scale").map(|s| s.parse::<f64>().unwrap());

    let mut object_description = fs::File::open(matches.value_of("FILE").unwrap()).unwrap();

    let offset = matches.is_present("offset");
    let debug = matches.is_present("debug");
    let test = matches.is_present("test");

    let output_folder = if test {
        ""
    } else {
        matches.value_of("OUTPUT_DIR").unwrap()
    };

    let mut data = String::new();

    let read_count = object_description.read_to_string(&mut data)?;

    if debug {
        println!(
            "\nRead {} bytes, scale is {}",
            read_count,
            scale.unwrap_or(1.0_f64)
        );
    }

    let lex = parser::Token::lexer(&data);

    let mut p = parser::Parser::new();

    let mut line_ends = false;

    for token in lex {
        if debug {
            println!("{:?}", token);
        }
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
    let mut min_x: Option<i64> = None;
    let mut max_x: Option<i64> = None;
    let mut min_y: Option<i64> = None;
    let mut max_y: Option<i64> = None;
    let mut min_z: Option<i64> = None;
    let mut max_z: Option<i64> = None;

    let mut vars = HashMap::new();
    vars.insert('s', scale.unwrap_or(1_f64));

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
        let min =
            (limit.min.eval(&ident_arg, &var_arg)?.floor() as i64) - if offset { 1 } else { 0 };
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

    if debug {
        println!("\n{:?}", tree);
    }

    if test {
        return Ok(());
    }

    let min_x: i64 = min_x.unwrap();
    let max_x: i64 = max_x.unwrap();
    let min_y: i64 = min_y.unwrap();
    let max_y: i64 = max_y.unwrap();
    let min_z: i64 = min_z.unwrap();
    let max_z: i64 = max_z.unwrap();

    let width: i64 = 1 + max_x - min_x;
    let height: i64 = 1 + max_y - min_y;
    let depth: i64 = 1 + max_z - min_z;

    let pix_width: usize = 6 * (width as usize) + 1;
    let pix_height: usize = 6 * (height as usize) + 1;
    let pix_size: usize = pix_width * pix_height;

    let lite_size: usize = (((width as usize) * (height as usize) * (depth as usize)) / 32) + 1;

    let mut lite_block_data: Vec<i64> = Vec::with_capacity(lite_size);
    let mut working: i64 = 0;
    let mut counter: u8 = 0;
    let mut total_blocks: i32 = 0;
    let mut total_volume: i32 = 0;

    for z in min_z..=max_z {
        let name = format! {"{}/layer{:04}.png",output_folder,1 + z - min_z};

        let filled_in = RGBA8::new(0, 0, 0, 255); // Black
        let empty = RGBA8::new(255, 255, 255, 255); // White
        let multiple_filled_in = RGBA8::new(0, 0, 255, 255); // Blue
        let multiple_empty = RGBA8::new(255, 255, 0, 255); // Yellow
        let grid = RGBA8::new(255, 128, 128, 255); // Coral (Grid)

        let mut pixels: Vec<RGBA8> = Vec::with_capacity(pix_size);

        pixels.resize(pix_size, grid);

        for y in min_y..=max_y {
            let square_start_y = 6 * (y - min_y) as usize;
            let grid_y = y.abs() % 10 == 0;
            for x in min_x..=max_x {
                let x_f: f64 = (x as f64) + if offset { 0.5_f64 } else { 0_f64 };
                let y_f: f64 = (y as f64) + if offset { 0.5_f64 } else { 0_f64 };
                let z_f: f64 = (z as f64) + if offset { 0.5_f64 } else { 0_f64 };
                let rho: f64 = (x_f.powi(2) + y_f.powi(2)).sqrt();
                let r: f64 = (z_f.powi(2) + rho.powi(2)).sqrt();
                let tht: f64 = (z_f / rho).atan();

                let phi: f64;

                if rho < 2_f64 * f64::EPSILON {
                    phi = f64::NAN;
                } else if y_f >= 0_f64 {
                    phi = (x_f / rho).acos();
                } else {
                    phi = -((x_f / rho).acos());
                }

                vars.insert('x', x_f);
                vars.insert('y', y_f);
                vars.insert('z', z_f);
                vars.insert('ρ', rho);
                vars.insert('φ', phi);
                vars.insert('r', r);
                vars.insert('θ', tht);

                let var_arg = Some(&vars);

                let square_start_x = 6 * (x - min_x) as usize;

                let grid = if grid_y { true } else { x.abs() % 10 == 0 };
                let is_filled = tree.eval(&ident_arg, &var_arg)?;

                let color = match (is_filled, grid) {
                    (false, false) => empty,
                    (false, true) => multiple_empty,
                    (true, false) => filled_in,
                    (true, true) => multiple_filled_in,
                };

                for pix_x in 0..5 {
                    for pix_y in 0..5 {
                        pixels[(1 + square_start_y + pix_y) * pix_width
                            + 1
                            + square_start_x
                            + pix_x] = color;
                    }
                }

                let new_block: i64 = if is_filled { 1 } else { 0 } << (counter * 2);
                working |= new_block;
                counter += 1;
                if counter >= 32 {
                    lite_block_data.push(working);
                    working = 0;
                    counter = 0;
                }
                total_volume += 1;
                total_blocks += if is_filled { 1 } else { 0 };
            }
        }
        lodepng::encode32_file(name, &pixels, pix_width, pix_height)?;
    }
    if counter != 0 {
        lite_block_data.push(working);
    }

    // Litematica schematic
    let mut enclosing_size = CompoundTag::new();
    enclosing_size.insert_i32("x", width as i32);
    enclosing_size.insert_i32("y", depth as i32);
    enclosing_size.insert_i32("z", height as i32);
    let region_size = enclosing_size.clone();
    let mut metadata = CompoundTag::new();
    metadata.insert("EnclosingSize", enclosing_size);
    metadata.insert_str("Author", crate_name!());
    metadata.insert_str("Description", crate_version!());
    metadata.insert_str("Name", output_folder);
    metadata.insert_i32("RegionCount", 1);
    metadata.insert_i32("TotalBlocks", total_blocks);
    metadata.insert_i32("TotalVolume", total_volume);
    let mut lite = CompoundTag::new();
    lite.insert_i32("Version", 4);
    lite.insert_i32("MinecraftDataVersion", 1343);
    lite.insert("Metadata", metadata);
    let mut region = CompoundTag::new();
    region.insert("Size", region_size);
    let mut region_position = CompoundTag::new();
    region_position.insert_i32("x", 0);
    region_position.insert_i32("y", 0);
    region_position.insert_i32("z", 0);
    region.insert("Position", region_position);
    let mut block_state_palette: Vec<Tag> = Vec::with_capacity(2);
    let mut air = CompoundTag::new();
    air.insert_str("Name", "minecraft:air");
    block_state_palette.push(Tag::from(air));
    let mut block = CompoundTag::new();
    block.insert_str("Name",matches.value_of("block").unwrap_or("minecraft:stone"));
    block_state_palette.push(Tag::from(block));
    region.insert("BlockStatePalette", block_state_palette);
    region.insert_i64_vec("BlockStates", lite_block_data);
    let mut regions = CompoundTag::new();
    regions.insert(output_folder, region);
    lite.insert("Regions", regions);

    let mut lite_file = fs::File::create(format!("{}/{}.litematic", output_folder, output_folder))?;

    write_gzip_compound_tag(&mut lite_file, &lite)?;

    Ok(())
}
