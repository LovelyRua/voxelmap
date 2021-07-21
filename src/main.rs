use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg, SubCommand};
use nbt::encode::write_gzip_compound_tag;
use nbt::{CompoundTag, Tag};
use rgb::*;
use std::collections::HashMap;
use std::fs;
use std::io::{Error, ErrorKind, Read, Write};

mod astree;
mod error;
mod ngon;
mod parser;

macro_rules! scale_message {
    ($n:ident) => {
        Err(format!("<{}> is not a valid scale value", $n))
    };
}

macro_rules! sides_message {
    ($n:ident) => {
        Err(format!("<{}> is not a valid number of sides", $n))
    };
}

macro_rules! csv_line {
    ($f:ident, $n:expr, $t:expr, $c:expr, $s:expr, $b:expr) => {
        writeln!($f, "{},{},{},{},{}", $n, $t, $c, $s, $b)?;
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
                .value_name("S")
                .validator(|n: String| -> Result<(), String> {
                    if let Ok(scale) = n.parse::<f64>() {
                        if scale >= 0_f64 {
                            return Ok(());
                        }
                    }
                    scale_message!(n)
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
            Arg::with_name("xoffset")
                .short("x")
                .long("xoffset")
                .help("Offset the x computation by half a block")
                .takes_value(false)
                .multiple(false),
        )
        .arg(
            Arg::with_name("yoffset")
                .short("y")
                .long("yoffset")
                .help("Offset the y computation by half a block")
                .takes_value(false)
                .multiple(false),
        )
        .arg(
            Arg::with_name("zoffset")
                .short("z")
                .long("zoffset")
                .help("Offset the z computation by half a block")
                .takes_value(false)
                .multiple(false),
        )
        .arg(
            Arg::with_name("debug")
                .short("d")
                .long("debug")
                .help("Show intermediate steps")
                .takes_value(false)
                .multiple(false),
        )
        .arg(
            Arg::with_name("graph")
                .short("g")
                .long("graph")
                .help("Output graph of internal state")
                .takes_value(false)
                .multiple(false),
        )
        .arg(
            Arg::with_name("test")
                .short("t")
                .long("test")
                .help("Parses the input, does not output")
                .takes_value(false)
                .multiple(false),
        )
        .subcommand(SubCommand::with_name("ngon")
            .about("Make an ngon")
            .arg(
                Arg::with_name("N")
                .help("The number of sides of the ngon")
                .required(true)
                .index(1)
                .validator(|n: String| -> Result<(), String> {
                    if let Ok(sides) = n.parse::<u8>() {
                        if (3..=100).contains(&sides) {
                            return Ok(());
                        }
                    }
                    sides_message!(n)
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
        )
        .subcommand(SubCommand::with_name("solid")
            .about("Read mathematical description of solid")
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
                })
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
        )
        .get_matches();

    let scale = matches.value_of("scale").map(|s| s.parse::<f64>().unwrap());

    let debug = matches.is_present("debug");

    let offset = matches.is_present("offset");
    let toggle_x = matches.is_present("xoffset");
    let toggle_y = matches.is_present("yoffset");
    let toggle_z = matches.is_present("zoffset");

    let offset_x = offset ^ toggle_x;
    let offset_y = offset ^ toggle_y;
    let offset_z = offset ^ toggle_z;

    if debug {
        println!(
            "Offset toggles:\n\tGlobal: {}\n\tx toggle: {}\n\ty toggle: {}\n\tz toggle: {}",
            offset, toggle_x, toggle_y, toggle_z
        );
        println!(
            "Offsets:\n\tx offset: {}\n\ty offset: {}\n\tz offset: {}",
            offset_x, offset_y, offset_z
        );
    }

    let graph = matches.is_present("graph");
    let test = matches.is_present("test");

    let mut output_folder = ".";

    let structure: astree::Structure;

    if let Some(submatches) = matches.subcommand_matches("solid") {
        let mut data = String::new();

        let mut object_description = fs::File::open(submatches.value_of("FILE").unwrap()).unwrap();
        output_folder = submatches.value_of("OUTPUT_DIR").unwrap_or(output_folder);

        let read_count = object_description.read_to_string(&mut data)?;

        if debug {
            println!(
                "\nRead {} bytes, scale is {}",
                read_count,
                scale.unwrap_or(1.0_f64)
            );
        }

        structure = parser::parse(
            &data,
            submatches.value_of("FILE").unwrap(),
            submatches.value_of("OUTPUT_DIR"),
            debug,
        )?;
    } else if let Some(submatches) = matches.subcommand_matches("ngon") {
        output_folder = submatches.value_of("OUTPUT_DIR").unwrap_or(output_folder);
        structure = ngon::generate(
            submatches
                .value_of("N")
                .map(|n| n.parse::<u8>().unwrap())
                .unwrap(),
        )?;
    } else {
        println!("{}", matches.usage());
        return Err(Error::MissingSubCommand);
    }

    let (assigns, limits, tree) = structure;

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

    // Print graph
    if graph {
        let mut gv_file = fs::File::create(format! {"{}/state.gv",output_folder})?;
        writeln!(
            gv_file,
            "/* Graph file generated from {} v{}, by {} */\n\n",
            crate_name!(),
            crate_version!(),
            crate_authors!()
        )?;
        writeln!(gv_file, "digraph State {{")?;
        // Print main condition
        let tree_nodes = tree.graph(&mut gv_file, 1)?;
        // Print ident definitions
        let mut max_node = tree_nodes;
        for (label, expression) in &idents {
            writeln!(
                gv_file,
                "\tnode{} [label=\"{}\",shape=cds];",
                max_node + 1,
                label
            )?;
            writeln!(gv_file, "\tnode{} -> node{};", max_node + 1, max_node + 2)?;
            max_node = expression.graph(&mut gv_file, max_node + 2)?;
        }
        writeln!(gv_file, "}}")?;
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

    let filled_in = RGBA8::new(0, 0, 0, 255); // Black
    let empty = RGBA8::new(255, 255, 255, 255); // White
    let multiple_filled_in = RGBA8::new(0, 0, 255, 255); // Blue
    let multiple_empty = RGBA8::new(255, 255, 0, 255); // Yellow
    let grid = RGBA8::new(255, 128, 128, 255); // Coral (Grid)
    let transparent = RGBA8::new(255, 255, 255, 0); // Transparent White

    let top_size: usize = (width as usize) * (height as usize);
    let mut top_view: Vec<RGBA8> = Vec::with_capacity(top_size);
    top_view.resize(top_size, transparent);

    for z in min_z..=max_z {
        let name = format! {"{}/layer{:04}.png",output_folder,1 + z - min_z};

        let mut pixels: Vec<RGBA8> = Vec::with_capacity(pix_size);

        pixels.resize(pix_size, grid);

        let current_depth = z - min_z;
        let ratio = (current_depth * 192 / depth) as u8;
        let top_color = RGBA8::new(ratio, ratio, ratio, 255);

        for y in min_y..=max_y {
            let square_start_y = 6 * (y - min_y) as usize;
            let grid_y = y.abs() % 10 == 0;
            for x in min_x..=max_x {
                let x_f: f64 = (x as f64) + if offset_x { 0.5_f64 } else { 0_f64 };
                let y_f: f64 = (y as f64) + if offset_y { 0.5_f64 } else { 0_f64 };
                let z_f: f64 = (z as f64) + if offset_z { 0.5_f64 } else { 0_f64 };
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

                if is_filled {
                    let offset: usize = ((y - min_y) * width + (x - min_x)) as usize;

                    top_view[offset] = top_color;
                }

                for pix_x in 0..5 {
                    for pix_y in 0..5 {
                        let offset =
                            (1 + square_start_y + pix_y) * pix_width + 1 + square_start_x + pix_x;

                        pixels[offset] = color;
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

    // Top View image
    lodepng::encode32_file(
        format! {"{}/top_view.png",output_folder},
        &top_view,
        width as usize,
        height as usize,
    )?;

    // Manifest
    let mut manif_file = fs::File::create(format! {"{}/manifest.csv",output_folder})?;
    writeln!(manif_file, "Unit,Total,Combined,Stacks,Blocks")?;
    writeln!(manif_file, ",,,,")?;
    let stacks = if total_blocks % 64 == 0 {
        total_blocks / 64
    } else {
        1 + total_blocks / 64
    };
    let chests = if stacks % 27 == 0 {
        stacks / 27
    } else {
        1 + stacks / 27
    };
    let doubles = if chests % 2 == 0 {
        chests / 2
    } else {
        1 + chests / 2
    };
    let combined_doubles = total_blocks / (64 * 54);
    let combined_chests = (total_blocks % (64 * 54)) / (27 * 64);
    let combined_stacks = (total_blocks % (64 * 27)) / 64;
    let combined_blocks = total_blocks % 64;
    csv_line!(
        manif_file,
        "Double Chests",
        doubles,
        combined_doubles,
        54,
        3456
    );
    csv_line!(manif_file, "Chests", chests, combined_chests, 27, 1728);
    csv_line!(manif_file, "Stacks", stacks, combined_stacks, 1, 64);
    csv_line!(manif_file, "Blocks", total_blocks, combined_blocks, '-', 1);
    writeln!(manif_file, ",,,,")?;
    let gold_picks = if total_blocks % 32 == 0 {
        total_blocks / 32
    } else {
        1 + total_blocks / 32
    };
    let wood_picks = if total_blocks % 59 == 0 {
        total_blocks / 59
    } else {
        1 + total_blocks / 59
    };
    let stone_picks = if total_blocks % 131 == 0 {
        total_blocks / 131
    } else {
        1 + total_blocks / 131
    };
    let iron_picks = if total_blocks % 250 == 0 {
        total_blocks / 250
    } else {
        1 + total_blocks / 250
    };
    let diamond_picks = if total_blocks % 1561 == 0 {
        total_blocks / 1561
    } else {
        1 + total_blocks / 1561
    };
    let netherite_picks = if total_blocks % 2031 == 0 {
        total_blocks / 2031
    } else {
        1 + total_blocks / 2031
    };
    csv_line!(manif_file, "Gold Picks", gold_picks, '-', '-', 32);
    csv_line!(manif_file, "Wood Picks", wood_picks, '-', '-', 59);
    csv_line!(manif_file, "Stone Picks", stone_picks, '-', 2, 131);
    csv_line!(manif_file, "Iron Picks", iron_picks, '-', 3, 250);
    csv_line!(manif_file, "Diamond Picks", diamond_picks, '-', 24, 1561);
    csv_line!(
        manif_file,
        "Netherite Picks",
        netherite_picks,
        '-',
        31,
        2031
    );

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
    block.insert_str(
        "Name",
        matches.value_of("block").unwrap_or("minecraft:stone"),
    );
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
