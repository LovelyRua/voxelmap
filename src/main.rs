use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
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

fn main() {
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

    let mut data = Vec::new();

    if let Ok(size) = object_description.read_to_end(&mut data) {
        println!("Read {} bytes, scale is {}", size, scale.unwrap_or(1));
    }

    //println!("Scale was read and is <{}>", scale.unwrap_or(1));
}
