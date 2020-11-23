use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};

macro_rules! scale_message {
    ($n:ident) => {
        Err(format!("<{}> is not a positive integer", $n))
    };
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
        .get_matches();

    let scale = matches.value_of("scale").map(|s| s.parse::<i32>().unwrap());

    println!("Scale was read and is <{}>", scale.unwrap_or(1));
}
