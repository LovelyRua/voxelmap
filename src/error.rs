use lodepng::Error as LPNGError;
use std::io::Error as IOError;

#[derive(Debug)]
pub enum Error {
    ParserError,
    UnrecognisedBinaryOperator,
    UnrecognisedCondition,
    UnrecognisedJunction,
    MissingVarValue,
    MissingIdentAssignment,
    MissingVarMap,
    MissingIdentMap,
    MissingSubCommand,
    IllegalVarInBoundary,
    IllegarBoundedVar,
    UnboundedVar,
    LodePNG(LPNGError),
    Io(IOError),
}

impl From<()> for Error {
    fn from(_: ()) -> Self {
        Error::ParserError
    }
}

impl From<LPNGError> for Error {
    fn from(lode: LPNGError) -> Self {
        Error::LodePNG(lode)
    }
}

impl From<IOError> for Error {
    fn from(io: IOError) -> Self {
        Error::Io(io)
    }
}
