use lodepng::Error as LPNGError;

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
    IllegalVarInBoundary,
    IllegarBoundedVar,
    UnboundedVar,
    LodePNG(LPNGError),
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
