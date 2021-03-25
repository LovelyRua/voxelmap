#[derive(Debug, PartialEq)]
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
}

impl From<()> for Error {
    fn from(_: ()) -> Self {
        Error::ParserError
    }
}
