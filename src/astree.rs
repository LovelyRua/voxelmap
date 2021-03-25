use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnrecognisedBinaryOperator,
    UnrecognisedCondition,
    UnrecognisedJunction,
    MissingVarValue,
    MissingIdentAssignment,
    MissingVarMap,
    MissingIdentMap,
}

#[derive(Debug, PartialEq)]
pub enum FunctionType {
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
    Ln,
    Log,
}

#[derive(Debug)]
pub struct FunctionData {
    kind: FunctionType,
    arg: Box<Expression>,
}

impl FunctionData {
    pub fn new(kind: FunctionType, arg_exp: Expression) -> Self {
        let arg = Box::new(arg_exp);
        FunctionData { kind, arg }
    }

    pub fn eval(
        &self,
        idents: Option<&HashMap<String, Expression>>,
        vars: Option<&HashMap<char, f64>>,
    ) -> Result<f64, Error> {
        let value = self.arg.eval(idents, vars)?;

        match self.kind {
            FunctionType::Sin => Ok(value.sin()),
            FunctionType::Cos => Ok(value.cos()),
            FunctionType::Tan => Ok(value.tan()),
            FunctionType::Asin => Ok(value.asin()),
            FunctionType::Acos => Ok(value.acos()),
            FunctionType::Atan => Ok(value.atan()),
            FunctionType::Sign => Ok(value.signum()),
            FunctionType::Abs => Ok(value.abs()),
            FunctionType::Sqrt => Ok(value.sqrt()),
            FunctionType::Exp => Ok(value.exp()),
            FunctionType::Ln => Ok(value.ln()),
            FunctionType::Log => Ok(value.log10()),
        }
    }
}

#[derive(Debug)]
pub struct OperationData {
    kind: char,
    left: Box<Expression>,
    right: Box<Expression>,
}

impl OperationData {
    pub fn new(kind: char, left_exp: Expression, right_exp: Expression) -> Self {
        let left = Box::new(left_exp);
        let right = Box::new(right_exp);
        OperationData { kind, left, right }
    }

    pub fn eval(
        &self,
        idents: Option<&HashMap<String, Expression>>,
        vars: Option<&HashMap<char, f64>>,
    ) -> Result<f64, Error> {
        let left = self.left.eval(idents, vars)?;
        let right = self.right.eval(idents, vars)?;

        match self.kind {
            '+' => Ok(left + right),
            '-' => Ok(left - right),
            '*' => Ok(left * right),
            '/' => Ok(left / right),
            '^' => Ok(left.powf(right)),
            _ => Err(Error::UnrecognisedBinaryOperator),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Var(char),
    Float(f64),
    Ident(String),
    Function(FunctionData),
    Operation(OperationData),
}

impl Expression {
    pub fn var(c: char) -> Self {
        Expression::Var(c)
    }

    pub fn float(f: f64) -> Self {
        Expression::Float(f)
    }

    pub fn ident(s: String) -> Self {
        Expression::Ident(s)
    }

    pub fn function(f: FunctionType, arg: Expression) -> Self {
        let data = FunctionData::new(f, arg);
        Expression::Function(data)
    }

    pub fn operation(kind: char, left: Expression, right: Expression) -> Self {
        let data = OperationData::new(kind, left, right);
        Expression::Operation(data)
    }

    pub fn eval(
        &self,
        idents: Option<&HashMap<String, Expression>>,
        vars: Option<&HashMap<char, f64>>,
    ) -> Result<f64, Error> {
        match self {
            Expression::Float(f) => Ok(*f),
            Expression::Function(f) => f.eval(idents, vars),
            Expression::Operation(o) => o.eval(idents, vars),
            Expression::Var(c) => {
                let value = vars
                    .ok_or::<Error>(Error::MissingVarMap)?
                    .get(c)
                    .ok_or::<Error>(Error::MissingVarValue)?;
                Ok(*value)
            }
            Expression::Ident(s) => {
                let referred = idents
                    .ok_or::<Error>(Error::MissingIdentMap)?
                    .get(s)
                    .ok_or::<Error>(Error::MissingIdentAssignment)?;
                referred.eval(idents, vars)
            }
        }
    }
}

#[derive(Debug)]
pub struct Condition {
    kind: char,
    left: Box<Expression>,
    right: Box<Expression>,
}

impl Condition {
    pub fn new(kind: char, left_exp: Expression, right_exp: Expression) -> Self {
        let left = Box::new(left_exp);
        let right = Box::new(right_exp);
        Condition { kind, left, right }
    }

    pub fn eval(
        &self,
        idents: Option<&HashMap<String, Expression>>,
        vars: Option<&HashMap<char, f64>>,
    ) -> Result<bool, Error> {
        let left_val = self.left.eval(idents, vars)?;
        let right_val = self.right.eval(idents, vars)?;
        match self.kind {
            '=' => Ok((left_val - right_val).abs() < 100.0_f64 * f64::EPSILON),
            '<' => Ok(left_val < right_val),
            '>' => Ok(left_val > right_val),
            '≤' => Ok(left_val <= right_val),
            '≥' => Ok(left_val >= right_val),
            _ => Err(Error::UnrecognisedCondition),
        }
    }
}

#[derive(Debug)]
pub struct JunctionData {
    kind: char,
    left: Box<Junction>,
    right: Box<Junction>,
}

impl JunctionData {
    pub fn new(kind: char, left_cond: Junction, right_cond: Junction) -> Self {
        let left = Box::new(left_cond);
        let right = Box::new(right_cond);
        JunctionData { kind, left, right }
    }

    pub fn eval(
        &self,
        idents: Option<&HashMap<String, Expression>>,
        vars: Option<&HashMap<char, f64>>,
    ) -> Result<bool, Error> {
        let left_val = self.left.eval(idents, vars)?;
        let right_val = self.right.eval(idents, vars)?;
        match self.kind {
            '⋀' => Ok(left_val & right_val),
            '⋁' => Ok(left_val | right_val),
            '⊻' => Ok(left_val ^ right_val),
            '⊼' => Ok(!(left_val & right_val)),
            '⊽' => Ok(!(left_val | right_val)),
            _ => Err(Error::UnrecognisedJunction),
        }
    }
}

#[derive(Debug)]
pub enum Junction {
    Singleton(Condition),
    Meta(JunctionData),
}

impl Junction {
    pub fn singleton(cond: Condition) -> Self {
        Junction::Singleton(cond)
    }

    pub fn meta(kind: char, left_cond: Junction, right_cond: Junction) -> Self {
        let data = JunctionData::new(kind, left_cond, right_cond);
        Junction::Meta(data)
    }

    pub fn eval(
        &self,
        idents: Option<&HashMap<String, Expression>>,
        vars: Option<&HashMap<char, f64>>,
    ) -> Result<bool, Error> {
        match self {
            Junction::Meta(meta) => meta.eval(idents, vars),
            Junction::Singleton(cond) => cond.eval(idents, vars),
        }
    }
}
