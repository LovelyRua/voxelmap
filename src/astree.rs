use crate::error::Error;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::io::Write;

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionType {
    Sin,
    Cos,
    Tan,
    Sec,
    Csc,
    Cot,
    Asin,
    Acos,
    Atan,
    Sign,
    Abs,
    Sqrt,
    Exp,
    Ln,
    Log,
    Neg,
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match *self {
            FunctionType::Sin => "sin",
            FunctionType::Cos => "cos",
            FunctionType::Tan => "tan",
            FunctionType::Sec => "sec",
            FunctionType::Csc => "csc",
            FunctionType::Cot => "cot",
            FunctionType::Asin => "asin",
            FunctionType::Acos => "acos",
            FunctionType::Atan => "atan",
            FunctionType::Sign => "sign",
            FunctionType::Abs => "abs",
            FunctionType::Sqrt => "√",
            FunctionType::Exp => "exp",
            FunctionType::Ln => "ln",
            FunctionType::Log => "log",
            FunctionType::Neg => "-",
        };
        text.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionData {
    kind: FunctionType,
    arg: Box<Expression>,
}

impl fmt::Display for FunctionData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.kind, self.arg)
    }
}

impl FunctionData {
    pub fn new(kind: FunctionType, arg_exp: Expression) -> Self {
        let arg = Box::new(arg_exp);
        FunctionData { kind, arg }
    }

    pub fn eval(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
        vars: &Option<&HashMap<char, f64>>,
    ) -> Result<f64, Error> {
        let value = self.arg.eval(idents, vars)?;

        match self.kind {
            FunctionType::Sin => Ok(value.sin()),
            FunctionType::Cos => Ok(value.cos()),
            FunctionType::Tan => Ok(value.tan()),
            FunctionType::Sec => Ok(value.cos().recip()),
            FunctionType::Csc => Ok(value.sin().recip()),
            FunctionType::Cot => Ok(value.tan().recip()),
            FunctionType::Asin => Ok(value.asin()),
            FunctionType::Acos => Ok(value.acos()),
            FunctionType::Atan => Ok(value.atan()),
            FunctionType::Sign => Ok(value.signum()),
            FunctionType::Abs => Ok(value.abs()),
            FunctionType::Sqrt => Ok(value.sqrt()),
            FunctionType::Exp => Ok(value.exp()),
            FunctionType::Ln => Ok(value.ln()),
            FunctionType::Log => Ok(value.log10()),
            FunctionType::Neg => Ok(-value),
        }
    }

    pub fn graph(&self, output: &mut impl Write, id: usize) -> Result<usize, Error> {
        let label = match self.kind {
            FunctionType::Sin => "sin",
            FunctionType::Cos => "cos",
            FunctionType::Tan => "tan",
            FunctionType::Sec => "sec",
            FunctionType::Csc => "csc",
            FunctionType::Cot => "cot",
            FunctionType::Asin => "asin",
            FunctionType::Acos => "acos",
            FunctionType::Atan => "atan",
            FunctionType::Sign => "sign",
            FunctionType::Abs => "abs",
            FunctionType::Sqrt => "sqrt",
            FunctionType::Exp => "exp",
            FunctionType::Ln => "ln",
            FunctionType::Log => "log_10",
            FunctionType::Neg => "-",
        };
        writeln!(
            output,
            "\tnode{} [label=\"{}\",shape=trapezium];",
            id, label
        )?;
        let max_id = self.arg.graph(output, id + 1)?;
        writeln!(output, "\tnode{} -> node{};", id, max_id)?;
        Ok(max_id)
    }
}

#[derive(Debug, Clone)]
pub struct OperationData {
    kind: char,
    left: Box<Expression>,
    right: Box<Expression>,
}

impl fmt::Display for OperationData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.kind, self.right)
    }
}

impl OperationData {
    pub fn new(kind: char, left_exp: Expression, right_exp: Expression) -> Self {
        let left = Box::new(left_exp);
        let right = Box::new(right_exp);
        OperationData { kind, left, right }
    }

    pub fn eval(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
        vars: &Option<&HashMap<char, f64>>,
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

    pub fn graph(&self, output: &mut impl Write, id: usize) -> Result<usize, Error> {
        writeln!(
            output,
            "\tnode{} [label=\"{}\",shape=invtriangle];",
            id, self.kind
        )?;
        let new_id = self.left.graph(output, id + 1)?;
        let max_id = self.right.graph(output, new_id + 1)?;
        writeln!(output, "\tnode{} -> node{} [label=\"left\"];", id, id + 1)?;
        writeln!(
            output,
            "\tnode{} -> node{} [label=\"right\"];",
            id,
            new_id + 1
        )?;
        Ok(max_id)
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Var(char),
    Float(f64),
    Ident(String),
    Function(FunctionData),
    Operation(OperationData),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Var(v) => v.fmt(f),
            Expression::Float(n) => n.fmt(f),
            Expression::Ident(s) => s.fmt(f),
            Expression::Function(function) => function.fmt(f),
            Expression::Operation(op) => op.fmt(f),
        }
    }
}

impl Expression {
    pub fn var(c: char) -> Self {
        Expression::Var(c)
    }

    pub fn float<T: Into<f64>>(f: T) -> Self {
        Expression::Float(f.into())
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
        idents: &Option<&HashMap<String, Expression>>,
        vars: &Option<&HashMap<char, f64>>,
    ) -> Result<f64, Error> {
        match self {
            Expression::Float(f) => Ok(*f),
            Expression::Function(f) => f.eval(idents, vars),
            Expression::Operation(o) => o.eval(idents, vars),
            Expression::Var(c) => {
                let var_values = vars.as_ref().ok_or::<Error>(Error::MissingVarMap)?;
                let value = var_values.get(c).ok_or::<Error>(Error::MissingVarValue)?;
                Ok(*value)
            }
            Expression::Ident(s) => {
                let ident_exps = idents.as_ref().ok_or::<Error>(Error::MissingIdentMap)?;
                let referred = ident_exps
                    .get(s)
                    .ok_or::<Error>(Error::MissingIdentAssignment)?;
                referred.eval(idents, vars)
            }
        }
    }

    pub fn graph(&self, output: &mut impl Write, id: usize) -> Result<usize, Error> {
        match self {
            Expression::Float(f) => {
                writeln!(output, "\tnode{} [label=\"{}\",shape=oval];", id, f)?;
                Ok(id)
            }
            Expression::Function(f) => f.graph(output, id),
            Expression::Operation(o) => o.graph(output, id),
            Expression::Var(c) => {
                writeln!(output, "\tnode{} [label=\"{}\",shape=egg];", id, c)?;
                Ok(id)
            }
            Expression::Ident(s) => {
                writeln!(output, "\tnode{} [label=\"{}\",shape=cds];", id, s)?;
                Ok(id)
            }
        }
    }

    pub fn var_dependencies(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
    ) -> Result<HashSet<char>, Error> {
        match self {
            Expression::Float(_) => Ok(HashSet::new()),
            Expression::Function(f) => f.arg.var_dependencies(idents),
            Expression::Operation(o) => {
                let left = o.left.var_dependencies(idents)?;
                let right = o.right.var_dependencies(idents)?;
                let mut result: HashSet<char> = HashSet::new();
                for dep in left {
                    result.insert(dep);
                }
                for dep in right {
                    result.insert(dep);
                }
                Ok(result)
            }
            Expression::Var(c) => {
                let mut result: HashSet<char> = HashSet::new();
                result.insert(*c);
                Ok(result)
            }
            Expression::Ident(s) => {
                let ident_exps = idents.as_ref().ok_or::<Error>(Error::MissingIdentMap)?;
                let referred = ident_exps
                    .get(s)
                    .ok_or::<Error>(Error::MissingIdentAssignment)?;
                referred.var_dependencies(idents)
            }
        }
    }

    pub fn ident_dependencies(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
    ) -> Result<HashSet<(String, usize)>, Error> {
        match self {
            Expression::Float(_) => Ok(HashSet::new()),
            Expression::Function(f) => f.arg.ident_dependencies(idents),
            Expression::Operation(o) => {
                let left = o.left.ident_dependencies(idents)?;
                let right = o.right.ident_dependencies(idents)?;
                let mut result: HashSet<(String, usize)> = HashSet::new();
                for dep in left.union(&right) {
                    result.insert(dep.to_owned());
                }
                Ok(result)
            }
            Expression::Var(_) => Ok(HashSet::new()),
            Expression::Ident(s) => {
                let mut result: HashSet<(String, usize)> = HashSet::new();

                let mut max_recursion: usize = 0;

                let ident_exps = idents.as_ref().ok_or::<Error>(Error::MissingIdentMap)?;
                let referred = ident_exps
                    .get(s)
                    .ok_or::<Error>(Error::MissingIdentAssignment)?;
                let recursive = referred.ident_dependencies(idents)?;
                for dep in recursive {
                    let (_, level) = dep;
                    result.insert(dep);
                    if level + 1 > max_recursion {
                        max_recursion = level + 1;
                    }
                }
                result.insert((s.to_owned(), max_recursion));
                Ok(result)
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

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.kind, self.right)
    }
}

impl Condition {
    pub fn new(kind: char, left_exp: Expression, right_exp: Expression) -> Self {
        let left = Box::new(left_exp);
        let right = Box::new(right_exp);
        Condition { kind, left, right }
    }

    pub fn eval(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
        vars: &Option<&HashMap<char, f64>>,
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

    pub fn graph(&self, output: &mut impl Write, id: usize) -> Result<usize, Error> {
        writeln!(
            output,
            "\tnode{} [label=\"{}\",shape=square];",
            id, self.kind
        )?;
        let new_id = self.left.graph(output, id + 1)?;
        let max_id = self.right.graph(output, new_id + 1)?;
        writeln!(output, "\tnode{} -> node{} [label=\"left\"];", id, id + 1)?;
        writeln!(
            output,
            "\tnode{} -> node{} [label=\"right\"];",
            id,
            new_id + 1
        )?;
        Ok(max_id)
    }

    pub fn ident_dependencies(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
    ) -> Result<HashSet<(String, usize)>, Error> {
        let left_idents = self.left.ident_dependencies(idents)?;
        let right_idents = self.right.ident_dependencies(idents)?;
        let mut result: HashSet<(String, usize)> = HashSet::new();
        for dep in left_idents.union(&right_idents) {
            result.insert(dep.to_owned());
        }
        Ok(result)
    }
}

#[derive(Debug)]
pub struct JunctionData {
    kind: char,
    left: Box<Junction>,
    right: Box<Junction>,
}

impl fmt::Display for JunctionData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{{} {} {}}}", self.left, self.kind, self.right)
    }
}

impl JunctionData {
    pub fn new(kind: char, left_cond: Junction, right_cond: Junction) -> Self {
        let left = Box::new(left_cond);
        let right = Box::new(right_cond);
        JunctionData { kind, left, right }
    }

    pub fn eval(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
        vars: &Option<&HashMap<char, f64>>,
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

    pub fn graph(&self, output: &mut impl Write, id: usize) -> Result<usize, Error> {
        writeln!(
            output,
            "\tnode{} [label=\"{}\",shape=hexagon];",
            id, self.kind
        )?;
        let new_id = self.left.graph(output, id + 1)?;
        let max_id = self.right.graph(output, new_id + 1)?;
        writeln!(output, "\tnode{} -> node{} [label=\"left\"];", id, id + 1)?;
        writeln!(
            output,
            "\tnode{} -> node{} [label=\"right\"];",
            id,
            new_id + 1
        )?;
        Ok(max_id)
    }

    pub fn ident_dependencies(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
    ) -> Result<HashSet<(String, usize)>, Error> {
        let left_idents = self.left.ident_dependencies(idents)?;
        let right_idents = self.right.ident_dependencies(idents)?;
        let mut result: HashSet<(String, usize)> = HashSet::new();
        for dep in left_idents.union(&right_idents) {
            result.insert(dep.to_owned());
        }
        Ok(result)
    }
}

#[derive(Debug)]
pub enum Junction {
    Singleton(Condition),
    Meta(JunctionData),
}

impl fmt::Display for Junction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Junction::Singleton(condition) => condition.fmt(f),
            Junction::Meta(meta) => meta.fmt(f),
        }
    }
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
        idents: &Option<&HashMap<String, Expression>>,
        vars: &Option<&HashMap<char, f64>>,
    ) -> Result<bool, Error> {
        match self {
            Junction::Meta(meta) => meta.eval(idents, vars),
            Junction::Singleton(cond) => cond.eval(idents, vars),
        }
    }

    pub fn graph(&self, output: &mut impl Write, id: usize) -> Result<usize, Error> {
        match self {
            Junction::Meta(meta) => meta.graph(output, id),
            Junction::Singleton(cond) => cond.graph(output, id),
        }
    }

    pub fn ident_dependencies(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
    ) -> Result<HashSet<(String, usize)>, Error> {
        match self {
            Junction::Singleton(cond) => cond.ident_dependencies(idents),
            Junction::Meta(meta) => meta.ident_dependencies(idents),
        }
    }
}

#[derive(Debug)]
pub struct Boundary {
    pub var: char,
    pub min: Expression,
    pub max: Expression,
}

impl fmt::Display for Boundary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ≤ {} ≤ {}", self.min, self.var, self.max)
    }
}

impl Boundary {
    pub fn new(
        l: Expression,
        lcond: char,
        var: char,
        rcond: char,
        r: Expression,
    ) -> Result<Self, ()> {
        if var != 'x' && var != 'y' && var != 'z' {
            return Err(());
        }
        if lcond == '<' || lcond == '≤' {
            if rcond == '<' || rcond == '≤' {
                let min = l;
                let max = r;
                return Ok(Boundary { var, min, max });
            }
            return Err(());
        } else if lcond == '>' || lcond == '≥' {
            if rcond == '>' || rcond == '≥' {
                let min = r;
                let max = l;
                return Ok(Boundary { var, min, max });
            }
            return Err(());
        }
        Err(())
    }

    pub fn ident_dependencies(
        &self,
        idents: &Option<&HashMap<String, Expression>>,
    ) -> Result<HashSet<(String, usize)>, Error> {
        let left_idents = self.min.ident_dependencies(idents)?;
        let right_idents = self.max.ident_dependencies(idents)?;
        let mut result: HashSet<(String, usize)> = HashSet::new();
        for dep in left_idents.union(&right_idents) {
            result.insert(dep.to_owned());
        }
        Ok(result)
    }
}

pub type Boundaries = [Boundary; 3];

pub type Structure = (Option<HashMap<String, Expression>>, Boundaries, Junction);
