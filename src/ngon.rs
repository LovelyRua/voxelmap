use crate::astree;
use crate::astree::{Boundaries, Boundary, Condition, Expression, FunctionType, Junction};
use crate::error;
use std::collections::HashMap;

pub fn generate<T: Into<String> + Clone, S: Into<f64> + Copy>(
    n: u8,
    prefix_val: Option<T>,
    offset: (Option<T>, Option<T>),
    angle_offset: Option<T>,
    scale: Option<S>,
    previous_labels: Option<HashMap<String, Expression>>,
) -> Result<astree::Structure, error::Error> {
    let prefix = if let Some(val) = prefix_val {
        val.into()
    } else {
        "".to_string()
    };

    let mut junctions = Vec::<Junction>::with_capacity(n as usize);

    let mut labels = previous_labels.unwrap_or_default();
    let mut vars = HashMap::<char, f64>::new();
    vars.insert('s', 1_f64);
    let vars_eval = Some(&vars);

    let double_n: u16 = (n as u16) * 2;

    let (x_offset, y_offset) = offset;

    let mut min_x = f64::MAX;
    let mut min_x_label = String::from("error");
    let mut max_x = f64::MIN;
    let mut max_x_label = String::from("error");

    let mut min_y = f64::MAX;
    let mut min_y_label = String::from("error");
    let mut max_y = f64::MIN;
    let mut max_y_label = String::from("error");

    let multiplier = if let Some(new_scale) = scale {
        Expression::operation('*', Expression::var('s'), Expression::float(new_scale))
    } else {
        Expression::var('s')
    };

    for i in 0..n {
        let angle_label = format!("@{}angle{}", prefix, i);
        let angle_inner = Expression::operation(
            '/',
            Expression::operation(
                '*',
                Expression::float(i),
                Expression::float(std::f64::consts::TAU),
            ),
            Expression::float(n),
        );
        let angle_offset_clone = angle_offset.clone();
        let angle_exp = if let Some(ident) = angle_offset_clone {
            Expression::operation('+', Expression::ident(ident.into()), angle_inner)
        } else {
            angle_inner
        };

        labels.insert(angle_label.to_string(), angle_exp);

        let idents = Some(&labels);

        let x_label = format!("@{}x{}", prefix, i);
        let x_base = Expression::operation(
            '*',
            multiplier.clone(),
            Expression::function(
                FunctionType::Cos,
                Expression::ident(angle_label.to_string()),
            ),
        );

        let x_offset_clone = x_offset.clone();
        let x_exp = if let Some(ident) = x_offset_clone {
            Expression::operation('+', x_base, Expression::ident(ident.into()))
        } else {
            x_base
        };
        let x_val = x_exp.eval(&idents, &vars_eval)?;
        labels.insert(x_label.to_string(), x_exp);

        if x_val < min_x {
            min_x = x_val;
            min_x_label = x_label.to_string();
        }

        if x_val > max_x {
            max_x = x_val;
            max_x_label = x_label.to_string();
        }

        let idents = Some(&labels);

        let y_label = format!("@{}y{}", prefix, i);
        let y_base = Expression::operation(
            '*',
            multiplier.clone(),
            Expression::function(
                FunctionType::Sin,
                Expression::ident(angle_label.to_string()),
            ),
        );
        let y_offset_clone = y_offset.clone();
        let y_exp = if let Some(ident) = y_offset_clone {
            Expression::operation('+', y_base, Expression::ident(ident.into()))
        } else {
            y_base
        };
        let y_val = y_exp.eval(&idents, &vars_eval)?;
        labels.insert(y_label.to_string(), y_exp);

        if y_val < min_y {
            min_y = y_val;
            min_y_label = y_label.to_string();
        }

        if y_val > max_y {
            max_y = y_val;
            max_y_label = y_label.to_string();
        }

        let normal_index: u16 = (i as u16) * 2 + 1;

        let normal_angle_label = format!("@{}nangle{}", prefix, i);
        let normal_angle_base = Expression::operation(
            '/',
            Expression::operation(
                '*',
                Expression::float(normal_index),
                Expression::float(std::f64::consts::TAU),
            ),
            Expression::float(double_n),
        );
        let angle_offset_clone = angle_offset.clone();
        let normal_angle_exp = if let Some(ident) = angle_offset_clone {
            Expression::operation('+', normal_angle_base, Expression::ident(ident.into()))
        } else {
            normal_angle_base
        };
        labels.insert(normal_angle_label.to_string(), normal_angle_exp);

        let normal_x_label = format!("@{}nx{}", prefix, i);
        let normal_x_exp = Expression::function(
            FunctionType::Cos,
            Expression::ident(normal_angle_label.to_string()),
        );
        labels.insert(normal_x_label.to_string(), normal_x_exp);

        let normal_y_label = format!("@{}ny{}", prefix, i);
        let normal_y_exp = Expression::function(
            FunctionType::Sin,
            Expression::ident(normal_angle_label.to_string()),
        );
        labels.insert(normal_y_label.to_string(), normal_y_exp);

        let j_exp = Expression::operation(
            '+',
            Expression::operation(
                '*',
                Expression::ident(normal_x_label.to_string()),
                Expression::operation(
                    '-',
                    Expression::var('x'),
                    Expression::ident(x_label.to_string()),
                ),
            ),
            Expression::operation(
                '*',
                Expression::ident(normal_y_label.to_string()),
                Expression::operation(
                    '-',
                    Expression::var('y'),
                    Expression::ident(y_label.to_string()),
                ),
            ),
        );

        junctions.push(Junction::singleton(Condition::new(
            '≤',
            j_exp,
            Expression::float(0),
        )));
    }

    while junctions.len() > 1 {
        let left = junctions.remove(0);
        let right = junctions.remove(0);
        junctions.push(Junction::meta('⋀', left, right));
    }
    let the_junction = junctions.pop().unwrap();

    let boundaries: Boundaries = [
        Boundary::new(Expression::float(0), '≤', 'z', '≤', Expression::float(0))?,
        Boundary::new(
            Expression::ident(min_x_label),
            '≤',
            'x',
            '≤',
            Expression::ident(max_x_label),
        )?,
        Boundary::new(
            Expression::ident(min_y_label),
            '≤',
            'y',
            '≤',
            Expression::ident(max_y_label),
        )?,
    ];

    Ok((Some(labels), boundaries, the_junction))
}
