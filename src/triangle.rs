use crate::astree;
use crate::astree::{Boundaries, Boundary, Condition, Expression, Junction};
use crate::error;
use std::collections::HashMap;

pub fn any_triangle<T: Into<String> + Clone>(
    x_1: T,
    y_1: T,
    x_2: T,
    y_2: T,
    x_3: T,
    y_3: T,
    previous_labels: Option<HashMap<String, Expression>>,
) -> Result<astree::Structure, error::Error> {
    let mut junctions = Vec::<Junction>::with_capacity(3);

    let labels = previous_labels.unwrap_or_default();
    let mut vars = HashMap::<char, f64>::new();
    vars.insert('s', 1_f64);
    let vars_eval = Some(&vars);

    // Determine if clockwise
    let idents = Some(&labels);
    let clockwise_exp = Expression::operation(
        '+',
        Expression::operation(
            '+',
            Expression::operation(
                '*',
                Expression::operation(
                    '-',
                    Expression::ident(x_3.clone().into()),
                    Expression::ident(x_2.clone().into()),
                ),
                Expression::operation(
                    '+',
                    Expression::ident(y_3.clone().into()),
                    Expression::ident(y_2.clone().into()),
                ),
            ),
            Expression::operation(
                '*',
                Expression::operation(
                    '-',
                    Expression::ident(x_1.clone().into()),
                    Expression::ident(x_3.clone().into()),
                ),
                Expression::operation(
                    '+',
                    Expression::ident(y_1.clone().into()),
                    Expression::ident(y_3.clone().into()),
                ),
            ),
        ),
        Expression::operation(
            '*',
            Expression::operation(
                '-',
                Expression::ident(x_2.clone().into()),
                Expression::ident(x_1.clone().into()),
            ),
            Expression::operation(
                '+',
                Expression::ident(y_2.clone().into()),
                Expression::ident(y_1.clone().into()),
            ),
        ),
    );
    let clockwise = clockwise_exp.eval(&idents, &vars_eval)? > 1e-10_f64;

    let ident_x_1 = x_1.into();
    let ident_y_1 = y_1.into();
    let ident_x_2;
    let ident_y_2;
    let ident_x_3;
    let ident_y_3;
    if clockwise {
        ident_x_2 = x_3.into();
        ident_y_2 = y_3.into();
        ident_x_3 = x_2.into();
        ident_y_3 = y_2.into();
    } else {
        ident_x_2 = x_2.into();
        ident_y_2 = y_2.into();
        ident_x_3 = x_3.into();
        ident_y_3 = y_3.into();
    }

    let mut min_x = f64::MAX;
    let mut min_x_label = String::from("error");
    let mut max_x = f64::MIN;
    let mut max_x_label = String::from("error");

    let mut min_y = f64::MAX;
    let mut min_y_label = String::from("error");
    let mut max_y = f64::MIN;
    let mut max_y_label = String::from("error");

    let x_1_val = Expression::ident(ident_x_1.clone()).eval(&idents, &vars_eval)?;
    let x_2_val = Expression::ident(ident_x_2.clone()).eval(&idents, &vars_eval)?;
    let x_3_val = Expression::ident(ident_x_3.clone()).eval(&idents, &vars_eval)?;
    let y_1_val = Expression::ident(ident_y_1.clone()).eval(&idents, &vars_eval)?;
    let y_2_val = Expression::ident(ident_y_2.clone()).eval(&idents, &vars_eval)?;
    let y_3_val = Expression::ident(ident_y_3.clone()).eval(&idents, &vars_eval)?;

    if x_1_val < min_x {
        min_x = x_1_val;
        min_x_label = ident_x_1.clone();
    }
    if x_2_val < min_x {
        min_x = x_2_val;
        min_x_label = ident_x_2.clone();
    }
    if x_3_val < min_x {
        min_x_label = ident_x_3.clone();
    }
    if x_1_val > max_x {
        max_x = x_1_val;
        max_x_label = ident_x_1.clone();
    }
    if x_2_val > max_x {
        max_x = x_2_val;
        max_x_label = ident_x_2.clone();
    }
    if x_3_val > max_x {
        max_x_label = ident_x_3.clone();
    }
    if y_1_val < min_y {
        min_y = y_1_val;
        min_y_label = ident_y_1.clone();
    }
    if y_2_val < min_y {
        min_y = y_2_val;
        min_y_label = ident_y_2.clone();
    }
    if y_3_val < min_y {
        min_y_label = ident_y_3.clone();
    }
    if y_1_val > max_y {
        max_y = y_1_val;
        max_y_label = ident_y_1.clone();
    }
    if y_2_val > max_y {
        max_y = y_2_val;
        max_y_label = ident_y_2.clone();
    }
    if y_3_val > max_y {
        max_y_label = ident_y_3.clone();
    }

    let normal_1_y = Expression::operation(
        '-',
        Expression::ident(ident_x_1.clone()),
        Expression::ident(ident_x_2.clone()),
    )
    .eval(&idents, &vars_eval)?;
    let normal_1_x = Expression::operation(
        '-',
        Expression::ident(ident_y_2.clone()),
        Expression::ident(ident_y_1.clone()),
    )
    .eval(&idents, &vars_eval)?;
    let normal_2_y = Expression::operation(
        '-',
        Expression::ident(ident_x_2.clone()),
        Expression::ident(ident_x_3.clone()),
    )
    .eval(&idents, &vars_eval)?;
    let normal_2_x = Expression::operation(
        '-',
        Expression::ident(ident_y_3.clone()),
        Expression::ident(ident_y_2.clone()),
    )
    .eval(&idents, &vars_eval)?;
    let normal_3_y = Expression::operation(
        '-',
        Expression::ident(ident_x_3.clone()),
        Expression::ident(ident_x_1.clone()),
    )
    .eval(&idents, &vars_eval)?;
    let normal_3_x = Expression::operation(
        '-',
        Expression::ident(ident_y_1.clone()),
        Expression::ident(ident_y_3.clone()),
    )
    .eval(&idents, &vars_eval)?;

    let j_exp = Expression::operation(
        '+',
        Expression::operation(
            '*',
            Expression::float(normal_1_x),
            Expression::operation('-', Expression::var('x'), Expression::ident(ident_x_1)),
        ),
        Expression::operation(
            '*',
            Expression::float(normal_1_y),
            Expression::operation('-', Expression::var('y'), Expression::ident(ident_y_1)),
        ),
    );

    junctions.push(Junction::singleton(Condition::new(
        '≤',
        j_exp,
        Expression::float(0),
    )));

    let j_exp = Expression::operation(
        '+',
        Expression::operation(
            '*',
            Expression::float(normal_2_x),
            Expression::operation('-', Expression::var('x'), Expression::ident(ident_x_2)),
        ),
        Expression::operation(
            '*',
            Expression::float(normal_2_y),
            Expression::operation('-', Expression::var('y'), Expression::ident(ident_y_2)),
        ),
    );

    junctions.push(Junction::singleton(Condition::new(
        '≤',
        j_exp,
        Expression::float(0),
    )));

    let j_exp = Expression::operation(
        '+',
        Expression::operation(
            '*',
            Expression::float(normal_3_x),
            Expression::operation('-', Expression::var('x'), Expression::ident(ident_x_3)),
        ),
        Expression::operation(
            '*',
            Expression::float(normal_3_y),
            Expression::operation('-', Expression::var('y'), Expression::ident(ident_y_3)),
        ),
    );

    junctions.push(Junction::singleton(Condition::new(
        '≤',
        j_exp,
        Expression::float(0),
    )));

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
