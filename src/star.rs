use crate::astree;
use crate::astree::{Expression, Junction};
use crate::error;
use crate::ngon;
use crate::triangle::any_triangle;
use std::collections::HashMap;

pub fn generate<T: Into<String> + Clone, S: Into<f64> + Copy>(
    n: u8,
    prefix_val: Option<T>,
    offset: (Option<T>, Option<T>),
    angle_offset: Option<T>,
    scale: Option<S>,
    star_scale: Option<S>,
    previous_labels: Option<HashMap<String, Expression>>,
) -> Result<astree::Structure, error::Error> {
    let prefix = if let Some(val) = prefix_val {
        val.into()
    } else {
        "".to_string()
    };

    let mut junctions = Vec::<Junction>::with_capacity(n as usize);

    let mut labels = previous_labels.unwrap_or_default();

    let (x_offset, y_offset) = offset;

    let center_x;
    let center_y;
    if let Some(x_label) = x_offset {
        center_x = x_label.into();
    } else {
        center_x = format!("@{}centerx", prefix);
        labels.insert(center_x.clone(), Expression::float(0_f64));
    }
    if let Some(y_label) = y_offset {
        center_y = y_label.into();
    } else {
        center_y = format!("@{}centery", prefix);
        labels.insert(center_y.clone(), Expression::float(0_f64));
    }

    let new_angle_offset = angle_offset.map(|a| a.into());
    let new_scale = scale.map(|s| s.into());

    let inner_prefix = format!("{}inner", prefix);

    let (new_labels, _, _) = ngon::generate(
        n,
        Some(inner_prefix.clone()),
        (Some(center_x.clone()), Some(center_y.clone())),
        new_angle_offset,
        new_scale,
        Some(labels),
    )?;

    let angle_offset_label = format!("@{}innernangle0", prefix);
    let outer_prefix = format!("{}outer", prefix);

    let recip_n: f64 = (n as f64).recip();
    let scale_angle = recip_n * std::f64::consts::PI;
    let double_scale_angle = scale_angle + scale_angle;
    let default_scale = scale_angle.sin() * double_scale_angle.tan() + scale_angle.cos();

    let outer_scale =
        star_scale.map(|s| s.into()).unwrap_or(default_scale) * new_scale.unwrap_or(1_f64);

    let (definitive_labels, boundaries, _) = ngon::generate(
        n,
        Some(outer_prefix.clone()),
        (Some(center_x.clone()), Some(center_y.clone())),
        Some(angle_offset_label),
        Some(outer_scale),
        new_labels,
    )?;

    let mut input_labels = definitive_labels;

    for i in 0..n {
        let label_x_1 = format!("@{}x{}", inner_prefix, i);
        let label_y_1 = format!("@{}y{}", inner_prefix, i);
        let label_x_2 = format!("@{}x{}", outer_prefix, i);
        let label_y_2 = format!("@{}y{}", outer_prefix, i);
        let (definitive_labels, _, junction) = any_triangle(
            center_x.clone(),
            center_y.clone(),
            label_x_1,
            label_y_1,
            label_x_2,
            label_y_2,
            input_labels,
        )?;

        input_labels = definitive_labels;

        junctions.push(junction);

        let label_x_1 = format!("@{}x{}", outer_prefix, i);
        let label_y_1 = format!("@{}y{}", outer_prefix, i);
        let label_x_2 = format!("@{}x{}", inner_prefix, (i + 1) % n);
        let label_y_2 = format!("@{}y{}", inner_prefix, (i + 1) % n);
        let (definitive_labels, _, junction) = any_triangle(
            center_x.clone(),
            center_y.clone(),
            label_x_1,
            label_y_1,
            label_x_2,
            label_y_2,
            input_labels,
        )?;

        input_labels = definitive_labels;

        junctions.push(junction);
    }

    while junctions.len() > 1 {
        let left = junctions.remove(0);
        let right = junctions.remove(0);
        junctions.push(Junction::meta('⋁', left, right));
    }
    let the_junction = junctions.pop().unwrap();

    Ok((input_labels, boundaries, the_junction))
}
