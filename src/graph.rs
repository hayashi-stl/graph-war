use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_rapier2d::prelude::*;
use bevy_svg::prelude::Svg2dBundle;
use fxhash::FxHashMap;
use once_cell::sync::Lazy;
use pest::{
    error::{Error, ErrorVariant, LineColLocation},
    iterators::{Pair, Pairs},
    Parser,
};
use std::{iter, time::Duration};

use crate::{
    collision::{CollisionGroups, PrevPosition},
    ui::{FunctionEntry, FunctionStatus, FunctionWhere, FunctionX, FunctionY, Textbox},
    z, Owner, PlayerLabel,
};

pub const QUICK_HELP: &str = r"
Examples:
x(t) = v                    x(t) = -2 * t
y(t) = u^2                  y(t) = sin(3 * t)
where u = 2 * t - 4         where
      v = u + t * 0               <nothing>

Operations:
add (+), subtract (-), multiply (*), divide (/),
floor divide (//), modulo (%), exponent (^)

Constants: tau, pi, e

Unary functions (syntax: `sin a`):
sin, cos, tan, asin, acos, atan, sinh, cosh, tanh,
asinh, acosh, atanh, ln, log2, log10, sqrt, cbrt,
abs, sign, floor, ceil, fract

Binary functions (syntax: `min a b`): min, max, atan2

Precedence (highest to lowest):
function call
^
* / // %
+ -
";

#[derive(Parser)]
#[grammar = "function.pest"]
pub struct FunctionParser;

macro_rules! def_str_lookup {
    (
        $(#[$attr:meta])*
        pub enum $enum_name:ident {
            $($var:ident ( $string:tt ) => $func:expr),* $(,)?
        }
        static $set:ident: $set_ty:ty;
        const $arr:ident: $arr_ty:ty;
    ) => {
        $(#[$attr])*
        pub enum $enum_name {
            $($var),*
        }

        static $set: $set_ty = once_cell::sync::Lazy::new(|| [
            $(($string, $enum_name::$var)),*
        ].into_iter().collect());

        const $arr: $arr_ty = [$($func),*];
    };
}

def_str_lookup! {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum Call1 {
        Sin("sin") => f64::sin,
        Cos("cos") => f64::cos,
        Tan("tan") => f64::tan,
        Asin("asin") => f64::asin,
        Acos("acos") => f64::acos,
        Atan("atan") => f64::atan,
        Sinh("sinh") => f64::sinh,
        Cosh("cosh") => f64::cosh,
        Tanh("tanh") => f64::tanh,
        Asinh("asinh") => f64::asinh,
        Acosh("acosh") => f64::acosh,
        Atanh("atanh") => f64::atanh,
        Ln("ln") => f64::ln,
        Log2("log2") => f64::log2,
        Log10("log10") => f64::log10,
        Sqrt("sqrt") => f64::sqrt,
        Cbrt("cbrt") => f64::cbrt,
        Abs("abs") => f64::abs,
        Sign("sign") => f64::signum,
        Floor("floor") => f64::floor,
        Ceil("ceil") => f64::ceil,
        Fract("fract") => f64::fract,
    }

    static CALL_1_FN_MAP: Lazy<FxHashMap<&str, Call1>>;

    const CALL_1_FNS: [fn(f64) -> f64; 22];
}

impl Call1 {
    fn call(self, t: f64) -> f64 {
        CALL_1_FNS[self as usize](t)
    }
}

def_str_lookup! {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum Call2 {
        Min("min") => f64::min,
        Max("max") => f64::max,
        Atan2("atan2") => f64::atan2,
    }

    static CALL_2_FN_MAP: Lazy<FxHashMap<&str, Call2>>;

    const CALL_2_FNS: [fn(f64, f64) -> f64; 3];
}

impl Call2 {
    fn call(self, t1: f64, t2: f64) -> f64 {
        CALL_2_FNS[self as usize](t1, t2)
    }
}

static CONSTS: Lazy<FxHashMap<&str, f64>> = Lazy::new(|| {
    [
        ("tau", std::f64::consts::TAU),
        ("pi", std::f64::consts::PI),
        ("e", std::f64::consts::E),
    ]
    .into_iter()
    .collect()
});

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OpType {
    Normal,
    Inverse,
    Third,
    Fourth,
}

#[derive(Clone, Debug)]
pub enum Function {
    /// If the option is None, the variable is `t`
    Var(Option<usize>),
    Const(f64),
    Add(Vec<(Function, OpType)>),
    Mul(Vec<(Function, OpType)>),
    Exp(Vec<Function>),
    Neg(Box<Function>),
    Call1(Call1, Box<Function>),
    Call2(Call2, Box<[Function; 2]>),
}

/// Labels a rocket
#[derive(Component)]
pub struct Rocket;

const ROCKET_TIME: f64 = 5.0;

/// The offset of a rocket from the parametric equation it follows
#[derive(Component)]
pub struct Offset(Vec2);

impl Function {
    fn from_multi_op_sequence(
        pair: Pair<Rule>,
        variant: impl Fn(Vec<(Function, OpType)>) -> Self,
        signs: &[(&str, OpType)],
        var_map: &VarIndexMap,
    ) -> Result<Self, Error<Rule>> {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        if inner.peek().is_some() {
            let pair_vec = inner.collect::<Vec<_>>();
            Ok(variant(
                iter::once(Self::from_pair(first, var_map).map(|f| (f, OpType::Normal)))
                    .chain(pair_vec.chunks(2).map(|pairs| {
                        let op_sign = &pairs[0];
                        let expr = pairs[1].clone();
                        Self::from_pair(expr, var_map).map(|f| {
                            (
                                f,
                                signs
                                    .iter()
                                    .find_map(|(sign, op)| (*sign == op_sign.as_str()).then(|| *op))
                                    .unwrap(),
                            )
                        })
                    }))
                    .collect::<Result<_, _>>()?,
            ))
        } else {
            Self::from_pair(first, var_map)
        }
    }

    fn from_op_sequence(
        pair: Pair<Rule>,
        variant: impl Fn(Vec<Function>) -> Self,
        var_map: &VarIndexMap,
    ) -> Result<Self, Error<Rule>> {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        if inner.peek().is_some() {
            Ok(variant(
                iter::once(first)
                    .chain(inner)
                    .map(|p| Self::from_pair(p, var_map))
                    .collect::<Result<_, _>>()?,
            ))
        } else {
            Self::from_pair(first, var_map)
        }
    }

    fn from_pair(pair: Pair<Rule>, var_map: &VarIndexMap) -> Result<Self, Error<Rule>> {
        match pair.as_rule() {
            Rule::expr => Self::from_pair(pair.into_inner().next().unwrap(), var_map),
            Rule::add => Self::from_multi_op_sequence(
                pair,
                Self::Add,
                &[("+", OpType::Normal), ("-", OpType::Inverse)],
                var_map,
            ),
            Rule::mul => Self::from_multi_op_sequence(
                pair,
                Self::Mul,
                &[
                    ("*", OpType::Normal),
                    ("/", OpType::Inverse),
                    ("//", OpType::Third),
                    ("%", OpType::Fourth),
                ],
                var_map,
            ),
            Rule::neg => {
                let negate = pair.as_str().starts_with('-');
                let expr = Self::from_pair(pair.into_inner().next().unwrap(), var_map)?;
                if negate {
                    Ok(Self::Neg(Box::new(expr)))
                } else {
                    Ok(expr)
                }
            }
            Rule::exp => Self::from_op_sequence(pair, Self::Exp, var_map),
            Rule::call_1 => {
                let mut pairs = pair.into_inner();
                let func = pairs.next().unwrap();
                let expr = pairs.next().unwrap();
                if let Some(call) = CALL_1_FN_MAP.get(func.as_str()) {
                    Ok(Self::Call1(
                        *call,
                        Box::new(Self::from_pair(expr, var_map)?),
                    ))
                } else {
                    Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("unknown unary function: {}", func.as_str()),
                        },
                        func.as_span(),
                    ))
                }
            }
            Rule::call_2 => {
                let mut pairs = pair.into_inner();
                let func = pairs.next().unwrap();
                let expr1 = pairs.next().unwrap();
                let expr2 = pairs.next().unwrap();
                if let Some(call) = CALL_2_FN_MAP.get(func.as_str()) {
                    Ok(Self::Call2(
                        *call,
                        Box::new([
                            Self::from_pair(expr1, var_map)?,
                            Self::from_pair(expr2, var_map)?,
                        ]),
                    ))
                } else {
                    Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("unknown binary function: {}", func.as_str()),
                        },
                        func.as_span(),
                    ))
                }
            }
            Rule::primary => Self::from_pair(pair.into_inner().next().unwrap(), var_map),
            Rule::primitive => Self::from_pair(pair.into_inner().next().unwrap(), var_map),
            Rule::var => {
                if let Some(constant) = CONSTS.get(pair.as_str()) {
                    Ok(Self::Const(*constant))
                } else if let Some(index) = var_map.get(pair.as_str()) {
                    Ok(Self::Var(*index))
                } else {
                    Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("unknown variable: {}", pair.as_str()),
                        },
                        pair.as_span(),
                    ))
                }
            }
            Rule::constant => Ok(Self::Const(str::parse(pair.as_str()).unwrap())),

            _ => unreachable!(),
        }
    }

    fn eval(&self, t: f64, assigns: &[Function]) -> f64 {
        match self {
            Self::Var(index) => index.map(|i| assigns[i].eval(t, assigns)).unwrap_or(t),
            Self::Const(c) => *c,
            Self::Add(fs) => fs.iter().fold(0.0, |acc, (f, op)| match *op {
                OpType::Normal => acc + f.eval(t, assigns),
                OpType::Inverse => acc - f.eval(t, assigns),
                _ => unreachable!(),
            }),
            Self::Mul(fs) => fs.iter().fold(1.0, |acc, (f, op)| match *op {
                OpType::Normal => acc * f.eval(t, assigns),
                OpType::Inverse => acc / f.eval(t, assigns),
                OpType::Third => acc.div_euclid(f.eval(t, assigns)),
                OpType::Fourth => acc.rem_euclid(f.eval(t, assigns)),
            }),
            Self::Exp(fs) => fs
                .iter()
                .rev()
                .fold(1.0, |acc, f| f.eval(t, assigns).powf(acc)),
            Self::Neg(f) => -f.eval(t, assigns),
            Self::Call1(call, f) => call.call(f.eval(t, assigns)),
            Self::Call2(call, fs) => call.call(fs[0].eval(t, assigns), fs[1].eval(t, assigns)),
        }
    }
}

/// Maps variable indexes to functions
type AssignVec = Vec<Function>;

/// Maps variable names to indexes
type VarIndexMap = FxHashMap<String, Option<usize>>;

trait Assigns: Sized {
    fn from_pairs(pairs: Pairs<Rule>) -> Result<(Self, VarIndexMap), Error<Rule>>;
}

impl Assigns for AssignVec {
    fn from_pairs(pairs: Pairs<Rule>) -> Result<(Self, VarIndexMap), Error<Rule>> {
        let mut var_map = [("t".to_owned(), None)]
            .into_iter()
            .collect::<FxHashMap<_, _>>();

        let assign_vec = pairs
            .filter(|pair| pair.as_rule() != Rule::EOI)
            .enumerate()
            .map(|(i, pair)| {
                let mut pairs = pair.into_inner();
                let var = pairs.next().unwrap();
                let var = if var_map.contains_key(var.as_str()) {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("'{}' is already defined", var.as_str()),
                        },
                        var.as_span(),
                    ));
                } else if CONSTS.contains_key(var.as_str()) {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("cannot assign to constant '{}'", var.as_str()),
                        },
                        var.as_span(),
                    ));
                } else {
                    var.as_str().to_owned()
                };

                var_map.insert(var, Some(i));
                let expr = Function::from_pair(pairs.next().unwrap(), &var_map)?;
                Ok(expr)
            })
            .collect::<Result<_, _>>()?;

        Ok((assign_vec, var_map))
    }
}

#[derive(Clone, Debug, Component)]
pub struct Parametric {
    pub x: Function,
    pub y: Function,
    pub assigns: AssignVec,
}

impl Parametric {
    pub fn try_new(x: Function, y: Function, assigns: AssignVec) -> Result<Self, Error<Rule>> {
        Ok(Self { x, y, assigns })
    }

    fn eval(&self, t: f64) -> Vec2 {
        Vec2::new(
            self.x.eval(t, &self.assigns) as f32,
            self.y.eval(t, &self.assigns) as f32,
        )
    }
}

#[derive(Clone, Debug)]
/// Event that says that some player should fire a rocket from their position
pub struct FireRocket {
    pub player_index: u32,
}

struct ParseError {
    error: Error<Rule>,
    label: String,
    include_line: bool,
}

impl ParseError {
    fn new(error: Error<Rule>, label: String, include_line: bool) -> Self {
        Self {
            error,
            label,
            include_line,
        }
    }
}

fn set_status_text(text: &mut Text, error: Option<ParseError>) {
    if let Some(error) = error {
        let message_end = match error.error.variant {
            ErrorVariant::CustomError { message } => message,
            ErrorVariant::ParsingError { .. } => "syntax".into(),
        };
        let (line, column) = match error.error.line_col {
            LineColLocation::Pos((l, c)) | LineColLocation::Span((l, c), _) => (l, c),
        };
        let line_message = if error.include_line {
            format!("line {} ", line)
        } else {
            String::new()
        };
        let message = format!(
            "Error in {} ({}col {}): {}\n",
            error.label, line_message, column, message_end
        );
        text.sections[0].value = message.into();
        text.sections[0].style.color = Color::MAROON;
    } else {
        text.sections[0].value = "Successfully entered functions\n".into();
        text.sections[0].style.color = Color::DARK_GREEN;
    }
}

pub fn handle_fire_events(
    function_x: Query<(&Owner, &Textbox), (With<FunctionX>, With<FunctionEntry>)>,
    function_y: Query<(&Owner, &Textbox), (With<FunctionY>, With<FunctionEntry>)>,
    assigns: Query<(&Owner, &Textbox), (With<FunctionWhere>, With<FunctionEntry>)>,
    players: Query<(&Owner, &GlobalTransform), With<PlayerLabel>>,
    mut status: Query<&mut Text, With<FunctionStatus>>,
    mut fire_events: EventReader<FireRocket>,
    asset_server: Res<AssetServer>,
    mut commands: Commands,
) {
    'main: for event in fire_events.iter() {
        let mut status_text = status.get_single_mut().unwrap();
        let player = event.player_index;

        let fx = function_x
            .iter()
            .find_map(|(owner, textbox)| (owner.0 == player).then(|| &textbox.text))
            .unwrap();
        let fy = function_y
            .iter()
            .find_map(|(owner, textbox)| (owner.0 == player).then(|| &textbox.text))
            .unwrap();
        let assigns = assigns
            .iter()
            .find_map(|(owner, textbox)| (owner.0 == player).then(|| &textbox.text))
            .unwrap();

        let (assigns, var_map) = match FunctionParser::parse(Rule::assigns, assigns) {
            Ok(mut pairs) => {
                let assign_pairs = pairs.next().unwrap().into_inner();
                match AssignVec::from_pairs(assign_pairs) {
                    Ok(assigns) => assigns,
                    Err(error) => {
                        set_status_text(
                            &mut *status_text,
                            Some(ParseError::new(error, "'where'".into(), true)),
                        );
                        continue 'main;
                    }
                }
            }

            Err(error) => {
                set_status_text(
                    &mut *status_text,
                    Some(ParseError::new(error, "'where'".into(), true)),
                );
                continue 'main;
            }
        };

        let mut funcs = Vec::with_capacity(2);

        for (axis, func) in [("x", fx), ("y", fy)] {
            match FunctionParser::parse(Rule::func, func) {
                Ok(mut pairs) => {
                    let func = pairs.next().unwrap();
                    let expr = func.into_inner().next().unwrap();
                    match Function::from_pair(expr, &var_map) {
                        Ok(f) => funcs.push(f),
                        Err(error) => {
                            set_status_text(
                                &mut *status_text,
                                Some(ParseError::new(error, format!("{}(t)", axis), false)),
                            );
                            continue 'main;
                        }
                    }
                }

                Err(error) => {
                    set_status_text(
                        &mut *status_text,
                        Some(ParseError::new(error, format!("{}(t)", axis), false)),
                    );
                    continue 'main;
                }
            }
        }

        let fy = funcs.pop().unwrap();
        let fx = funcs.pop().unwrap();
        //println!("{:#?}", fy);
        let start_x = fx.eval(0.0, &assigns) as f32;
        let start_y = fy.eval(0.0, &assigns) as f32;

        let parametric = match Parametric::try_new(fx, fy, assigns) {
            Ok(p) => p,
            Err(error) => {
                set_status_text(
                    &mut *status_text,
                    Some(ParseError::new(error, "'where'".into(), true)),
                );
                continue 'main;
            }
        };

        set_status_text(&mut *status_text, None);

        let transform = players
            .iter()
            .find_map(|(owner, transform)| (owner.0 == player).then(|| transform))
            .unwrap();

        let scale = 0.4;
        commands
            .spawn_bundle(Svg2dBundle {
                svg: asset_server.load(&format!("rocket{}.svg", player + 1)),
                transform: Transform::from(*transform).with_scale([scale; 3].into()),
                ..Default::default()
            })
            .insert(parametric)
            .insert(Offset(
                transform.translation.xy() - Vec2::new(start_x, start_y),
            ))
            .insert(Rocket)
            .insert(Timer::new(Duration::from_secs_f64(ROCKET_TIME), false))
            .insert(Owner(player))
            .insert(PrevPosition(transform.translation.xy()))
            .insert_bundle(RigidBodyBundle {
                body_type: RigidBodyType::KinematicPositionBased.into(),
                position: transform.translation.xy().extend(0.0).into(),
                // kinematic-static CCD doesn't work
                ..Default::default()
            })
            .with_children(|body| {
                body.spawn_bundle(ColliderBundle {
                    shape: ColliderShape::ball(scale / 2.0).into(),
                    collider_type: ColliderType::Solid.into(),
                    position: Vec2::ZERO.into(),
                    flags: ColliderFlags {
                        collision_groups: InteractionGroups::new(
                            CollisionGroups::ROCKET.bits(),
                            CollisionGroups::ROCKET_CAST.bits(),
                        ),
                        active_collision_types: ActiveCollisionTypes::KINEMATIC_KINEMATIC
                            | ActiveCollisionTypes::KINEMATIC_STATIC,
                        ..Default::default()
                    }
                    .into(),
                    ..Default::default()
                });
            });
    }
}

pub fn move_rockets(
    mut rockets: Query<
        (
            &mut Transform,
            &mut RigidBodyPositionComponent,
            &Offset,
            &Parametric,
            &mut Timer,
            Entity,
        ),
        With<Rocket>,
    >,
    mut commands: Commands,
    time: Res<Time>,
) {
    for (mut transform, mut body_position, offset, parametric, mut timer, entity) in
        rockets.iter_mut()
    {
        timer.tick(time.delta());
        if timer.finished() {
            commands.entity(entity).despawn_recursive();
        }

        let next_pos = parametric.eval(timer.percent() as f64) + offset.0;
        let curr_pos = transform.translation.xy();
        if next_pos - curr_pos != Vec2::ZERO {
            transform.rotation =
                Quat::from_rotation_arc_2d(Vec2::X, (next_pos - curr_pos).normalize());
        }
        transform.translation = next_pos.extend(z::ROCKET);
        body_position.0.next_position =
            Isometry::new(next_pos.into(), transform.rotation.to_axis_angle().1);
    }
}
