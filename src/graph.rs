use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_svg::prelude::Svg2dBundle;
use fxhash::FxHashMap;
use once_cell::sync::Lazy;
use pest::{
    iterators::{Pair, Pairs},
    Parser, error::{Error, ErrorVariant},
};
use std::{iter, time::Duration};

use crate::{
    ui::{FunctionX, FunctionY, Textbox},
    Owner, Player,
};

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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OpType {
    Normal,
    Inverse,
}

#[derive(Clone, Debug)]
pub enum Function {
    Var,
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
    fn from_2_op_sequence(
        pair: Pair<Rule>,
        variant: impl Fn(Vec<(Function, OpType)>) -> Self,
        normal_sign: &str,
    ) -> Result<Self, Error<Rule>> {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        if inner.peek().is_some() {
            let pair_vec = inner.collect::<Vec<_>>();
            Ok(variant(
                iter::once(Self::from_pair(first).map(|f| (f, OpType::Normal)))
                    .chain(pair_vec.chunks(2).map(|pairs| {
                        let op_type = &pairs[0];
                        let expr = pairs[1].clone();
                        Self::from_pair(expr).map(|f| (
                            f,
                            if op_type.as_str() == normal_sign {
                                OpType::Normal
                            } else {
                                OpType::Inverse
                            },
                        ))
                    }))
                    .flatten()
                    .collect(),
            ))
        } else {
            Self::from_pair(first)
        }
    }

    fn from_op_sequence(pair: Pair<Rule>, variant: impl Fn(Vec<Function>) -> Self) -> Result<Self, Error<Rule>> {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        if inner.peek().is_some() {
            Ok(variant(
                iter::once(first).chain(inner).map(Self::from_pair).flatten().collect()
            ))
        } else {
            Self::from_pair(first)
        }
    }

    fn from_pair(pair: Pair<Rule>) -> Result<Self, Error<Rule>> {
        match pair.as_rule() {
            Rule::expr => Self::from_pair(pair.into_inner().next().unwrap()),
            Rule::add => Self::from_2_op_sequence(pair, Self::Add, "+"),
            Rule::mul => Self::from_2_op_sequence(pair, Self::Mul, "*"),
            Rule::neg => {
                let negate = pair.as_str().starts_with("-");
                let expr = Self::from_pair(pair.into_inner().next().unwrap())?;
                if negate {
                    Ok(Self::Neg(Box::new(expr)))
                } else {
                    Ok(expr)
                }
            }
            Rule::exp => Self::from_op_sequence(pair, Self::Exp),
            Rule::call_1 => {
                let mut pairs = pair.into_inner();
                let func = pairs.next().unwrap();
                let expr = pairs.next().unwrap();
                if let Some(call) = CALL_1_FN_MAP.get(func.as_str()) {
                    Ok(Self::Call1(*call, Box::new(Self::from_pair(expr)?)))
                } else {
                    Err(Error::new_from_span(ErrorVariant::CustomError {
                        message: format!("unknown unary function: {}", func.as_str())
                    }, func.as_span()))
                }
            }
            Rule::call_2 => {
                let mut pairs = pair.into_inner();
                let func = pairs.next().unwrap();
                let expr1 = pairs.next().unwrap();
                let expr2 = pairs.next().unwrap();
                if let Some(call) = CALL_2_FN_MAP.get(func.as_str()) {
                    Ok(Self::Call2(*call, Box::new([Self::from_pair(expr1)?, Self::from_pair(expr2)?])))
                } else {
                    Err(Error::new_from_span(ErrorVariant::CustomError {
                        message: format!("unknown binary function: {}", func.as_str())
                    }, func.as_span()))
                }
            }
            Rule::primary => Self::from_pair(pair.into_inner().next().unwrap()),
            Rule::primitive => Self::from_pair(pair.into_inner().next().unwrap()),
            Rule::var => Ok(Self::Var),
            Rule::constant => Ok(Self::Const(str::parse(pair.as_str()).unwrap())),

            _ => unreachable!(),
        }
    }

    fn eval(&self, t: f64) -> f64 {
        match self {
            Self::Var => t,
            Self::Const(c) => *c,
            Self::Add(fs) => fs.iter().fold(0.0, |acc, (f, op)| {
                if *op == OpType::Normal {
                    acc + f.eval(t)
                } else {
                    acc - f.eval(t)
                }
            }),
            Self::Mul(fs) => fs.iter().fold(1.0, |acc, (f, op)| {
                if *op == OpType::Normal {
                    acc * f.eval(t)
                } else {
                    acc / f.eval(t)
                }
            }),
            Self::Exp(fs) => fs.iter().rev().fold(1.0, |acc, f|
                f.eval(t).powf(acc)
            ),
            Self::Neg(f) => -f.eval(t),
            Self::Call1(call, f) => call.call(f.eval(t)),
            Self::Call2(call, fs) => call.call(fs[0].eval(t), fs[1].eval(t)),
        }
    }
}

#[derive(Clone, Debug, Component)]
pub struct Parametric {
    pub x: Function,
    pub y: Function,
}

#[derive(Clone, Debug)]
/// Event that says that some player should fire a rocket from their position
pub struct FireRocket {
    pub player_index: u32,
}

pub fn handle_fire_events(
    function_x: Query<(&Owner, &Textbox), With<FunctionX>>,
    function_y: Query<(&Owner, &Textbox), With<FunctionY>>,
    players: Query<(&Owner, &GlobalTransform), With<Player>>,
    mut fire_events: EventReader<FireRocket>,
    asset_server: Res<AssetServer>,
    mut commands: Commands,
) {
    'main: for event in fire_events.iter() {
        let player = event.player_index;

        let fx = function_x
            .iter()
            .find_map(|(owner, textbox)| (owner.0 == player).then(|| &textbox.0))
            .unwrap();
        let fy = function_y
            .iter()
            .find_map(|(owner, textbox)| (owner.0 == player).then(|| &textbox.0))
            .unwrap();
        let transform = players
            .iter()
            .find_map(|(owner, transform)| (owner.0 == player).then(|| transform))
            .unwrap();

        let mut funcs = Vec::with_capacity(2);

        for (axis, func) in [("x", fx), ("y", fy)] {
            match FunctionParser::parse(Rule::func, func) {
                Ok(mut pairs) => {
                    let func = pairs.next().unwrap();
                    let expr = func.into_inner().next().unwrap();
                    match Function::from_pair(expr) {
                        Ok(f) => funcs.push(f),
                        Err(error) => {
                            log::warn!("Error parsing {}(t): {}", axis, error);
                            continue 'main;
                        }
                    }
                }

                Err(error) => {
                    log::warn!("Error parsing {}(t): {}", axis, error);
                    continue 'main;
                }
            }
        }

        let fy = funcs.pop().unwrap();
        let fx = funcs.pop().unwrap();
        let start_x = fx.eval(0.0) as f32;
        let start_y = fy.eval(0.0) as f32;

        commands
            .spawn_bundle(Svg2dBundle {
                svg: asset_server.load(&format!("rocket{}.svg", player + 1)),
                transform: (*transform).into(),
                ..Default::default()
            })
            .insert(Parametric { x: fx, y: fy })
            .insert(Offset(
                transform.translation.xy() - Vec2::new(start_x, start_y),
            ))
            .insert(Rocket)
            .insert(Timer::new(Duration::from_secs_f64(ROCKET_TIME), false))
            .insert(Owner(player));
    }
}

pub fn move_rockets(
    mut rockets: Query<
        (
            &Owner,
            &mut Transform,
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
    for (owner, mut transform, offset, parametric, mut timer, entity) in rockets.iter_mut() {
        timer.tick(time.delta());
        if timer.finished() {
            commands.entity(entity).despawn();
        }

        let x = parametric.x.eval(timer.percent() as f64);
        let y = parametric.y.eval(timer.percent() as f64);
        let next_pos = Vec2::new(x as f32, y as f32) + offset.0;
        let curr_pos = transform.translation.xy();
        transform.rotation = Quat::from_rotation_arc_2d(Vec2::X, (next_pos - curr_pos).normalize());
        transform.translation = next_pos.extend(3.0);
    }
}
