use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_svg::prelude::Svg2dBundle;
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use std::{iter, time::Duration};

use crate::{
    ui::{FunctionX, FunctionY, Textbox},
    Owner, Player,
};

#[derive(Parser)]
#[grammar = "function.pest"]
pub struct FunctionParser;

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
    ) -> Self {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        if inner.peek().is_some() {
            let pair_vec = inner.collect::<Vec<_>>();
            variant(
                iter::once((Self::from_pair(first), OpType::Normal))
                    .chain(pair_vec.chunks(2).map(|pairs| {
                        let op_type = &pairs[0];
                        let func = pairs[1].clone();
                        (
                            Self::from_pair(func),
                            if op_type.as_str() == normal_sign {
                                OpType::Normal
                            } else {
                                OpType::Inverse
                            },
                        )
                    }))
                    .collect(),
            )
        } else {
            Self::from_pair(first)
        }
    }

    fn from_op_sequence(pair: Pair<Rule>, variant: impl Fn(Vec<Function>) -> Self) -> Self {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        if inner.peek().is_some() {
            variant(
                iter::once(first).chain(inner).map(Self::from_pair).collect()
            )
        } else {
            Self::from_pair(first)
        }
    }

    fn from_pair(pair: Pair<Rule>) -> Self {
        match pair.as_rule() {
            Rule::expr => Self::from_pair(pair.into_inner().next().unwrap()),
            Rule::add => Self::from_2_op_sequence(pair, Self::Add, "+"),
            Rule::mul => Self::from_2_op_sequence(pair, Self::Mul, "*"),
            Rule::neg => {
                let negate = pair.as_str().starts_with("-");
                let func = Self::from_pair(pair.into_inner().next().unwrap());
                if negate {
                    Self::Neg(Box::new(func))
                } else {
                    func
                }
            }
            Rule::exp => Self::from_op_sequence(pair, Self::Exp),
            Rule::primary => Self::from_pair(pair.into_inner().next().unwrap()),
            Rule::var => Self::Var,
            Rule::constant => Self::Const(str::parse(pair.as_str()).unwrap()),

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
                    funcs.push(Function::from_pair(expr));
                }

                Err(error) => {
                    log::warn!("Error parsing {}(t): {}", axis, error);
                    continue 'main;
                }
            }
        }

        let fy = funcs.pop().unwrap();
        let fx = funcs.pop().unwrap();
        log::info!("{:#?}", fy);
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
