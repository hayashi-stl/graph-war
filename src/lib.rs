#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

#[macro_use]
extern crate pest_derive;

pub mod collision;
pub mod graph;
pub mod random;
pub mod time;
pub mod ui;

use bevy::{
    ecs::system::EntityCommands,
    math::{Mat2, Vec3Swizzles},
    prelude::*,
    render::camera::ScalingMode,
};
use bevy_egui::EguiPlugin;
use bevy_rapier2d::{physics::PhysicsSystems, prelude::*};
use graph::{Parametric, Graph};
use rand::{prelude::Distribution, distributions::Uniform};
use rand_pcg::Pcg64;
use rand::SeedableRng;
#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

use crate::{collision::CollisionGroups, random::RectRegion, time::AdvanceRound};

#[cfg(target_family = "wasm")]
#[macro_export]
macro_rules! console_log {
    ($($t:tt)*) => {
        $crate::log(&format_args!($($t)*).to_string())
    };
}

#[cfg(target_family = "wasm")]
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

/// Owned by some player with the stored player index
#[derive(Copy, Clone, Debug, Component)]
pub struct Owner(pub u32);

/// Labels a player's score
#[derive(Component)]
pub struct Score;

#[derive(Component)]
pub struct PlayerLabel;

/// Info stored per player. This is meant to be contained in a vec
/// for easy access given a player index.
#[derive(Clone, Debug, Default)]
pub struct Player {
    pub num_balls: u32,
    /// Parametric is stored here until rocket gets fired
    pub parametric: Option<Parametric>,
}

#[derive(Component)]
pub struct Ball;

#[derive(Component)]
pub struct Mine;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PlayState {
    Wait,
    /// Enter functions
    Enter,
    /// Fire rockets
    Fire,
}

#[derive(Debug)]
pub struct Game {
    pub order_index: u32,
    pub player_order: Vec<u32>,
    pub inverse_order: Vec<u32>,
    pub scale: f32,
    /// this is 1-indexed to simplify advance_round
    pub round_index: u32,
    pub num_rounds: u32,
}

impl Default for Game {
    fn default() -> Self {
        Self {
            order_index: 0,
            player_order: vec![],
            inverse_order: vec![],
            scale: 4.0,
            round_index: 0,
            num_rounds: 0,
        }
    }
}

impl Game {
    pub fn set_num_players(&mut self, num_players: u32) {
        self.player_order = (0..num_players).collect();
        self.inverse_order = self.player_order.clone();
        self.num_rounds = 1;//18 / num_players.pow(2) * num_players;
    }

    pub fn rotate_players(&mut self) {
        //self.player_order.rotate_left(1);
        //self.inverse_order.rotate_right(1);
    }

    /// Whose turn it is
    pub fn player_turn(&self) -> u32 {
        self.player_order[self.order_index as usize]
    }

    /// When a player goes
    pub fn order_index(&self, player: u32) -> u32 {
        self.inverse_order[player as usize]
    }

    pub fn is_on_last_normal_round(&self) -> bool {
        self.round_index == self.num_rounds
    }

    pub fn is_on_destruction_round(&self) -> bool {
        self.round_index == self.num_rounds + 1
    }
}

pub const ASPECT_RATIO: f32 = 16.0 / 9.0;

#[derive(Clone, Copy, Debug, SystemLabel, PartialEq, Eq, Hash)]
enum Label {
    Setup,
    LoadField,
    DoneButton,
    AdvanceRoundButton,
    CollectItems,
    AdvanceTurn,
    MovePlayers,
    MoveRockets,
    SeedRng,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, StageLabel)]
enum Stage {
    AdvanceTimers,
}

pub fn run() {
    //#[cfg(not(target_family = "wasm"))]
    //flexi_logger::Logger::try_with_str("info").unwrap()
    //    .format(flexi_logger::opt_format)
    //    .start().unwrap();
    //#[cfg(target_family = "wasm")]
    //wasm_logger::init(wasm_logger::Config::default());

    App::new()
        .insert_resource(Msaa { samples: 4 })
        .insert_resource(ui::IdLender::default())
        .insert_resource(Pcg64::new(0, 0))
        .insert_resource(vec![Player::default(); 0])
        .insert_resource(Game::default())
        .insert_resource(ui::TextboxesEditable(true))
        .insert_resource(ui::ButtonsEnabled(true))
        .add_state(PlayState::Wait)
        .add_plugins(DefaultPlugins)
        //.add_plugin(WorldInspectorPlugin::new())
        //.register_inspectable::<ui::EguiId>()
        .add_plugin(EguiPlugin)
        .add_plugin(RapierPhysicsPlugin::<NoUserData>::default())
        .add_event::<graph::SendFunctions>()
        .add_event::<time::AdvanceTurn>()
        .add_event::<time::AdvanceRound>()
        .add_stage_before(
            CoreStage::PreUpdate,
            Stage::AdvanceTimers,
            SystemStage::single_threaded(),
        )
        .add_startup_system(seed_rng.label(Label::SeedRng))
        .add_startup_system(ui::setup_egui.label(Label::Setup).after(Label::SeedRng))
        .add_startup_system(ui::load_ui.label(Label::Setup).after(Label::SeedRng))
        .add_startup_system(load_field.label(Label::LoadField).after(Label::Setup))
        .add_startup_system(ui::advance_round.after(Label::LoadField))
        .add_system_to_stage(Stage::AdvanceTimers, time::advance_timers)
        .add_system_to_stage(CoreStage::PreUpdate, ui::update_buttons)
        .add_system_to_stage(CoreStage::PreUpdate, collision::update_prev_positions)
        .add_system(resize)
        .add_system(ui::update_textboxes)
        .add_system_set(
            SystemSet::on_enter(PlayState::Enter)
                .with_system(move_players.label(Label::MovePlayers))
                .with_system(init_enter_functions.after(Label::MovePlayers)),
        )
        .add_system_set(
            SystemSet::on_update(PlayState::Enter)
                .before(PhysicsSystems::StepWorld)
                .with_system(ui::update_done_button.label(Label::DoneButton))
                .with_system(graph::send_functions.after(Label::DoneButton)),
        )
        .add_system_set(
            SystemSet::on_enter(PlayState::Fire)
                .after(Label::AdvanceTurn)
                .with_system(graph::fire_rockets),
        )
        .add_system_set(
            SystemSet::on_update(PlayState::Fire)
                .before(PhysicsSystems::StepWorld)
                .with_system(graph::move_rockets.label(Label::MoveRockets))
                .with_system(ui::update_next_round_button.label(Label::AdvanceRoundButton))
                .with_system(ui::advance_round.after(Label::AdvanceRoundButton)),
        )
        .add_system_set(
            SystemSet::on_update(PlayState::Fire)
                .after(PhysicsSystems::StepWorld)
                .with_system(collision::handle_collisions)
                .with_system(collision::collect_balls.label(Label::CollectItems))
                .with_system(graph::graph_functions.after(Label::CollectItems))
                .with_system(update_scores.after(Label::CollectItems)),
        )
        .add_system(
            ui::advance_turn
                .label(Label::AdvanceTurn)
                .after(Label::CollectItems),
        )
        .add_system_to_stage(CoreStage::PostUpdate, ui::assign_egui_ids)
        .add_system_to_stage(CoreStage::PostUpdate, ui::give_back_egui_ids)
        .run();
}

/// Z-indexes
pub mod z {
    pub const GRID: f32 = 0.0;
    pub const GRID_TEXT: f32 = 1.0;
    pub const GRAPH: f32 = 2.0;
    pub const PLAYER: f32 = 2.0;
    pub const BALL: f32 = 2.0;
    pub const MINE: f32 = 3.0;
    pub const ROCKET: f32 = 4.0;
    pub const SCORE: f32 = 5.0;
}

fn seed_rng(
    mut pcg: ResMut<Pcg64>,
) {
    let mut rng = rand::thread_rng();
    let mut seed = [0u8; 32];
    seed[0..16].copy_from_slice(&Uniform::from(0..=u128::MAX).sample(&mut rng).to_le_bytes());
    seed[16..32].copy_from_slice(&Uniform::from(0..=u128::MAX).sample(&mut rng).to_le_bytes());
    log::info!("RNG seed: {:02x?}", seed);
    *pcg = Pcg64::from_seed(seed);
}

pub fn load_field(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut players: ResMut<Vec<Player>>,
    mut game: ResMut<Game>,
    mut advance_round_events: EventWriter<AdvanceRound>,
) {
    const AXIS_THICKNESS: f32 = 0.04;
    const GRID_THICKNESS: f32 = 0.02;
    let cell_size = 1.0;
    let scale = game.scale;

    let mut camera = OrthographicCameraBundle::new_2d();
    camera.orthographic_projection.scaling_mode = ScalingMode::None;
    camera.orthographic_projection.scale = scale;
    commands.spawn_bundle(camera);

    let axis = Sprite {
        color: Color::rgb(0.0, 0.0, 0.0),
        custom_size: Some(Vec2::new(2.0 * scale, AXIS_THICKNESS)),
        ..Default::default()
    };
    let rot_90 =
        Transform::from_matrix(Mat4::from_mat3(Mat3::from_mat2(Mat2::from_cols_array(&[
            0.0, 1.0, -1.0, 0.0,
        ]))));

    // Axes
    commands.spawn_bundle(SpriteBundle {
        sprite: axis.clone(),
        ..Default::default()
    });
    commands.spawn_bundle(SpriteBundle {
        sprite: axis,
        transform: rot_90,
        ..Default::default()
    });

    // Grid
    let grid_line = Sprite {
        color: Color::rgba(0.0, 0.0, 0.0, 0.25),
        custom_size: Some(Vec2::new(2.0 * scale, GRID_THICKNESS)),
        ..Default::default()
    };

    let label_style = TextStyle {
        font: asset_server.load("NotoMono-Regular.ttf"),
        font_size: 0.0, // will be filled in by RelativeTextSize
        color: Color::BLACK,
    };
    let label_alignment_x = TextAlignment {
        vertical: VerticalAlign::Top,
        horizontal: HorizontalAlign::Center,
    };
    let label_alignment_y = TextAlignment {
        vertical: VerticalAlign::Center,
        horizontal: HorizontalAlign::Right,
    };

    for dist in (1..(scale / cell_size) as i32).map(|i| i as f32 * cell_size) {
        let transforms = [
            Transform::from_xyz(0.0, dist, z::GRID),
            Transform::from_xyz(0.0, -dist, z::GRID),
            rot_90.mul_transform(Transform::from_xyz(0.0, dist, z::GRID)),
            rot_90.mul_transform(Transform::from_xyz(0.0, -dist, z::GRID)),
        ];
        for transform in transforms {
            commands.spawn_bundle(SpriteBundle {
                sprite: grid_line.clone(),
                transform,
                ..Default::default()
            });
        }

        for dir in [1.0, -1.0] {
            let dist = dist * dir;
            let pairs = [
                (
                    Transform::from_xyz(dist, -0.05, z::GRID_TEXT),
                    label_alignment_x,
                ),
                (
                    Transform::from_xyz(-0.05, dist, z::GRID_TEXT),
                    label_alignment_y,
                ),
            ];
            for (transform, alignment) in pairs {
                commands
                    .spawn_bundle(Text2dBundle {
                        text: Text::with_section(
                            format!("{}", dist),
                            label_style.clone(),
                            alignment,
                        ),
                        transform,
                        ..Default::default()
                    })
                    .insert(RelativeTextSize(0.2));
            }
        }
    }

    let score_style = TextStyle {
        font: asset_server.load("NotoMono-Regular.ttf"),
        font_size: 0.0, // will be filled in by RelativeTextSize
        color: Color::BLACK,
    };
    let score_alignment = TextAlignment {
        vertical: VerticalAlign::Center,
        horizontal: HorizontalAlign::Center,
    };

    let positions = [
        [-3.0, 3.0, z::PLAYER],
        [-3.0, -3.0, z::PLAYER],
        [3.0, 3.0, z::PLAYER],
        [3.0, -3.0, z::PLAYER],
    ];
    game.set_num_players(positions.len() as u32);

    for (i, pos) in positions.into_iter().enumerate() {
        // Player icon
        commands
            .spawn_bundle(SpriteBundle {
                sprite: Sprite {
                    custom_size: Some(Vec2::ONE),
                    ..Default::default()
                },
                texture: asset_server.load(&format!("player{}.png", i + 1)),
                transform: Transform::from_translation(Vec3::from(pos))
                    .with_scale(Vec3::from([0.4; 3])),
                ..Default::default()
            })
            .insert(Owner(i as u32))
            .insert(PlayerLabel);

        // Score
        commands
            .spawn_bundle(Text2dBundle {
                text: Text::with_section("0", score_style.clone(), score_alignment),
                transform: Transform::from_translation(
                    (Vec2::new(pos[0], pos[1]) * 3.4 / 3.0).extend(z::SCORE),
                ),
                ..Default::default()
            })
            .insert(RelativeTextSize(0.4))
            .insert(Owner(i as u32))
            .insert(Score);

        players.push(Player::default());
    }

    advance_round_events.send(AdvanceRound);
}

fn move_players(
    game: Res<Game>,
    mut player_comps: Query<(&Owner, &mut Transform), With<PlayerLabel>>,
    mut scores: Query<(&Owner, &mut Transform), (With<Score>, Without<PlayerLabel>)>,
) {
    let positions = [
        [-3.0, 3.0, z::PLAYER],
        [-3.0, -3.0, z::PLAYER],
        [3.0, 3.0, z::PLAYER],
        [3.0, -3.0, z::PLAYER],
    ];
    for (owner, mut transform) in player_comps.iter_mut() {
        transform.translation = positions[game.order_index(owner.0) as usize].into();
    }
    for (owner, mut transform) in scores.iter_mut() {
        transform.translation = positions[game.order_index(owner.0) as usize].into();
        transform.translation *= 3.4 / 3.0;
    }
}

enum TexFn<'a> {
    Str(&'a str),
    Between(&'a str, &'a str),
}

impl<'a> TexFn<'a> {
    fn call(&self, param: u32) -> String {
        match self {
            Self::Str(s) => (*s).into(),
            Self::Between(s, t) => format!("{}{}{}", s, param, t)
        }
    }
}

struct ItemParams {
    texture: TexFn<'static>,
    scale_multiplier: f32,
    interaction_layers: CollisionGroups,
}

const ITEM_BALL: ItemParams = ItemParams {
    texture: TexFn::Str("ball.png"),
    scale_multiplier: 1.375,
    interaction_layers: CollisionGroups::BALL,
};

const ITEM_MINE: ItemParams = ItemParams {
    texture: TexFn::Str("mine.png"),
    scale_multiplier: 1.375,
    interaction_layers: CollisionGroups::MINE,
};

const ITEM_PLAYER_BALL: ItemParams = ItemParams {
    texture: TexFn::Between("player", ".png"),
    scale_multiplier: 1.0,
    interaction_layers: CollisionGroups::BALL,
};

fn init_enter_functions(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut rng: ResMut<Pcg64>,
    game: Res<Game>,
    players: Res<Vec<Player>>,
    items: Query<Entity, Or<(With<Ball>, With<Mine>, With<Graph>)>>,
) {
    for entity in items.iter() {
        commands.entity(entity).despawn_recursive();
    }

    #[rustfmt::skip]
    let item_distribution = RectRegion::new(
        &[
            Rect { left: -0.875, right: -0.5,   bottom: -0.5,   top:  0.5,   },
            Rect { left:  0.5,   right:  0.875, bottom: -0.5,   top:  0.5,   },
            Rect { left: -0.5,   right:  0.5,   bottom: -0.875, top: -0.5,   },
            Rect { left: -0.5,   right:  0.5,   bottom:  0.5,   top:  0.875, },
            Rect { left: -0.5,   right:  0.5,   bottom: -0.5,   top:  0.5,   },
        ],
        game.scale,
    );

    fn spawn_item<'a, 'w, 's>(
        commands: &'a mut Commands<'w, 's>,
        asset_server: &Res<AssetServer>,
        point: Vec3,
        item_params: &ItemParams,
        param: u32,
    ) -> EntityCommands<'w, 's, 'a> {
        let scale = 0.3;

        let mut entity_commands = commands.spawn_bundle(SpriteBundle {
            sprite: Sprite {
                custom_size: Some(Vec2::ONE),
                ..Default::default()
            },
            texture: asset_server.load(&item_params.texture.call(param)),
            transform: Transform::from_translation(point)
                .with_scale(Vec3::from([scale * item_params.scale_multiplier; 3])),
            ..Default::default()
        });
        entity_commands
            .insert_bundle(RigidBodyBundle {
                body_type: RigidBodyType::Static.into(),
                position: point.xy().extend(0.0).into(),
                ..Default::default()
            })
            .with_children(|body| {
                body.spawn_bundle(ColliderBundle {
                    shape: ColliderShape::ball(scale / 2.0).into(),
                    collider_type: ColliderType::Sensor.into(),
                    position: Vec2::ZERO.into(),
                    flags: ColliderFlags {
                        collision_groups: InteractionGroups::new(
                            item_params.interaction_layers.bits(),
                            CollisionGroups::ROCKET_CAST.bits(),
                        ),
                        ..Default::default()
                    }
                    .into(),
                    ..Default::default()
                });
            });
        entity_commands
    }

    if game.is_on_destruction_round() {
        for (i, player) in players.iter().enumerate() {
            let points = (&item_distribution).sample_iter(&mut *rng);
            for point in points.take(player.num_balls as usize) {
                spawn_item(&mut commands, &asset_server, point.extend(z::BALL), &ITEM_PLAYER_BALL, i as u32 + 1)
                    .insert(Owner(i as u32))
                    .insert(Ball);
            }
        }
    } else {
        let points = (&item_distribution).sample_iter(&mut *rng);
        for point in points.take(85) {
            spawn_item(&mut commands, &asset_server, point.extend(z::BALL), &ITEM_BALL, 0)
                .insert(Ball);
        }
        let points = (&item_distribution).sample_iter(&mut *rng);
        for point in points.take(15) {
            spawn_item(&mut commands, &asset_server, point.extend(z::MINE), &ITEM_MINE, 0)
                .insert(Mine);
        }
    }
}

fn update_scores(mut scores: Query<(&mut Text, &Owner), With<Score>>, players: Res<Vec<Player>>) {
    for (mut text, player_index) in scores.iter_mut() {
        text.sections[0].value = players[player_index.0 as usize].num_balls.to_string();
    }
}

/// Text size relative to camera
#[derive(Component)]
pub struct RelativeTextSize(pub f32);

//fn resized(
//    mut resize_events: EventReader<WindowResized>,
//    new_cameras: Query<&OrthographicProjection, Added<OrthographicProjection>>,
//) -> ShouldRun {
//    if resize_events.iter().next().is_some() || !new_cameras.is_empty() {
//        ShouldRun::Yes
//    } else {
//        ShouldRun::No
//    }
//}

fn resize(
    mut query: Query<(&mut Text, &mut Transform, &RelativeTextSize, Without<Node>)>,
    mut graph_node: Query<(&mut Style, With<ui::GraphNode>)>,
    mut camera: Query<&mut OrthographicProjection, Without<ui::UiCamera>>,
    windows: Res<Windows>,
) {
    let width = windows.get_primary().unwrap().width() as f32;
    let height = windows.get_primary().unwrap().height() as f32;
    let aspect_ratio = width / height;
    //windows.get_primary_mut().unwrap().set_resolution(aspect_ratio * 720.0, 720.0);
    //windows.get_primary_mut().unwrap().set_scale_factor_override(Some(height as f64 / 720.0));

    for (mut style, _) in graph_node.iter_mut() {
        style.flex_basis = Val::Px(height);
    }

    let mut camera = if let Ok(camera) = camera.get_single_mut() {
        camera
    } else {
        return;
    };

    let scale = camera.scale;
    camera.left = 1.0 - 2.0 * aspect_ratio;
    camera.right = 1.0;
    camera.top = 1.0;
    camera.bottom = -1.0;

    for (mut text, mut transform, size, _) in query.iter_mut() {
        for section in text.sections.iter_mut() {
            section.style.font_size = size.0 * height / (2.0 * scale);
        }
        transform.scale = Vec3::from([2.0 * scale / height; 3]);
    }
}

#[cfg(target_family = "wasm")]
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    run();
    Ok(())
}
