#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

#[macro_use]
extern crate pest_derive;

pub mod asset;
pub mod collision;
pub mod graph;
pub mod random;
pub mod time;
pub mod ui;

use bevy::{
    ecs::{schedule::ShouldRun, system::EntityCommands},
    math::{Mat2, Vec3Swizzles},
    prelude::*,
    render::camera::ScalingMode,
};
use bevy_egui::EguiPlugin;
use bevy_rapier2d::{physics::PhysicsSystems, prelude::*};
use graph::{Graph, Parametric, Rocket};
use once_cell::sync::Lazy;
use rand::SeedableRng;
use rand::{distributions::Uniform, prelude::Distribution};
use rand_pcg::Pcg64;
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

/// Labels the entire field, including the graph, players, items, etc.
#[derive(Clone, Debug, Component, Default)]
pub struct Field;

#[derive(Clone, Debug, Default, Bundle)]
pub struct FieldBundle {
    pub field: Field,
    pub transform: Transform,
    pub global_transform: GlobalTransform,
}

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

#[derive(Component)]
pub struct WinnerBox;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PlayState {
    Menu,
    Load,
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
        self.order_index = 0;
        self.player_order = (0..num_players).collect();
        self.inverse_order = self.player_order.clone();
        self.round_index = 0;
        self.num_rounds = 1; //18 / num_players.pow(2) * num_players;
    }

    pub fn num_players(&self) -> u32 {
        self.player_order.len() as u32
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
        .insert_resource(vec![] as Vec<Player>)
        .insert_resource(Game::default())
        .insert_resource(ui::TextboxesEditable(true))
        .insert_resource(ui::ButtonsEnabled(true))
        .insert_resource(PrevWindowSize([0.0, 0.0]))
        .insert_resource(vec![] as Vec<HandleUntyped>)
        .add_state(PlayState::Menu)
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
        .add_startup_system(asset::load_assets.label(Label::SeedRng))
        .add_startup_system(ui::setup_egui.label(Label::Setup).after(Label::SeedRng))
        .add_startup_system(ui::load_ui.label(Label::Setup).after(Label::SeedRng))
        .add_system_to_stage(Stage::AdvanceTimers, time::advance_timers)
        .add_system_to_stage(CoreStage::PreUpdate, ui::update_buttons)
        .add_system_to_stage(CoreStage::PreUpdate, collision::update_prev_positions)
        .add_system(resize.with_run_criteria(resized))
        .add_system(ui::update_textboxes)
        .add_system_set(SystemSet::on_enter(PlayState::Menu).with_system(ui::show_menu))
        .add_system_set(SystemSet::on_update(PlayState::Menu).with_system(ui::update_play_button))
        .add_system_set(
            SystemSet::on_enter(PlayState::Load)
                .with_system(load_field.label(Label::LoadField))
                .with_system(ui::advance_round.after(Label::LoadField)),
        )
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
        .add_system_set(SystemSet::on_update(PlayState::Fire).with_system(show_winner))
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
        .add_system(ui::advance_turn.label(Label::AdvanceTurn).after(Label::CollectItems))
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
    pub const WINNER_BOX: f32 = 6.0;
    pub const WINNER: f32 = 6.0;
}

/// Configuration for a field, containing player positions
/// and spots where items can go
struct FieldConfig {
    positions: Vec<Vec2>,
    item_region: RectRegion,
}

#[rustfmt::skip]
static FIELD_CONFIGS: Lazy<[FieldConfig; 5]> = Lazy::new(|| [
    FieldConfig {
        positions: vec![],
        item_region: RectRegion::new(&[])
    },

    // 1 player is invalid for now at least
    FieldConfig {
        positions: vec![],
        item_region: RectRegion::new(&[])
    },

    FieldConfig {
        positions: vec![
            [-0.75,  0.75].into(),
            [ 0.75, -0.75].into(),
        ],
        item_region: RectRegion::new(&[
            Rect { left: -0.875, right:  0.5,   bottom: -0.875, top: -0.5,   },
            Rect { left: -0.5,   right:  0.875, bottom:  0.5,   top:  0.875, },
            Rect { left: -0.875, right:  0.875, bottom: -0.5,   top:  0.5,   },
        ])
    },

    FieldConfig {
        positions: vec![
            [-0.75,  0.75].into(),
            [-0.75, -0.75].into(),
            [ 0.75,  0.0 ].into(),
        ],
        item_region: RectRegion::new(&[
            Rect { left: -0.875, right: -0.5,   bottom: -0.5,   top:  0.5,   },
            Rect { left: -0.5,   right:  0.5,   bottom: -0.875, top:  0.875, },
        ])
    },

    FieldConfig {
        positions: vec![
            [-0.75,  0.75].into(),
            [-0.75, -0.75].into(),
            [ 0.75,  0.75].into(),
            [ 0.75, -0.75].into(),
        ],
        item_region: RectRegion::new(&[
            Rect { left: -0.875, right: -0.5,   bottom: -0.5,   top:  0.5,   },
            Rect { left:  0.5,   right:  0.875, bottom: -0.5,   top:  0.5,   },
            Rect { left: -0.5,   right:  0.5,   bottom: -0.875, top: -0.5,   },
            Rect { left: -0.5,   right:  0.5,   bottom:  0.5,   top:  0.875, },
            Rect { left: -0.5,   right:  0.5,   bottom: -0.5,   top:  0.5,   },
        ])
    },
]);

fn seed_rng(mut pcg: ResMut<Pcg64>) {
    let mut rng = rand::thread_rng();
    let mut seed = [0u8; 32];
    seed[0..16].copy_from_slice(&Uniform::from(0..=u128::MAX).sample(&mut rng).to_le_bytes());
    seed[16..32].copy_from_slice(&Uniform::from(0..=u128::MAX).sample(&mut rng).to_le_bytes());
    log::info!("RNG seed: {:02x?}", seed);
    *pcg = Pcg64::from_seed(seed);
}

pub fn load_field(
    mut commands: Commands,
    game: ResMut<Game>,
    mut advance_round_events: EventWriter<AdvanceRound>,
    fonts: Res<Assets<Font>>,
    images: Res<Assets<Image>>,
) {
    const AXIS_THICKNESS: f32 = 0.04;
    const GRID_THICKNESS: f32 = 0.02;
    let cell_size = 1.0;
    let scale = game.scale;

    commands.spawn_bundle(FieldBundle::default()).with_children(|node| {
        let mut camera = OrthographicCameraBundle::new_2d();
        camera.orthographic_projection.scaling_mode = ScalingMode::None;
        camera.orthographic_projection.scale = scale;
        node.spawn_bundle(camera);

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
        node.spawn_bundle(SpriteBundle { sprite: axis.clone(), ..Default::default() });
        node.spawn_bundle(SpriteBundle { sprite: axis, transform: rot_90, ..Default::default() });

        // Grid
        let grid_line = Sprite {
            color: Color::rgba(0.0, 0.0, 0.0, 0.25),
            custom_size: Some(Vec2::new(2.0 * scale, GRID_THICKNESS)),
            ..Default::default()
        };

        let label_style =
            TextStyle { font: fonts.get_handle(asset::Font), color: Color::BLACK, font_size: 0.0 };
        let label_alignment_x =
            TextAlignment { vertical: VerticalAlign::Top, horizontal: HorizontalAlign::Center };
        let label_alignment_y =
            TextAlignment { vertical: VerticalAlign::Center, horizontal: HorizontalAlign::Right };

        for dist in (1..(scale / cell_size) as i32).map(|i| i as f32 * cell_size) {
            let transforms = [
                Transform::from_xyz(0.0, dist, z::GRID),
                Transform::from_xyz(0.0, -dist, z::GRID),
                rot_90.mul_transform(Transform::from_xyz(0.0, dist, z::GRID)),
                rot_90.mul_transform(Transform::from_xyz(0.0, -dist, z::GRID)),
            ];
            for transform in transforms {
                node.spawn_bundle(SpriteBundle {
                    sprite: grid_line.clone(),
                    transform,
                    ..Default::default()
                });
            }

            for dir in [1.0, -1.0] {
                let dist = dist * dir;
                let pairs = [
                    (Transform::from_xyz(dist, -0.05, z::GRID_TEXT), label_alignment_x),
                    (Transform::from_xyz(-0.05, dist, z::GRID_TEXT), label_alignment_y),
                ];
                for (transform, alignment) in pairs {
                    node.spawn_bundle(Text2dBundle {
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

        let score_style =
            TextStyle { font: fonts.get_handle(asset::Font), color: Color::BLACK, font_size: 0.0 };
        let score_alignment =
            TextAlignment { vertical: VerticalAlign::Center, horizontal: HorizontalAlign::Center };

        let positions = &FIELD_CONFIGS[game.num_players() as usize].positions;
        for (i, pos) in positions.iter().enumerate() {
            // Player icon
            node.spawn_bundle(SpriteBundle {
                sprite: Sprite { custom_size: Some(Vec2::ONE), ..Default::default() },
                texture: images.get_handle(asset::Player(i as u32)),
                transform: Transform::from_translation((*pos * scale).extend(z::PLAYER))
                    .with_scale(Vec3::from([0.4; 3])),
                ..Default::default()
            })
            .insert(Owner(i as u32))
            .insert(PlayerLabel);

            // Score
            node.spawn_bundle(Text2dBundle {
                text: Text::with_section("0", score_style.clone(), score_alignment),
                transform: Transform::from_translation((*pos * scale * 3.4 / 3.0).extend(z::SCORE)),
                ..Default::default()
            })
            .insert(RelativeTextSize(0.4))
            .insert(Owner(i as u32))
            .insert(Score);
        }
    });

    advance_round_events.send(AdvanceRound);
}

fn move_players(
    game: Res<Game>,
    mut player_comps: Query<(&Owner, &mut Transform), With<PlayerLabel>>,
    mut scores: Query<(&Owner, &mut Transform), (With<Score>, Without<PlayerLabel>)>,
) {
    let positions = &FIELD_CONFIGS[game.num_players() as usize].positions;
    for (owner, mut transform) in player_comps.iter_mut() {
        transform.translation =
            (positions[game.order_index(owner.0) as usize] * game.scale).extend(z::PLAYER);
    }
    for (owner, mut transform) in scores.iter_mut() {
        transform.translation = (positions[game.order_index(owner.0) as usize] * game.scale * 3.4
            / 3.0)
            .extend(z::PLAYER);
    }
}

enum TexFn {
    Asset(asset::Asset),
    AssetU32(fn(u32) -> asset::Asset),
}

impl TexFn {
    fn call(&self, param: u32) -> asset::Asset {
        match self {
            Self::Asset(s) => *s,
            Self::AssetU32(s) => s(param),
        }
    }
}

struct ItemParams {
    color: Color,
    texture: TexFn,
    scale_multiplier: f32,
    interaction_layers: CollisionGroups,
}

const ITEM_BALL: ItemParams = ItemParams {
    color: Color::WHITE,
    texture: TexFn::Asset(asset::Ball),
    scale_multiplier: 1.375,
    interaction_layers: CollisionGroups::BALL,
};

const ITEM_MINE: ItemParams = ItemParams {
    color: Color::WHITE,
    texture: TexFn::Asset(asset::Mine),
    scale_multiplier: 1.375,
    interaction_layers: CollisionGroups::MINE,
};

const ITEM_PLAYER_BALL: ItemParams = ItemParams {
    color: Color::rgb(0.8, 0.8, 0.8),
    texture: TexFn::AssetU32(asset::Player),
    scale_multiplier: 1.0,
    interaction_layers: CollisionGroups::BALL,
};

fn init_enter_functions(
    mut commands: Commands,
    mut rng: ResMut<Pcg64>,
    images: Res<Assets<Image>>,
    game: Res<Game>,
    players: Res<Vec<Player>>,
    items: Query<Entity, Or<(With<Ball>, With<Mine>, With<Graph>)>>,
    field: Query<Entity, With<Field>>,
) {
    for entity in items.iter() {
        commands.entity(entity).despawn_recursive();
    }

    let item_region = &FIELD_CONFIGS[game.num_players() as usize].item_region;
    let item_distribution = item_region.scaled(game.scale);

    fn spawn_item<'a, 'w, 's, 'b>(
        node: &'b mut ChildBuilder<'w, 's, 'a>,
        images: &Res<Assets<Image>>,
        point: Vec3,
        item_params: &ItemParams,
        param: u32,
    ) -> EntityCommands<'w, 's, 'b> {
        let scale = 0.3;

        let mut entity_commands = node.spawn_bundle(SpriteBundle {
            sprite: Sprite {
                color: item_params.color,
                custom_size: Some(Vec2::ONE),
                ..Default::default()
            },
            texture: images.get_handle(item_params.texture.call(param)),
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

    commands.entity(field.single()).with_children(|node| {
        if game.is_on_destruction_round() {
            for (i, player) in players.iter().enumerate() {
                let points = (&item_distribution).sample_iter(&mut *rng);
                for point in points.take(player.num_balls as usize) {
                    spawn_item(
                        node,
                        &images,
                        point.extend(z::BALL),
                        &ITEM_PLAYER_BALL,
                        i as u32,
                    )
                    .insert(Owner(i as u32))
                    .insert(Ball);
                }
            }
        } else {
            let points = (&item_distribution).sample_iter(&mut *rng);
            for point in points.take(85) {
                spawn_item(node, &images, point.extend(z::BALL), &ITEM_BALL, 0).insert(Ball);
            }
            let points = (&item_distribution).sample_iter(&mut *rng);
            for point in points.take(15) {
                spawn_item(node, &images, point.extend(z::MINE), &ITEM_MINE, 0).insert(Mine);
            }
        }
    });
}

fn update_scores(mut scores: Query<(&mut Text, &Owner), With<Score>>, players: Res<Vec<Player>>) {
    for (mut text, player_index) in scores.iter_mut() {
        text.sections[0].value = players[player_index.0 as usize].num_balls.to_string();
    }
}

fn show_winner(
    fonts: Res<Assets<Font>>,
    rockets: Query<&Rocket>,
    game: Res<Game>,
    players: Res<Vec<Player>>,
    winner_box: Query<&WinnerBox>,
    field: Query<Entity, With<Field>>,
    mut commands: Commands,
) {
    if rockets.iter().next().is_some()
        || winner_box.iter().next().is_some()
        || !game.is_on_destruction_round()
    {
        return;
    }

    commands.entity(field.single()).with_children(|node| {
        node.spawn_bundle(SpriteBundle {
            sprite: Sprite {
                color: Color::rgba(0.0, 0.0, 0.0, 0.5),
                custom_size: Some(Vec2::new(1.0, 0.35) * game.scale),
                ..Default::default()
            },
            transform: Transform::from_xyz(0.0, 0.0, z::WINNER_BOX),
            ..Default::default()
        })
        .insert(WinnerBox);

        let max_score = players.iter().map(|p| p.num_balls).max().unwrap();
        let mut winner_text = players
            .iter()
            .enumerate()
            .filter_map(|(i, p)| (p.num_balls == max_score).then(|| format!("P{}, ", i + 1)))
            .collect::<String>();
        winner_text = format!("Winners:\n{}", winner_text);
        winner_text.truncate(winner_text.len() - 2); // Remove final ", "

        node.spawn_bundle(Text2dBundle {
            text: Text::with_section(
                winner_text,
                TextStyle {
                    color: Color::WHITE,
                    font: fonts.get_handle(asset::Font),
                    font_size: 0.0,
                },
                TextAlignment {
                    horizontal: HorizontalAlign::Center,
                    vertical: VerticalAlign::Center,
                },
            ),
            transform: Transform::from_xyz(0.0, 0.0, z::WINNER),
            ..Default::default()
        })
        .insert(RelativeTextSize(0.5));
    });
}

/// Text size relative to camera
#[derive(Component)]
pub struct RelativeTextSize(pub f32);

fn resized(
    prev_height: Res<PrevWindowSize>,
    windows: Res<Windows>,
    added: Query<(), Or<(Added<Node>, Added<Text>, Added<OrthographicProjection>)>>,
) -> ShouldRun {
    let window = windows.get_primary().unwrap();
    if [window.width(), window.height()] != prev_height.0 || added.iter().next().is_some() {
        ShouldRun::Yes
    } else {
        ShouldRun::No
    }
}

struct PrevWindowSize([f32; 2]);

fn resize(
    mut query: Query<(&mut Text, &mut Transform, &RelativeTextSize, Without<Node>)>,
    mut graph_node: Query<(&mut Style, With<ui::GraphNode>)>,
    mut camera: Query<&mut OrthographicProjection, Without<ui::UiCamera>>,
    windows: Res<Windows>,
    mut prev_height: ResMut<PrevWindowSize>,
) {
    let width = windows.get_primary().unwrap().width() as f32;
    let height = windows.get_primary().unwrap().height() as f32;
    let aspect_ratio = width / height;
    prev_height.0 = [width, height];
    //windows.get_primary_mut().unwrap().set_resolution(aspect_ratio * 720.0, 720.0);
    //windows.get_primary_mut().unwrap().set_scale_factor_override(Some(height as f64 / 720.0));

    for (mut style, _) in graph_node.iter_mut() {
        style.flex_basis = Val::Px(height);
    }

    let mut camera = if let Ok(camera) = camera.get_single_mut() { camera } else { return };

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
