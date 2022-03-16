#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

#[macro_use]
extern crate pest_derive;

pub mod collision;
pub mod graph;
pub mod random;
pub mod ui;

use bevy::{
    ecs::{schedule::ShouldRun, system::EntityCommands},
    math::{Mat2, Vec3Swizzles},
    prelude::*,
    render::camera::ScalingMode,
    window::WindowResized,
};
use bevy_egui::EguiPlugin;
use bevy_rapier2d::{physics::PhysicsSystems, prelude::*};
use bevy_svg::prelude::*;
use rand::prelude::Distribution;
use rand_pcg::Pcg64;
#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

use crate::{collision::CollisionGroups, random::RectRegion};

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
#[derive(Component)]
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
}

#[derive(Component)]
pub struct Ball;

#[derive(Component)]
pub struct Mine;

pub const ASPECT_RATIO: f32 = 16.0 / 9.0;

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
        .insert_resource(Pcg64::new(
            0xcafef00dd15ea5e5,
            0xa02bdbf7bb3c0a7ac28fa16a64abf96,
        ))
        .insert_resource(vec![Player::default(); 0])
        .add_plugins(DefaultPlugins)
        //.add_plugin(WorldInspectorPlugin::new())
        //.register_inspectable::<ui::EguiId>()
        .add_plugin(SvgPlugin)
        .add_plugin(EguiPlugin)
        .add_plugin(RapierPhysicsPlugin::<NoUserData>::default())
        .add_event::<graph::FireRocket>()
        .add_startup_system(ui::setup_egui.label("setup"))
        .add_startup_system(ui::load_ui.label("setup"))
        .add_startup_system(load_field.label("load_field").after("setup"))
        .add_startup_system(resize.after("load_field"))
        .add_system(resize.with_run_criteria(resized))
        .add_system(ui::update_textboxes)
        .add_system_to_stage(CoreStage::PreUpdate, ui::update_buttons)
        .add_system_to_stage(CoreStage::PreUpdate, collision::update_prev_positions)
        .add_system_to_stage(CoreStage::PostUpdate, ui::assign_egui_ids)
        .add_system_to_stage(CoreStage::PostUpdate, ui::give_back_egui_ids)
        .add_system_set(
            SystemSet::new()
                .before(PhysicsSystems::StepWorld)
                .with_system(ui::update_fire_buttons.label("fire_buttons"))
                .with_system(graph::handle_fire_events.after("fire_buttons"))
                .with_system(graph::move_rockets),
        )
        .add_system_set(
            SystemSet::new()
                .after(PhysicsSystems::StepWorld)
                .with_system(collision::handle_collisions)
                .with_system(collision::collect_balls.label("collect"))
                .with_system(update_scores.after("collect")),
        )
        .run();
}

/// Z-indexes
pub mod z {
    pub const GRID: f32 = 0.0;
    pub const GRID_TEXT: f32 = 1.0;
    pub const PLAYER: f32 = 2.0;
    pub const BALL: f32 = 2.0;
    pub const MINE: f32 = 3.0;
    pub const ROCKET: f32 = 4.0;
    pub const SCORE: f32 = 5.0;
}

pub fn load_field(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut rng: ResMut<Pcg64>,
    mut players: ResMut<Vec<Player>>,
) {
    const AXIS_THICKNESS: f32 = 0.04;
    const GRID_THICKNESS: f32 = 0.02;
    let cell_size = 1.0;
    let scale = 4.0;

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

    for (i, pos) in [
        [-3.0, 3.0, z::PLAYER],
        [-3.0, -3.0, z::PLAYER],
        [3.0, 3.0, z::PLAYER],
        [3.0, -3.0, z::PLAYER],
    ]
    .into_iter()
    .enumerate()
    {
        // Player icon
        commands
            .spawn_bundle(Svg2dBundle {
                svg: asset_server.load(&format!("player{}.svg", i + 1)),
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

    #[rustfmt::skip]
    let item_distribution = RectRegion::new(
        &[
            Rect { left: -0.875, right: -0.5,   bottom: -0.5,   top:  0.5,   },
            Rect { left:  0.5,   right:  0.875, bottom: -0.5,   top:  0.5,   },
            Rect { left: -0.5,   right:  0.5,   bottom: -0.875, top: -0.5,   },
            Rect { left: -0.5,   right:  0.5,   bottom:  0.5,   top:  0.875, },
            Rect { left: -0.5,   right:  0.5,   bottom: -0.5,   top:  0.5,   },
        ],
        scale,
    );

    fn spawn_item<'a, 'w, 's>(
        commands: &'a mut Commands<'w, 's>,
        asset_server: &Res<AssetServer>,
        point: Vec3,
        item_type: usize,
    ) -> EntityCommands<'w, 's, 'a> {
        let scale = 0.3;

        let mut entity_commands = commands.spawn_bundle(Svg2dBundle {
            svg: asset_server.load(["ball.svg", "mine.svg"][item_type]),
            transform: Transform::from_translation(point).with_scale(Vec3::from([scale; 3])),
            ..Default::default()
        });
        entity_commands
            .insert_bundle(RigidBodyBundle {
                body_type: RigidBodyType::Static.into(),
                position: point.xy().extend(0.0).into(),
                ..Default::default()
            })
            .insert(RigidBodyPositionSync::Discrete)
            .with_children(|body| {
                body.spawn_bundle(ColliderBundle {
                    shape: ColliderShape::ball(scale / 2.0).into(),
                    collider_type: ColliderType::Sensor.into(),
                    position: Vec2::ZERO.into(),
                    flags: ColliderFlags {
                        collision_groups: InteractionGroups::new(
                            [CollisionGroups::BALL, CollisionGroups::MINE][item_type].bits(),
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

    let points = item_distribution.clone().sample_iter(&mut *rng);
    for point in points.take(85) {
        spawn_item(&mut commands, &asset_server, point.extend(z::BALL), 0).insert(Ball);
    }
    let points = item_distribution.sample_iter(&mut *rng);
    for point in points.take(15) {
        spawn_item(&mut commands, &asset_server, point.extend(z::MINE), 1).insert(Mine);
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

fn resized(
    mut resize_events: EventReader<WindowResized>,
    new_cameras: Query<&OrthographicProjection, Added<OrthographicProjection>>,
) -> ShouldRun {
    if resize_events.iter().next().is_some() || !new_cameras.is_empty() {
        ShouldRun::Yes
    } else {
        ShouldRun::No
    }
}

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
