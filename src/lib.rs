pub mod ui;

use bevy::{
    ecs::schedule::ShouldRun, math::Mat2, prelude::*, render::camera::ScalingMode,
    window::WindowResized,
};
use bevy_svg::prelude::*;
use std::iter;
#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

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
        .add_plugins(DefaultPlugins)
        .add_plugin(SvgPlugin)
        .add_startup_system(ui::load_ui.label("setup"))
        .add_startup_system(load_field.label("load_field").after("setup"))
        .add_startup_system(resize.after("load_field"))
        .add_system(resize.with_run_criteria(resized))
        .run();
}

pub fn load_field(mut commands: Commands, asset_server: Res<AssetServer>) {
    log::info!("Setup");

    const AXIS_THICKNESS: f32 = 0.04;
    const GRID_THICKNESS: f32 = 0.02;
    let scale = 4.0;
    let cell_size = 1.0;

    let mut camera = OrthographicCameraBundle::new_2d();
    camera.orthographic_projection.scaling_mode = ScalingMode::FixedVertical;
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
            Transform::from_xyz(0.0, dist, 0.0),
            Transform::from_xyz(0.0, -dist, 0.0),
            rot_90.mul_transform(Transform::from_xyz(0.0, dist, 0.0)),
            rot_90.mul_transform(Transform::from_xyz(0.0, -dist, 0.0)),
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
                    Transform::from_xyz(dist, -0.05, 1.0),
                    label_alignment_x.clone(),
                ),
                (
                    Transform::from_xyz(-0.05, dist, 1.0),
                    label_alignment_y.clone(),
                ),
            ];
            for (transform, alignment) in pairs {
                commands
                    .spawn_bundle(Text2dBundle {
                        text: Text::with_section(
                            format!("{}", dist),
                            label_style.clone(),
                            alignment.clone(),
                        ),
                        transform,
                        ..Default::default()
                    })
                    .insert(RelativeTextSize(0.2));
            }
        }
    }

    for (i, pos) in [
        [3.0, 3.0, 2.0],
        [3.0, -3.0, 2.0],
        [-3.0, -3.0, 2.0],
        [-3.0, 3.0, 2.0],
    ]
    .into_iter()
    .enumerate()
    {
        commands.spawn_bundle(Svg2dBundle {
            svg: asset_server.load(&format!("player{}.svg", i + 1)),
            transform: Transform::from_translation(Vec3::from(pos))
                .with_scale(Vec3::from([0.4; 3])),
            ..Default::default()
        });
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
    mut graph_node: Query<&mut Style, With<ui::GraphNode>>,
    camera: Query<&OrthographicProjection, Without<ui::UiCamera>>,
    windows: Res<Windows>,
) {
    let height = windows.get_primary().unwrap().height() as f32;
    if let Ok(mut style) = graph_node.get_single_mut() {
        style.flex_basis = Val::Px(height);
    }

    let camera = if let Ok(camera) = camera.get_single() {
        camera
    } else {
        return;
    };

    let scale = camera.scale;
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
