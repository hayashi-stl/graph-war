use bevy::prelude::*;

pub fn load_ui(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands
        .spawn_bundle(UiCameraBundle::default())
        .insert(UiCamera);

    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                flex_direction: FlexDirection::Row,
                position_type: PositionType::Absolute,
                position: Rect {
                    bottom: Val::Px(0.0),
                    right: Val::Px(0.0),
                    ..Default::default()
                },
                size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                align_items: AlignItems::Stretch,
                ..Default::default()
            },
            color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
            ..Default::default()
        })
        .with_children(|node| {
            let spawn_edge = |node: &mut ChildBuilder| {
                node.spawn_bundle(NodeBundle {
                    style: Style {
                        flex_direction: FlexDirection::Column,
                        align_items: AlignItems::Stretch,
                        flex_basis: Val::Percent(100.0),
                        flex_grow: 1.0,
                        flex_shrink: 1.0,
                        ..Default::default()
                    },
                    color: UiColor(Color::rgba(0.0, 0.0, 1.0, 0.5)),
                    ..Default::default()
                });
            };

            spawn_edge(node);
            node.spawn_bundle(NodeBundle {
                style: Style {
                    size: Size::new(Val::Px(720.0), Val::Percent(100.0)),
                    align_items: AlignItems::Stretch,
                    flex_basis: Val::Px(720.0),
                    flex_grow: 0.0,
                    flex_shrink: 0.0,
                    ..Default::default()
                },
                color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                ..Default::default()
            })
            .insert(GraphNode);
            spawn_edge(node);
        });

    commands.spawn_bundle(TextBundle {
        text: Text::with_section(
            "Sample Text",
            TextStyle {
                font: asset_server.load("NotoMono-Regular.ttf"),
                font_size: 20.0,
                color: Color::LIME_GREEN,
            },
            TextAlignment {
                horizontal: HorizontalAlign::Left,
                vertical: VerticalAlign::Bottom,
            },
        ),
        style: Style {
            align_self: AlignSelf::FlexEnd,
            position_type: PositionType::Absolute,
            position: Rect {
                bottom: Val::Px(5.0),
                right: Val::Px(10.0),
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Distinguishes the UI camera from another camera
#[derive(Component)]
pub struct UiCamera;

/// Labels the UI node that covers the graph
#[derive(Component)]
pub struct GraphNode;
