use bevy::{ecs::system::EntityCommands, prelude::*};
use bevy_egui::EguiContext;
use bevy_inspector_egui::Inspectable;
use egui::Align2;
use fxhash::FxHashMap;

use crate::{
    graph::{SendFunctions, QUICK_HELP},
    time::{AdvanceRound, AdvanceTurn},
    Game, Owner, PlayState, Player,
};

const FONT_SIZE: f32 = 18.0;

fn enter_function_text(player_index: u32) -> String {
    format!(
        concat!(
            "P{}: Enter functions in terms of t (0 ≤ t ≤ 1)\n",
            "The curve will be moved so it starts at your position",
        ),
        player_index + 1
    )
}

trait EntityCommandsExt {
    /// UI for inputting a function
    fn spawn_function_ui(
        &mut self,
        asset_server: &Res<AssetServer>,
        player_index: u32,
    ) -> &mut Self;

    /// Shows what a player inputted
    fn spawn_function_display(
        &mut self,
        asset_server: &Res<AssetServer>,
        player_index: u32,
    ) -> &mut Self;

    fn maybe_insert(&mut self, component: Option<impl Component>) -> &mut Self;
}

impl<'w, 's, 'a> EntityCommandsExt for EntityCommands<'w, 's, 'a> {
    fn maybe_insert(&mut self, component: Option<impl Component>) -> &mut Self {
        if let Some(c) = component {
            self.insert(c)
        } else {
            self
        }
    }

    fn spawn_function_ui(
        &mut self,
        asset_server: &Res<AssetServer>,
        player_index: u32,
    ) -> &mut Self {
        let function_label_style = TextStyle {
            font: asset_server.load("NotoMono-Regular.ttf"),
            font_size: FONT_SIZE,
            color: Color::BLACK,
        };
        let center_align = TextAlignment {
            horizontal: HorizontalAlign::Center,
            vertical: VerticalAlign::Center,
        };

        let left_side_style = TextStyle {
            font: asset_server.load("NotoMono-Regular.ttf"),
            font_size: FONT_SIZE,
            color: Color::BLACK,
        };

        let button_style = TextStyle {
            font: asset_server.load("NotoMono-Regular.ttf"),
            font_size: 28.0,
            color: Color::BLACK,
        };

        self.with_children(|node| {
            node.spawn_bundle(TextBundle {
                text: Text {
                    sections: vec![
                        TextSection {
                            value: " \n".to_owned(),
                            style: function_label_style.clone(),
                        },
                        TextSection {
                            value: enter_function_text(player_index),
                            style: function_label_style.clone(),
                        },
                    ],
                    alignment: center_align,
                },
                style: Style {
                    align_self: AlignSelf::Center,
                    margin: Rect {
                        top: Val::Px(15.0),
                        ..Default::default()
                    },
                    ..Default::default()
                },
                ..Default::default()
            })
            .insert(FunctionStatus);

            for axis in ["x", "y"] {
                node.spawn_bundle(NodeBundle {
                    style: Style {
                        flex_direction: FlexDirection::Row,
                        align_items: AlignItems::Stretch,
                        ..Default::default()
                    },
                    color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                    ..Default::default()
                })
                .with_children(|node| {
                    node.spawn_bundle(TextBundle {
                        text: Text::with_section(
                            format!("{}(t)=", axis),
                            left_side_style.clone(),
                            center_align,
                        ),
                        style: Style {
                            align_self: AlignSelf::Center,
                            margin: Rect::all(Val::Px(4.0)),
                            ..Default::default()
                        },
                        ..Default::default()
                    });

                    // Textbox spot
                    node.spawn_bundle(NodeBundle {
                        style: Style {
                            flex_basis: Val::Percent(100.0),
                            flex_grow: 1.0,
                            flex_shrink: 1.0,
                            min_size: Size::new(Val::Px(0.0), Val::Px(23.0)),
                            margin: Rect::all(Val::Px(4.0)),
                            ..Default::default()
                        },
                        color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                        ..Default::default()
                    })
                    .insert(Owner(player_index))
                    .insert(FunctionEntryBox)
                    .maybe_insert((axis == "x").then(|| FunctionX))
                    .maybe_insert((axis == "y").then(|| FunctionY))
                    .insert(Textbox {
                        text: "".to_owned(),
                        multiline: false,
                    })
                    .insert(EguiId::default());
                });
            }

            node.spawn_bundle(NodeBundle {
                style: Style {
                    flex_direction: FlexDirection::Row,
                    align_items: AlignItems::Stretch,
                    ..Default::default()
                },
                color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                ..Default::default()
            })
            .with_children(|node| {
                node.spawn_bundle(TextBundle {
                    text: Text::with_section("where", left_side_style.clone(), center_align),
                    style: Style {
                        align_self: AlignSelf::FlexEnd,
                        margin: Rect::all(Val::Px(4.0)),
                        ..Default::default()
                    },
                    ..Default::default()
                });

                node.spawn_bundle(NodeBundle {
                    style: Style {
                        flex_basis: Val::Percent(100.0),
                        flex_grow: 1.0,
                        flex_shrink: 1.0,
                        min_size: Size::new(Val::Px(0.0), Val::Px(72.0)),
                        margin: Rect::all(Val::Px(4.0)),
                        ..Default::default()
                    },
                    color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                    ..Default::default()
                })
                .insert(Owner(player_index))
                .insert(FunctionEntryBox)
                .insert(FunctionWhere)
                .insert(Textbox {
                    text: "".to_owned(),
                    multiline: true,
                })
                .insert(EguiId::default());
            });

            node.spawn_bundle(ButtonBundle {
                style: Style {
                    align_self: AlignSelf::Center,
                    align_items: AlignItems::Center,
                    justify_content: JustifyContent::Center,
                    margin: Rect {
                        top: Val::Px(6.0),
                        ..Default::default()
                    },
                    ..Default::default()
                },
                color: UiColor(NORMAL_BUTTON),
                ..Default::default()
            })
            .insert(DoneButton)
            .insert(Owner(player_index))
            .with_children(|node| {
                node.spawn_bundle(TextBundle {
                    text: Text::with_section("Fire", button_style.clone(), center_align),
                    style: Style {
                        margin: Rect::all(Val::Px(4.0)),
                        ..Default::default()
                    },
                    ..Default::default()
                });
            });

            node.spawn_bundle(TextBundle {
                text: Text::with_section(
                    QUICK_HELP,
                    left_side_style.clone(),
                    TextAlignment {
                        horizontal: HorizontalAlign::Left,
                        vertical: VerticalAlign::Center,
                    },
                ),
                style: Style {
                    align_self: AlignSelf::FlexStart,
                    margin: Rect::all(Val::Px(4.0)),
                    ..Default::default()
                },
                ..Default::default()
            });
        })
    }

    fn spawn_function_display(
        &mut self,
        asset_server: &Res<AssetServer>,
        num_players: u32,
    ) -> &mut Self {
        let function_label_style = TextStyle {
            font: asset_server.load("NotoMono-Regular.ttf"),
            font_size: FONT_SIZE,
            color: Color::BLACK,
        };
        let center_align = TextAlignment {
            horizontal: HorizontalAlign::Center,
            vertical: VerticalAlign::Center,
        };

        let left_side_style = TextStyle {
            font: asset_server.load("NotoMono-Regular.ttf"),
            font_size: FONT_SIZE,
            color: Color::BLACK,
        };

        for player_index in 0..num_players {
            self.with_children(|node| {
                fn spawn_edge<'w, 's, 'a, 'b>(
                    node: &'b mut ChildBuilder<'w, 's, 'a>,
                ) -> EntityCommands<'w, 's, 'b> {
                    node.spawn_bundle(NodeBundle {
                        style: Style {
                            flex_basis: Val::Px(1.0),
                            flex_grow: 1.0,
                            flex_shrink: 1.0,
                            ..Default::default()
                        },
                        color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                        ..Default::default()
                    })
                }

                spawn_edge(node);

                node.spawn_bundle(TextBundle {
                    text: Text::with_section(
                        format!("P{}'s function: (0 ≤ t ≤ 1)", player_index + 1),
                        function_label_style.clone(),
                        center_align,
                    ),
                    style: Style {
                        align_self: AlignSelf::Center,
                        ..Default::default()
                    },
                    ..Default::default()
                });

                for axis in ["x", "y"] {
                    node.spawn_bundle(NodeBundle {
                        style: Style {
                            flex_direction: FlexDirection::Row,
                            align_items: AlignItems::Stretch,
                            ..Default::default()
                        },
                        color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                        ..Default::default()
                    })
                    .with_children(|node| {
                        node.spawn_bundle(TextBundle {
                            text: Text::with_section(
                                format!("{}(t) = ", axis),
                                left_side_style.clone(),
                                center_align,
                            ),
                            style: Style {
                                align_self: AlignSelf::Center,
                                margin: Rect::all(Val::Px(2.0)),
                                ..Default::default()
                            },
                            ..Default::default()
                        });

                        // Textbox spot
                        node.spawn_bundle(NodeBundle {
                            style: Style {
                                flex_basis: Val::Percent(100.0),
                                flex_grow: 1.0,
                                flex_shrink: 1.0,
                                min_size: Size::new(Val::Px(0.0), Val::Px(23.0)),
                                margin: Rect::all(Val::Px(2.0)),
                                ..Default::default()
                            },
                            color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                            ..Default::default()
                        })
                        .insert(Owner(player_index))
                        .insert(FunctionDisplayBox)
                        .maybe_insert((axis == "x").then(|| FunctionX))
                        .maybe_insert((axis == "y").then(|| FunctionY))
                        .insert(Textbox {
                            text: "".to_owned(),
                            multiline: false,
                        })
                        .insert(EguiId::default());
                    });
                }

                node.spawn_bundle(NodeBundle {
                    style: Style {
                        flex_direction: FlexDirection::Row,
                        align_items: AlignItems::Stretch,
                        ..Default::default()
                    },
                    color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                    ..Default::default()
                })
                .with_children(|node| {
                    node.spawn_bundle(TextBundle {
                        text: Text::with_section("where ", left_side_style.clone(), center_align),
                        style: Style {
                            align_self: AlignSelf::FlexEnd,
                            margin: Rect::all(Val::Px(2.0)),
                            ..Default::default()
                        },
                        ..Default::default()
                    });

                    node.spawn_bundle(NodeBundle {
                        style: Style {
                            flex_basis: Val::Percent(100.0),
                            flex_grow: 1.0,
                            flex_shrink: 1.0,
                            min_size: Size::new(Val::Px(0.0), Val::Px(72.0)),
                            margin: Rect::all(Val::Px(2.0)),
                            ..Default::default()
                        },
                        color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                        ..Default::default()
                    })
                    .insert(Owner(player_index))
                    .insert(FunctionDisplayBox)
                    .insert(FunctionWhere)
                    .insert(Textbox {
                        text: "".to_owned(),
                        multiline: true,
                    })
                    .insert(EguiId::default());
                });

                spawn_edge(node);
            });
        }

        let button_style = TextStyle {
            font: asset_server.load("NotoMono-Regular.ttf"),
            font_size: 28.0,
            color: Color::BLACK,
        };

        self.with_children(|node| {
            node.spawn_bundle(ButtonBundle {
                style: Style {
                    align_self: AlignSelf::Center,
                    align_items: AlignItems::Center,
                    justify_content: JustifyContent::Center,
                    margin: Rect {
                        top: Val::Px(6.0),
                        ..Default::default()
                    },
                    ..Default::default()
                },
                color: UiColor(NORMAL_BUTTON),
                ..Default::default()
            })
            .insert(NextRoundButton)
            .with_children(|node| {
                node.spawn_bundle(TextBundle {
                    text: Text::with_section("Next Round", button_style.clone(), center_align),
                    style: Style {
                        margin: Rect::all(Val::Px(4.0)),
                        ..Default::default()
                    },
                    ..Default::default()
                });
            });
        });

        self
    }
}

pub fn load_ui(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands
        .spawn_bundle(UiCameraBundle::default())
        .insert(UiCamera);

    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                flex_direction: FlexDirection::RowReverse,
                position_type: PositionType::Absolute,
                position: Rect {
                    bottom: Val::Percent(0.0),
                    left: Val::Percent(0.0),
                    ..Default::default()
                },
                size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                min_size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                max_size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                align_items: AlignItems::Stretch,
                ..Default::default()
            },
            color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
            ..Default::default()
        })
        .with_children(|node| {
            // Half of field (no nodes)
            node.spawn_bundle(NodeBundle {
                style: Style {
                    flex_basis: Val::Px(720.0),
                    flex_grow: 0.0,
                    flex_shrink: 0.0,
                    ..Default::default()
                },
                color: UiColor(Color::rgba(0.0, 0.0, 0.0, 0.0)),
                ..Default::default()
            })
            .insert(GraphNode);

            // Function display
            node.spawn_bundle(NodeBundle {
                style: Style {
                    display: Display::None,
                    flex_direction: FlexDirection::ColumnReverse,
                    align_items: AlignItems::Stretch,
                    flex_basis: Val::Percent(100.0),
                    flex_grow: 1.0,
                    flex_shrink: 1.0,
                    overflow: Overflow::Hidden,
                    ..Default::default()
                },
                color: UiColor(Color::rgba(0.65, 0.65, 0.65, 1.0)),
                ..Default::default()
            })
            .insert(FunctionDisplay)
            .spawn_function_display(&asset_server, 4);

            // Function entry
            node.spawn_bundle(NodeBundle {
                style: Style {
                    flex_direction: FlexDirection::ColumnReverse,
                    align_items: AlignItems::Stretch,
                    flex_basis: Val::Percent(100.0),
                    flex_grow: 1.0,
                    flex_shrink: 1.0,
                    overflow: Overflow::Hidden,
                    ..Default::default()
                },
                color: UiColor(Color::rgba(0.65, 0.65, 0.65, 1.0)),
                ..Default::default()
            })
            .insert(FunctionUi)
            .spawn_function_ui(&asset_server, 0);
        });
}

#[derive(Component)]
pub struct DoneButton;

#[derive(Component)]
pub struct NextRoundButton;

const NORMAL_BUTTON: Color = Color::rgb(0.85, 0.85, 0.85);
const HOVERED_BUTTON: Color = Color::rgb(0.80, 0.80, 0.80);
const PRESSED_BUTTON: Color = Color::rgb(0.75, 0.75, 0.75);
const DISABLED_BUTTON: Color = Color::rgb(0.70, 0.70, 0.70);

pub fn update_buttons(
    mut buttons: Query<(&Interaction, &mut UiColor), With<Button>>,
    buttons_enabled: Res<ButtonsEnabled>,
) {
    for (interaction, mut color) in buttons.iter_mut() {
        *color = if buttons_enabled.0 {
            match interaction {
                Interaction::None => UiColor(NORMAL_BUTTON),
                Interaction::Hovered => UiColor(HOVERED_BUTTON),
                Interaction::Clicked => UiColor(PRESSED_BUTTON),
            }
        } else {
            UiColor(DISABLED_BUTTON)
        }
    }
}

pub fn update_done_button(
    buttons: Query<(&Interaction, &Owner), (Changed<Interaction>, With<DoneButton>)>,
    mut fire_events: EventWriter<SendFunctions>,
    buttons_enabled: Res<ButtonsEnabled>,
) {
    if !buttons_enabled.0 {
        return;
    }

    if let Ok((interaction, owner)) = buttons.get_single() {
        if *interaction == Interaction::Clicked {
            fire_events.send(SendFunctions {
                player_index: owner.0,
            });
        }
    }
}

pub fn update_next_round_button(
    buttons: Query<&Interaction, (Changed<Interaction>, With<NextRoundButton>)>,
    mut advance_round_events: EventWriter<AdvanceRound>,
    buttons_enabled: Res<ButtonsEnabled>,
) {
    if !buttons_enabled.0 {
        return;
    }

    if let Ok(interaction) = buttons.get_single() {
        if *interaction == Interaction::Clicked {
            advance_round_events.send(AdvanceRound);
        }
    }
}

pub fn advance_turn(
    mut advance_turn_events: EventReader<AdvanceTurn>,
    mut play_state: ResMut<State<PlayState>>,
    mut owned_ui: Query<&mut Owner, Or<(With<FunctionEntryBox>, With<DoneButton>)>>,
    mut entry_textboxes: Query<&mut Textbox, With<FunctionEntryBox>>,
    mut status_text: Query<&mut Text, With<FunctionStatus>>,
    players: Res<Vec<Player>>,
    mut game: ResMut<Game>,
    mut textboxes_editable: ResMut<TextboxesEditable>,
    mut buttons_enabled: ResMut<ButtonsEnabled>,
    mut function_ui: Query<&mut Style, With<FunctionUi>>,
    mut function_display: Query<&mut Style, (With<FunctionDisplay>, Without<FunctionUi>)>,
) {
    if advance_turn_events.iter().next().is_none() {
        return;
    }

    game.order_index += 1;
    if game.order_index >= players.len() as u32 {
        game.order_index = 0;
        game.rotate_players();
    } else {
        textboxes_editable.0 = true;
    }
    buttons_enabled.0 = true;

    for mut owner in owned_ui.iter_mut() {
        owner.0 = game.player_turn();
    }
    for mut textbox in entry_textboxes.iter_mut() {
        textbox.text.clear();
    }

    let text = &mut status_text.single_mut();
    text.sections[0].value = " \n".into();
    text.sections[1].value = enter_function_text(game.player_turn());

    if game.order_index == 0 {
        // All players have entered functions.
        function_ui.single_mut().display = Display::None;
        function_display.single_mut().display = Display::Flex;
        play_state.set(PlayState::Fire).unwrap();
    }
}

pub fn advance_round(
    mut advance_round_events: EventReader<AdvanceRound>,
    mut play_state: ResMut<State<PlayState>>,
    mut function_ui: Query<&mut Style, With<FunctionUi>>,
    mut function_display: Query<&mut Style, (With<FunctionDisplay>, Without<FunctionUi>)>,
    mut textboxes_editable: ResMut<TextboxesEditable>,
) {
    if advance_round_events.iter().next().is_none() {
        return;
    }
    play_state.set(PlayState::Enter).unwrap();

    // Workaround while this is a startup system
    function_ui
        .get_single_mut()
        .map(|mut ui| ui.display = Display::Flex)
        .ok();
    function_display
        .get_single_mut()
        .map(|mut ui| ui.display = Display::None)
        .ok();
    textboxes_editable.0 = true;
}

/// Distinguishes the UI camera from another camera
#[derive(Component)]
pub struct UiCamera;

/// Labels the UI entity that covers the graph
#[derive(Component)]
pub struct GraphNode;

/// Labels the function entry UI
#[derive(Component)]
pub struct FunctionUi;

/// Labels the function display UI
#[derive(Component)]
pub struct FunctionDisplay;

pub fn setup_egui(mut egui_ctx: ResMut<EguiContext>) {
    let mut style = (*egui_ctx.ctx_mut().style()).clone();
    style.visuals = egui::Visuals::light();
    style.visuals.extreme_bg_color = egui::Color32::LIGHT_GRAY;
    egui_ctx.ctx_mut().set_style(style);
}

/// Labels entities that should get an egui textbox.
/// Stores the text that goes in the textbox.
#[derive(Component)]
pub struct Textbox {
    pub text: String,
    pub multiline: bool,
}

/// Whether textboxes are editable
pub struct TextboxesEditable(pub bool);

/// Whether buttons are enabled
pub struct ButtonsEnabled(pub bool);

/// Labels function entry textboxes
/// Labels function entry textboxes
#[derive(Component)]
pub struct FunctionEntryBox;

/// Labels function display textboxes
#[derive(Component)]
pub struct FunctionDisplayBox;

/// Labels x(t) textbox
#[derive(Component)]
pub struct FunctionX;

/// Labels y(t) textbox
#[derive(Component)]
pub struct FunctionY;

/// Labels "where" textbox
#[derive(Component)]
pub struct FunctionWhere;

/// Labels status text, which shows "success" or an error message in the first section
#[derive(Component)]
pub struct FunctionStatus;

pub fn update_textboxes(
    mut textboxes: Query<(&mut Textbox, &EguiId, &Node, &GlobalTransform)>,
    mut egui_ctx: ResMut<EguiContext>,
    textboxes_editable: Res<TextboxesEditable>,
) {
    for (mut textbox, id, size, transform) in textboxes.iter_mut() {
        if size.size.x == 0.0 && size.size.y == 0.0 {
            continue;
        }

        let id = if let Some(id) = id.0 { id } else { continue };
        let left = transform.translation.x - size.size.x / 2.0;
        let bottom = transform.translation.y - size.size.y / 2.0;

        egui::Area::new(id.to_string())
            .anchor(Align2::LEFT_BOTTOM, [left, -bottom])
            .show(egui_ctx.ctx_mut(), |ui| {
                ui.set_width(size.size.x);
                ui.set_height(size.size.y);

                fn add_textbox<'r>(
                    ui: &mut egui::Ui,
                    text: &'r mut dyn egui::TextBuffer,
                    text_edit_fn: impl Fn(&'r mut dyn egui::TextBuffer) -> egui::TextEdit<'r>,
                ) {
                    ui.add_sized(
                        ui.available_size(),
                        text_edit_fn(text).font(egui::FontId {
                            family: egui::FontFamily::Monospace,
                            size: FONT_SIZE,
                        }),
                    );
                }

                if textbox.multiline {
                    egui::ScrollArea::vertical().show(ui, |ui| {
                        if textboxes_editable.0 {
                            add_textbox(ui, &mut textbox.text, egui::TextEdit::multiline)
                        } else {
                            add_textbox(ui, &mut textbox.text.as_str(), egui::TextEdit::multiline)
                        }
                    });
                } else if textboxes_editable.0 {
                    add_textbox(ui, &mut textbox.text, egui::TextEdit::singleline);
                } else {
                    add_textbox(ui, &mut textbox.text.as_str(), egui::TextEdit::singleline);
                }
            });
    }
}

#[derive(Default)]
pub struct IdLender {
    next_if_full: u32,
    unused: Vec<u32>,
    active: FxHashMap<Entity, u32>,
}

impl IdLender {
    /// Lends an id to an entity and returns it.
    fn lend(&mut self, entity: Entity) -> u32 {
        let id = if let Some(next) = self.unused.pop() {
            next
        } else {
            self.next_if_full += 1;
            self.next_if_full - 1
        };
        self.active.insert(entity, id);
        id
    }

    /// Gives the id of an entity back to the lender.
    fn give_back(&mut self, entity: Entity) {
        let id = self.active.remove(&entity).unwrap();
        self.unused.push(id);
    }
}

#[derive(Default, Component, Inspectable)]
pub struct EguiId(Option<u32>);

pub fn assign_egui_ids(
    mut lender: ResMut<IdLender>,
    mut ids: Query<(Entity, &mut EguiId), Added<EguiId>>,
) {
    for (entity, mut id) in ids.iter_mut() {
        id.0 = Some(lender.lend(entity));
    }
}

pub fn give_back_egui_ids(mut lender: ResMut<IdLender>, entities: RemovedComponents<EguiId>) {
    for entity in entities.iter() {
        lender.give_back(entity);
    }
}
