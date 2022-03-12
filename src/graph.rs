use bevy::prelude::*;

use crate::{
    ui::{FunctionX, FunctionY, Textbox},
    Owner,
};

/// Event that says that some player should fire a function from their position
pub struct FireFunction {
    pub player_index: u32,
}

pub fn handle_fire_events(
    function_x: Query<(&Owner, &Textbox), With<FunctionX>>,
    function_y: Query<(&Owner, &Textbox), With<FunctionY>>,
    mut fire_events: EventReader<FireFunction>,
) {
    for event in fire_events.iter() {
        let player = event.player_index;

        let fx = function_x
            .iter()
            .find_map(|(owner, textbox)| (owner.0 == player).then(|| &textbox.0))
            .unwrap();
        let fy = function_y
            .iter()
            .find_map(|(owner, textbox)| (owner.0 == player).then(|| &textbox.0))
            .unwrap();

        log::info!(
            "Player {} fired:\n    x(t)={}\n    y(t)={}",
            player + 1,
            fx,
            fy
        );
    }
}
