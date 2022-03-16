use std::time::Duration;

use bevy::prelude::*;

#[derive(Clone, Copy, Debug, Component, PartialEq, Eq)]
pub enum DelayedEvent {
    AdvanceTurn,
}

/// Event to move on to the next player
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AdvanceTurn;

#[derive(Clone, Debug, Bundle)]
pub struct DelayedEventBundle {
    timer: Timer,
    event: DelayedEvent,
}

impl DelayedEventBundle {
    /// `delay` is in seconds
    pub fn new(delay: f32, event: DelayedEvent) -> Self {
        Self {
            timer: Timer::new(Duration::from_secs_f32(delay), false),
            event,
        }
    }
}

pub fn advance_timers(
    mut commands: Commands,
    time: Res<Time>,
    mut timers: Query<(Entity, &mut Timer, &DelayedEvent)>,
    mut advance_turn_events: EventWriter<AdvanceTurn>,
) {
    for (entity, mut timer, event) in timers.iter_mut() {
        if timer.tick(time.delta()).finished() {
            commands.entity(entity).despawn();
            match event {
                DelayedEvent::AdvanceTurn => advance_turn_events.send(AdvanceTurn),
            }
        }
    }
}
