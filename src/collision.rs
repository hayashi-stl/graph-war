use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_rapier2d::prelude::*;
use bitflags::bitflags;
use decorum::Total;
use fxhash::FxHashSet;

use crate::{Ball, Mine, Owner, Player};

bitflags! {
    pub struct CollisionGroups: u32 {
        const ROCKET      = 0b001;
        const ROCKET_CAST = 0b001;
        const BALL        = 0b010;
        const MINE        = 0b100;
    }
}

pub fn handle_collisions(mut item_events: EventReader<ContactEvent>) {
    for event in item_events.iter() {
        log::info!("Collision event: {:?}", event);
    }
}

/// Previous position of an object
#[derive(Component)]
pub struct PrevPosition(pub Vec2);

pub fn update_prev_positions(mut positions: Query<(&mut PrevPosition, &GlobalTransform)>) {
    for (mut prev, curr) in positions.iter_mut() {
        prev.0 = curr.translation.xy();
    }
}

pub fn collect_balls(
    rockets: Query<(
        Entity,
        &PrevPosition,
        &mut Transform,
        &RigidBodyCollidersComponent,
        &Owner,
    )>,
    query_pipeline: Res<QueryPipeline>,
    collider_query: QueryPipelineColliderComponentsQuery,
    collider_shapes: Query<&ColliderShapeComponent>,
    parents: Query<&Parent>,
    balls: Query<&Ball>,
    mines: Query<&Mine>,
    mut commands: Commands,
    mut players: ResMut<Vec<Player>>,
) {
    let collider_set = QueryPipelineColliderComponentsSet(&collider_query);

    // Each impact contains a player index, a player rocket entity, ball/mine/rocket entity, and a time of impact.
    let mut impacts = vec![];

    for (rocket, prev_pos, curr_transform, colliders, owner) in rockets.iter() {
        let prev_pos = prev_pos.0;
        let curr_pos = curr_transform.translation.xy();

        // Positions equal => rockets just spawned (not near any balls) or rockets didn't move
        if prev_pos == curr_pos {
            continue;
        }

        // Perform shape-casts one at a time to get all the balls swept
        // Unfortunately, kinematic-static CCD doesn't work so mines are getting swept as well here.
        let rocket_collider_entity = colliders.0 .0[0].entity();
        let mut curr_toi = 0.0;
        let shape = collider_shapes.get(rocket_collider_entity).unwrap();
        let velocity = curr_pos - prev_pos;
        let groups = InteractionGroups::new(
            CollisionGroups::ROCKET_CAST.bits(),
            (CollisionGroups::BALL | CollisionGroups::MINE).bits(),
        );
        let mut collided_items = FxHashSet::default();
        while let Some((item_collider, hit)) = query_pipeline.cast_shape(
            &collider_set,
            &Isometry::new(prev_pos.lerp(curr_pos, curr_toi).into(), 0.0),
            &velocity.into(),
            &*shape.0 .0,
            1.0 - curr_toi,
            groups,
            Some(&|item_collider| !collided_items.contains(&item_collider.entity())),
        ) {
            curr_toi += hit.toi;
            collided_items.insert(item_collider.entity());
            let parent = parents.get(item_collider.entity()).unwrap().0;
            impacts.push((owner.0, rocket, parent, curr_toi));

            if mines.get(parent).is_ok() {
                break;
            }
        }
    }

    // Figure out which rockets hit which items first
    impacts.sort_by_key(|(_, _, _, toi)| Total::from(*toi));
    let mut items_reached = FxHashSet::default();
    let mut live_rockets = vec![true; players.len()];
    for (player_index, rocket, item, _) in impacts {
        if live_rockets[player_index as usize] && items_reached.insert(item) {
            commands.entity(item).despawn_recursive();

            if balls.get(item).is_ok() {
                players[player_index as usize].num_balls += 1;
            } else if mines.get(item).is_ok() {
                commands.entity(rocket).despawn_recursive();
                live_rockets[player_index as usize] = false;
            }
        }
    }
}
