use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_kira_audio::{Audio, AudioSource};
use bevy_rapier2d::prelude::*;
use bitflags::bitflags;
use decorum::Total;
use fxhash::FxHashSet;

use crate::{asset, Ball, Mine, Owner, Player};

bitflags! {
    pub struct CollisionGroups: u32 {
        const ROCKET      = 0b001;
        const ROCKET_CAST = 0b001;
        const BALL        = 0b010;
        const MINE        = 0b100;
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

/// Rocket collision event
pub struct RocketCollision {
    pub rocket: Entity,
    pub other: Entity,
}

pub fn collect_balls(
    mut rockets: Query<(
        Entity,
        &PrevPosition,
        &mut Transform,
        &RigidBodyCollidersComponent,
        &Owner,
    )>,
    owned: Query<&Owner>,
    query_pipeline: Res<QueryPipeline>,
    collider_query: QueryPipelineColliderComponentsQuery,
    collider_shapes: Query<&ColliderShapeComponent>,
    parents: Query<&Parent>,
    balls: Query<&Ball>,
    mines: Query<&Mine>,
    mut commands: Commands,
    mut players: ResMut<Vec<Player>>,
    mut rocket_collisions: EventWriter<RocketCollision>,
    audio: Res<Audio>,
    sounds: Res<Assets<AudioSource>>,
) {
    let collider_set = QueryPipelineColliderComponentsSet(&collider_query);

    // Each impact contains a player index, a player rocket entity, an optional other player index, a ball/mine/rocket entity, and a time of impact.
    let mut impacts = vec![];

    for (rocket, prev_pos, curr_transform, colliders, owner) in rockets.iter() {
        let prev_pos = prev_pos.0;
        let curr_pos = curr_transform.translation.xy();

        // Positions equal => rockets just spawned (not near any balls) or rockets didn't move
        if prev_pos == curr_pos {
            continue;
        }

        // The colliders are missing for 1 frame, so skip that frame
        if colliders.0 .0.is_empty() {
            return;
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
            impacts.push((owner.0, rocket, None, parent, curr_toi));

            if mines.get(parent).is_ok() {
                break;
            }
        }

        // Collision with other rockets
        for (other, other_prev_pos, other_curr_transform, other_colliders, other_owner) in
            rockets.iter()
        {
            if other_owner.0 > owner.0 {
                let other_curr_pos = other_curr_transform.translation.xy();
                // Account for the other rocket's motion
                // Collider X goes a->b, and collider Y goes c->d.
                // From Y's point of view, X goes (a - c) -> (b - d).
                // This implies a velocity of (b - d) - (a - c) = (b - a) - (d - c)
                // Since Y is now at d, the shapecast should start at a - c + d
                let position = prev_pos - other_prev_pos.0 + other_curr_pos;
                let position = Isometry::new(position.into(), 0.0);
                let velocity = velocity - (other_curr_pos - other_prev_pos.0);
                let groups = InteractionGroups::new(
                    CollisionGroups::ROCKET_CAST.bits(),
                    CollisionGroups::ROCKET.bits(),
                );

                if let Some((_, hit)) = query_pipeline.cast_shape(
                    &collider_set,
                    &position,
                    &velocity.into(),
                    &*shape.0 .0,
                    1.0,
                    groups,
                    Some(&|rocket_collider| rocket_collider == other_colliders.0 .0[0]),
                ) {
                    impacts.push((owner.0, rocket, Some(other_owner.0), other, hit.toi));
                }
            }
        }
    }

    // Figure out which rockets hit which items first
    impacts.sort_by_key(|(_, _, _, _, toi)| Total::from(*toi));
    let mut items_reached = FxHashSet::default();
    let mut live_rockets = vec![true; players.len()];
    let mut tois = vec![None; players.len()];
    for (player_index, rocket, other_player_index, item, toi) in impacts {
        if let Some(other_player_index) = other_player_index {
            // Rocket-rocket collision. Both rockets must be alive for the collision to happen.
            if live_rockets[player_index as usize] && live_rockets[other_player_index as usize] {
                commands.entity(rocket).despawn_recursive();
                commands.entity(item).despawn_recursive();
                live_rockets[player_index as usize] = false;
                live_rockets[other_player_index as usize] = false;
                tois[player_index as usize] = Some(toi);
                tois[other_player_index as usize] = Some(toi);
                rocket_collisions.send(RocketCollision { rocket, other: item });
                audio.play(sounds.get_handle(asset::Explosion));
            }
        } else if live_rockets[player_index as usize] && items_reached.insert(item) {
            commands.entity(item).despawn_recursive();

            if balls.get(item).is_ok() {
                if let Ok(owner) = owned.get(item) {
                    // Destruction round
                    players[owner.0 as usize].num_balls -= 1;
                    audio.play(sounds.get_handle(asset::PlayerBallPickup));
                } else {
                    // Normal round
                    players[player_index as usize].num_balls += 1;
                    audio.play(sounds.get_handle(asset::BallPickup));
                }
            } else if mines.get(item).is_ok() {
                commands.entity(rocket).despawn_recursive();
                live_rockets[player_index as usize] = false;
                tois[player_index as usize] = Some(toi);
                rocket_collisions.send(RocketCollision { rocket, other: item });
                audio.play(sounds.get_handle(asset::Explosion));
            }
        }
    }

    // Move despawned rockets to impact position. This is relevant for graphing
    for (_, prev_pos, mut curr_transform, _, owner) in rockets.iter_mut() {
        if let Some(toi) = tois[owner.0 as usize] {
            let pos_xy = prev_pos.0.lerp(curr_transform.translation.xy(), toi);
            curr_transform.translation = pos_xy.extend(curr_transform.translation.z);
        }
    }
}
