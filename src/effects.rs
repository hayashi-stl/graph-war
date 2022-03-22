use bevy::prelude::*;

use crate::{asset, collision::RocketCollision, z, Field};

#[derive(Component)]
pub struct Effect;

pub fn spawn_boom(
    mut commands: Commands,
    field: Query<Entity, With<Field>>,
    mut rocket_collisions: EventReader<RocketCollision>,
    transforms: Query<&Transform>,
    images: Res<Assets<Image>>,
) {
    let rotation = Quat::from_rotation_z(std::f32::consts::TAU / 16.0);

    commands.entity(field.single()).with_children(|node| {
        for collision in rocket_collisions.iter() {
            let pos0 = transforms.get(collision.rocket).unwrap();
            let pos1 = transforms.get(collision.other).unwrap();
            let mut position = (pos0.translation + pos1.translation) / 2.0;
            position.z = z::BOOM;
            node.spawn_bundle(SpriteBundle {
                sprite: Sprite { custom_size: Some([0.5; 2].into()), ..Default::default() },
                texture: images.get_handle(asset::Boom),
                transform: Transform::from_rotation(rotation).with_translation(position),
                ..Default::default()
            })
            .insert(Effect);
        }
    });
}

pub fn remove_effects(mut commands: Commands, effects: Query<Entity, With<Effect>>) {
    for entity in effects.iter() {
        commands.entity(entity).despawn_recursive();
    }
}
