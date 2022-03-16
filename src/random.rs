use bevy::prelude::*;
use decorum::Total;
use rand::prelude::Distribution;

/// A region made of disjoint rectangles.
/// Can be used to sample a point.
#[derive(Clone, Debug)]
pub struct RectRegion {
    rects: Vec<(Rect<f32>, f32)>,
    scale: f32,
}

impl RectRegion {
    pub fn new(rects: &[Rect<f32>], scale: f32) -> Self {
        let mut sum_areas = 0.0;
        let mut rects = rects
            .iter()
            .map(|rect| {
                let result = (*rect, sum_areas);
                sum_areas += (rect.right - rect.left) * (rect.top - rect.bottom);
                result
            })
            .collect::<Vec<_>>();
        // Normalize
        for (_, cdf) in &mut rects {
            *cdf /= sum_areas;
        }

        Self { rects, scale }
    }
}

impl Distribution<Vec2> for RectRegion {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Vec2 {
        let rect_select = rng.gen_range(0f32..1f32);
        let rect_index = self
            .rects
            .binary_search_by_key(&Total::from(rect_select), |(_, cdf)| Total::from(*cdf));
        let rect = match rect_index {
            Ok(index) => self.rects[index].0,
            Err(index) => self.rects[index - 1].0,
        };

        let x = rng.gen_range(rect.left..rect.right);
        let y = rng.gen_range(rect.bottom..rect.top);
        Vec2::new(x, y) * self.scale
    }
}
