//! Signal Processing Utilities for BRR.

use super::DecodedSample;

/// Applies the Gaussian lowpass filter of the S-SMP BRR hardware decoder to accurately emulate what digital audio
/// signal would result from the given sample.
///
/// Adopted from BRRTools' `apply_gauss_filter` [^license].
///
/// [^license]: BRRTools does not have a proper OSS license, the author states in the README that "This is freeware, feel
/// free to redistribute/improve but DON'T CLAIM IT IS YOUR OWN WORK THANK YOU".
#[allow(clippy::doc_markdown)]
pub fn apply_hardware_gauss_filter(samples: &mut [DecodedSample]) {
	if samples.len() >= 2 {
		let mut previous = (372i32 + 1304)
			.overflowing_mul(samples[0] as i32)
			.0
			.overflowing_add(372i32.overflowing_mul(samples[1] as i32).0)
			.0;
		for i in 1 .. samples.len() - 1 {
			let next = 372i32
				.overflowing_mul((samples[i - 1] as i32).overflowing_add(samples[i + 1].into()).0)
				.0
				.overflowing_add(1304i32.overflowing_mul(samples[i].into()).0)
				.0;
			samples[i - 1] = (previous / 2048).clamp(i16::MIN as i32, i16::MAX as i32) as i16;
			previous = next;
		}
		let last = 372i32
			.overflowing_mul((*samples.iter().nth_back(2).unwrap()).into())
			.0
			.overflowing_add((1304i32 + 372).overflowing_mul((*samples.last().unwrap()).into()).0)
			.0;
		*samples.iter_mut().nth_back(2).unwrap() = (previous / 2048).clamp(i16::MIN as i32, i16::MAX as i32) as i16;
		*samples.last_mut().unwrap() = (last / 2048).clamp(i16::MIN as i32, i16::MAX as i32) as i16;
	}
}

/// Applies a treble boost to the given samples which compensates the lowpass effect of the Gaussian interpolation the
/// hardware BRR decoder of the S-SMP performs.
///
/// Adopted from BRRTools' `treble_boost_filter` [^license].
///
/// [^license]: BRRTools does not have a proper OSS license, the author states in the README that "This is freeware, feel
/// free to redistribute/improve but DON'T CLAIM IT IS YOUR OWN WORK THANK YOU".
#[allow(clippy::doc_markdown, unused)]
pub fn apply_treble_boost_filter(samples: &[DecodedSample]) -> Vec<DecodedSample> {
	// "Tepples' coefficient multiplied by 0.6 to avoid overflow in most cases"
	if samples.is_empty() {
		samples.to_vec()
	} else {
		const coefficients: [f64; 8] =
			[0.912_962, -0.161_99, -0.015_328_3, 0.042_678_3, -0.037_200_4, 0.023_436, -0.010_581_6, 0.002_504_74];
		let last_sample = samples.last().unwrap();
		let first_sample = samples.first().unwrap();
		(0 .. samples.len())
			.map(|i| {
				f64::from(samples[i]).mul_add(
					coefficients[0],
					(1 ..= 7)
						.rev()
						.map(|k| {
							coefficients[k].mul_add(
								f64::from(*samples.get(i.saturating_add(k)).unwrap_or(last_sample)),
								coefficients[k] * f64::from(*samples.get(i.saturating_sub(k)).unwrap_or(first_sample)),
							)
						})
						.sum::<f64>(),
				) as i16
			})
			.collect()
	}
}
