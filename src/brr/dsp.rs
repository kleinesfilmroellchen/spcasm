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
#[allow(clippy::doc_markdown)]
pub fn apply_brrtools_treble_boost_filter(samples: &[DecodedSample]) -> Vec<DecodedSample> {
	// "Tepples' coefficient multiplied by 0.6 to avoid overflow in most cases"
	const filter: [f64; 8] =
		[0.912_962, -0.161_99, -0.015_328_3, 0.042_678_3, -0.037_200_4, 0.023_436, -0.010_581_6, 0.002_504_74];
	apply_fir_filter(filter, samples)
}

/// Applies a treble boost filter which exactly reverses the Gaussian interpolation of the hardware BRR decoder.
pub fn apply_precise_treble_boost_filter(samples: &[DecodedSample]) -> Vec<DecodedSample> {
	const filter: [f64; 12] = [
		1.91237, -0.59909, 0.18768, -0.05879, 0.01842, -0.00577, 0.00181, -0.00057, 0.00018, -0.00006, 0.00002,
		-0.00001,
	];
	apply_fir_filter(filter, samples)
}

/// Apply an FIR (finite impulse response) filter of arbitrary lookback to the input samples. The input filter only
/// provides one half of the filter coefficients; it is mirrored and used for lookahead the same as for lookback. For
/// example, the third coefficient is both used for `x[i-3]` as well as `x[i+3]` when processing sample number `i`. The
/// zeroth filter coefficient is applied to the sample `x[i]` itself and not involved in mirroring.
pub fn apply_fir_filter<const lookback: usize>(
	filter: [f64; lookback],
	samples: &[DecodedSample],
) -> Vec<DecodedSample> {
	if samples.is_empty() {
		samples.to_vec()
	} else {
		let last_sample = samples.last().unwrap();
		let first_sample = samples.first().unwrap();
		(0 .. samples.len())
			.map(|i| {
				f64::from(samples[i]).mul_add(
					filter[0],
					(1 ..= lookback - 1)
						.rev()
						.map(|k| {
							filter[k].mul_add(
								f64::from(*samples.get(i.saturating_add(k)).unwrap_or(last_sample)),
								filter[k] * f64::from(*samples.get(i.saturating_sub(k)).unwrap_or(first_sample)),
							)
						})
						.sum::<f64>(),
				) as i16
			})
			.collect()
	}
}
