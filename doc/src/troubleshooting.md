# Troubleshooting

If you have problems using spcasm, first consult the below problems and solutions. If that doesn't help, feel free to [file an issue](https://github.com/kleinesfilmroellchen/spcasm/issues/new).

## There is no error at the location that spcasm tells me!

While decently good, spcasm still sometimes suffers from incorrect source code indices. Look at the lines above the error and what the error actually says.

## BRR encoding is slow!

The BRR encoder benefits massively from Rust optimizations. An expected encode speed on modern hardware is only ~700KB/s unoptimized, but up to 50MB/s optimized. Run any BRR-related commands (benchmarks, tests, assembly that invokes the encoder a lot) under an optimized spcasm build. It is usually enough to compile spcasm in release mode by passing `--release` to any cargo command (build, run, test, ...)
