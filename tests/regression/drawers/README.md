# Drawer rendering regressions

Requires Lazarus LCL, BGRABitmapPack, lmDSP and the bundled DejaVu font.
This suite is separate from the console-only synthetic manifest because it
initializes the widgetset and needs a graphical session.

From the repository root:

```sh
lazbuild --no-write-project tests/regression/drawers/test_drawers.lpi
.regression-build/drawers/test_drawers
# Linux headless alternative:
timeout 120 xvfb-run -a .regression-build/drawers/test_drawers
```

On Windows, run the corresponding `test_drawers.exe` after building with the
Windows Lazarus packages. The test code uses conditional `cthreads` only on
Unix; no Unix process API is needed by the tests themselves.

The five chart workers are started and joined as real threads. Cases cover
normal and fractional durations, subsecond signals, input shorter than an
FFT window, empty input, silence, finite short-term loudness, tiny/zero-sized
bitmaps and worker error propagation. Pixel assertions ensure that a
subsecond waveform is not collapsed to a column and that the spectrogram
contains colored data rather than only axes. These are not cross-platform
pixel-perfect snapshot tests.

A nil target deliberately triggers worker exceptions after resource
allocation. The project enables range/overflow checks and heaptrc; verify
both successful assertions and `0 unfreed memory blocks` in the output.

The loudness display uses LUFS directly, clips finite values to its visible
-70..+5 LUFS interval, and breaks curves at unavailable/nonfinite values.
The meter data itself is not changed. Report duration remains integer
seconds; rendering and playback-end checks use `DurationSeconds`.

Tests do not certify Windows rendering or the full GUI close/resize event
lifecycle. Those require running the application on the target platform.
