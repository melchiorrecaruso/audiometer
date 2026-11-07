# Regression tests

Each test is a non-interactive program: exit code 0 means success, anything
else a failure. Run them from the repository root:

```sh
instantfpc .github/workflows/regression.pas
```

Requirements: Free Pascal, Lazarus with the `lmDSP` (LMath) and
`BGRABitmapPack` packages registered, `ffmpeg` and `ffprobe`.
`--help` lists the options.

On Windows the tests are built for x86_64/win64, like the released
application (the i386 compiler cannot load the BGRABitmap units reliably).
`--cpu=` and `--os=` choose another target; an empty value keeps the
compiler's default.

To add a test, create `<theme>/test_<theme>.lpr` and `<theme>/test_<theme>.lpi`
(the program is built beside the project) and register it in `tests.txt`,
whose header describes the modes. Logs are written to
`.regression-build/<mode>/test_<theme>/`.

The `workers` test covers cancellation before/after `Start`, failure completion,
main-thread delivery of `OnTerminate`, and exactly-once destruction with both
manual ownership and `FreeOnTerminate`. It also runs valid in-memory WAV
analyses (including cancellation inside `Execute`), followed by a screen worker,
and releases the input resources at completion before automatic destruction.
Rendering itself is covered by `drawers`; `workers` uses an empty drawing mode.
These tests do not automate the application's file dialogs or inject allocation
failures into GUI error handlers.
