# Regression tests

Each test is a non-interactive program: exit code 0 means success, anything
else a failure. Run them from the repository root:

```sh
instantfpc .github/workflows/regression.pas
```

Requirements: Free Pascal, Lazarus with the `lmDSP` (LMath), `BGRABitmapPack`,
`bgracontrols` and `playwavepackage` packages registered, `ffmpeg` and
`ffprobe`. `--help` lists the options.

The `widgetset` tests are built and run once per widgetset. On Linux the
defaults are GTK2, GTK3, Qt5 and Qt6, run on a private Xvfb display
(`xvfb-run`), so they need the GTK2/GTK3 development packages, `libQt5Pas`,
`libQt6Pas` and Xvfb. `--ws=gtk2,qt6` selects a subset; `--display=current`
uses the session's `$DISPLAY` instead of Xvfb. Windows uses win32, macOS cocoa.
The `application` test creates the application's forms, analyzes two
synthetic WAVs through the main form's batch, resizes and closes the form.
It checks for `ffmpeg` and `ffprobe` before creating the forms and stops with
an error if either is missing. On Windows the runner copies both tools
beside the decoder and application test executables (`--ffmpeg-dir` overrides
the source folder; otherwise they are found in `PATH`).

The `ebu` test reads the EBU reference WAVs (see `references/LICENSES.md`):
`--ebu[=FOLDER]` runs it, otherwise it is skipped.

On Windows the tests are built for x86_64/win64, like the released
application.
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
