# Regression tests

Each test is a non-interactive program: exit code 0 means success, anything
else a failure. Run them from the repository root:

```sh
instantfpc .github/workflows/regression.pas
```

Requirements: Free Pascal, Lazarus with the `lmDSP` (LMath) and
`BGRABitmapPack` packages registered, `ffmpeg` and `ffprobe`.
`--help` lists the options.

To add a test, create `<theme>/test_<theme>.lpr` and `<theme>/test_<theme>.lpi`
(the program is built beside the project) and register it in `tests.txt`,
whose header describes the modes. Logs are written to
`.regression-build/<mode>/test_<theme>/`.
