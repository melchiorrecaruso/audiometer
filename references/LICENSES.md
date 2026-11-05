# Licenses of the reference material

AudioMeter is implemented and tested against the third-party material
listed below. It is **not** included in this repository and **not** covered
by the AudioMeter license (GNU GPL v3, see `../LICENSE`): each item remains
the property of its owner, under the terms listed. Download it from the
official sources.

The EBU compliance test reads the test set from a local folder:

    build/audiotest <folder with the EBU test set WAV files>

Files copied into this folder are ignored by git (see `../.gitignore`).

## Standards and specifications

| Document | Owner | Official source | Terms |
|---|---|---|---|
| `R-REC-BS.1770-5-202311.pdf` | ITU | <https://www.itu.int/rec/R-REC-BS.1770> | © ITU 2023. All rights reserved. No part may be reproduced, by any means whatsoever, without written permission of ITU (from the document). |
| `tech3341.pdf` | EBU | <https://tech.ebu.ch/publications/tech3341> | © EBU. Free download from the EBU; no redistribution terms stated in the document. |
| `tech3342.pdf` | EBU | <https://tech.ebu.ch/publications/tech3342> | As above. |
| `tech3343.pdf` | EBU | <https://tech.ebu.ch/publications/tech3343> | As above. |
| `tech3344.pdf` | EBU | <https://tech.ebu.ch/loudness> | As above. |
| `measuring-dr-en-v3.pdf` | Pleasurize Music Foundation (author: Friedemann Tischmeyer) | — | No terms stated in the document. |

## Test signals

| Material | Owner | Official source | Terms |
|---|---|---|---|
| EBU loudness test set v05 (`ebu-loudness-test-setv05.zip`) | EBU (PLOUD group) | <https://tech.ebu.ch/loudness> | Provided "as is" with no warranty; may only be used for technical testing purposes (from its `readme.txt`). |

`seq-3341-7_seq-3342-5` and `seq-3341-8_seq-3342-6` contain excerpts of the
films *The Misfortunates* (De Helaasheid der Dingen, dir. Felix van
Groeningen, prod. Menuet) and *Bloody Mondays and Strawberry Pies* (dir. Coco
Schreiber, prod. Bonanza Films). Their right holders granted permission to
the EBU (see `readme.txt` in the test set).

## Software used as implementation reference

Some AudioMeter units follow the behaviour of FFmpeg filters and reproduce
data from their source code. Algorithms are re-implemented in Pascal; the
FFmpeg sources are not included in this repository.

| AudioMeter unit | FFmpeg source (release 8.0) | Copyright | License |
|---|---|---|---|
| `src/spectrogram.pas` (picture layout, `intensity` color table) | `libavfilter/avf_showspectrum.c` | © 2012-2013 Clément Bœsch, © 2013 Rudolf Polzer, © 2015 Paul B Mahol | LGPL 2.1 or later |
| `src/freqspectrum.pas` | `libavfilter/avf_showfreqs.c` | © 2015 Paul B Mahol | LGPL 2.1 or later |
| `src/stft.pas` (Hann window, magnitude scale) | `libavfilter/window_func.h`, `avf_showspectrum.c` | © 2015 Paul B Mahol | LGPL 2.1 or later |

The LGPL 2.1 (section 3) allows code derived from these files to be
distributed under the GNU GPL, as AudioMeter is. See
<https://ffmpeg.org/legal.html>.
