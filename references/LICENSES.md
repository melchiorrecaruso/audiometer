# Licenses of the reference material

AudioMeter is implemented and tested against the third-party material
listed below. This material is kept outside the repository: each item
remains the property of its owner, under the terms listed. Download it from
the official sources.

The EBU compliance test reads the test set from a local folder:

    instantfpc .github/workflows/regression.pas --only=ebu/ --ebu=<folder with the EBU test set WAV files>

Files copied into this folder are ignored by git (see `../.gitignore`).

## Standards and specifications

| Document | Owner | Official source | Terms |
|---|---|---|---|
| `R-REC-BS.1770-5-202311.pdf` | ITU | <https://www.itu.int/rec/R-REC-BS.1770> | © ITU 2023. All rights reserved. No part may be reproduced, by any means whatsoever, without written permission of ITU (from the document). |
| `tech3341.pdf` | EBU | <https://tech.ebu.ch/publications/tech3341> | © EBU. Free download from the EBU. |
| `tech3342.pdf` | EBU | <https://tech.ebu.ch/publications/tech3342> | As above. |
| `tech3343.pdf` | EBU | <https://tech.ebu.ch/publications/tech3343> | As above. |
| `measuring-dr-en-v3.pdf` | Pleasurize Music Foundation (author: Friedemann Tischmeyer) | — | — |

## Test signals

| Material | Owner | Official source | Terms |
|---|---|---|---|
| EBU loudness test set v05 (`ebu-loudness-test-setv05.zip`) | EBU (PLOUD group) | <https://tech.ebu.ch/loudness> | Provided "as is" with no warranty; may only be used for technical testing purposes (from its `readme.txt`). |

`seq-3341-7_seq-3342-5` and `seq-3341-8_seq-3342-6` contain excerpts of the
films *The Misfortunates* (De Helaasheid der Dingen, dir. Felix van
Groeningen, prod. Menuet) and *Bloody Mondays and Strawberry Pies* (dir. Coco
Schreiber, prod. Bonanza Films). Their right holders granted permission to
the EBU (see `readme.txt` in the test set).

## External software

AudioMeter runs the `ffmpeg` and `ffprobe` programs as external tools, to
decode audio to WAV (`src/decoder.pas`) and in the `decoder` and
`application` regression tests.
