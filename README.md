# `Fill-Batchalign-Words`

Dan Villarreal

This repository hosts a Shiny app (https://djvill.shinyapps.io/fill-batchalign-words/) that allows users to fill transcribed words from [Batchalign](https://github.com/TalkBank/batchalign) into files that have undergone [segmentation](https://djvill.github.io/APLS/doc/transcription-convention#segmentation), customizing the output via several parameters.
Files must currently be `.eaf` ([Elan](https://archive.mpi.nl/tla/elan)) transcription format.
Segmentation is typically hand-corrected output of [pyannote](https://github.com/pyannote/pyannote-audio) speaker diarization.
