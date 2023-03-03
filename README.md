# Fingerboard

[![Actions Status](https://github.com/mbarbin/fingerboard/workflows/CI/badge.svg)](https://github.com/mbarbin/fingerboard/actions/workflows/ci.yml)
[![Deploy odoc Actions Status](https://github.com/mbarbin/fingerboard/workflows/Deploy-odoc/badge.svg)](https://github.com/mbarbin/fingerboard/actions/workflows/deploy-odoc.yml)

This project is meant to help me reason about and build a "microtonal
geography of the cello fingerboard".

# What's in the repo?

The repo implements the tools necessary to model microtonal geography
systems based on various scales. As we go over some specific
pythagorean, just and tempered scales and double stops, some specific
notations are introduced for each of the fingerboard locations
required to play each note. Then, we can reason about and exhibit
various facts involving these positions, while being able to verify
these facts programmatically, and display them with various format.

An example extracted from file [systems/cello/just/scales.ml](systems/cello/just/scales.ml)
displaying the names of the positions involved in playing the E Major
Just Scale on a cello tuned in perfect fifth:

```scheme
((IV ((E2 M3p) (F#2 A4p)))
 (III ((G#2 A1z) (A2 M2p) (B2 M3p) (C#3 A4z)))
 (II ((D#3 A1z) (E3 M2p) (F#3 M3p) (G#3 A4z)))
  (I
  ((A3 0) (B3 M2p) (C#4 M3z) (D#4 A4z) (E4 5p) (F#4 M6p) (G#4 M7z) (A4 0-1)
   (B4 M2p-1) (C#5 M3z-1) (D#5 A4z-1) (E5 5p-1) (F#5 M6p-1) (G#5 M7z-1)
   (A5 0-2) (B5 M2p-2) (C#6 M3z-2) (D#6 A4z-2) (E6 5p-2))))
```

Another example from file [systems/cello/e53/scales.ml](systems/cello/e53/scales.ml) explores the
difference between the scales of D Flat Major Pythagorean and its Just
counterpart, approximated in a [53
edo](https://en.wikipedia.org/wiki/53_equal_temperament) system maped
onto the cello fingerboard.

D Flat Major Pythagorean in a 53 edo system:
```scheme
((IV ((Db2 A1z-e53) (Eb2 m3p-e53) (F2 4p-e53) (Gb2 A4z-e53)))
 (III ((Ab2 A1z-e53) (Bb2 m3p-e53) (C3 4p-e53) (Db3 A4z-e53)))
 (II ((Eb3 A1z-e53) (F3 m3p-e53) (Gb3 M3z-e53) (Ab3 A4z-e53)))
 (I
  ((Bb3 A1z-e53) (C4 m3p-e53) (Db4 M3z-e53) (Eb4 A4z-e53) (F4 m6p-e53)
   (Gb4 M6z-e53) (Ab4 M7z-e53) (Bb4 A1z-e53-1) (C5 m3p-e53-1) (Db5 M3z-e53-1)
   (Eb5 A4z-e53-1) (F5 m6p-e53-1) (Gb5 M6z-e53-1) (Ab5 M7z-e53-1)
   (Bb5 A1z-e53-2) (C6 m3p-e53-2) (Db6 M3z-e53-2) (Eb6 A4z-e53-2)
   (F6 m6p-e53-2))))
```

D Flat Major Just in the same 53 edo system:
```scheme
((IV ((Db2 m2z-e53) (Eb2 m3z-e53) (F2 4p-e53) (Gb2 d5z-e53)))
 (III ((Ab2 m2z-e53) (Bb2 m3p-e53) (C3 4p-e53) (Db3 d5z-e53)))
 (II ((Eb3 m2z-e53) (F3 m3p-e53) (Gb3 M3p-e53) (Ab3 d5z-e53)))
 (I
  ((Bb3 A1z-e53) (C4 m3p-e53) (Db4 M3p-e53) (Eb4 d5z-e53) (F4 m6p-e53)
   (Gb4 M6p-e53) (Ab4 M7p-e53) (Bb4 A1z-e53-1) (C5 m3p-e53-1) (Db5 M3p-e53-1)
   (Eb5 d5z-e53-1) (F5 m6p-e53-1) (Gb5 M6p-e53-1) (Ab5 M7p-e53-1)
   (Bb5 A1z-e53-2) (C6 m3p-e53-2) (Db6 M3p-e53-2) (Eb6 d5z-e53-2)
   (F6 m6p-e53-2))))
```

Example of positions and intervals in the E flat Major double stops
scale in thirds from
[systems/cello/just/thirds.ml](systems/cello/just/thirds.ml):
```
┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
│ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
│ Eb2 │ IV     │ m3z   │ 316   │ G2   │ III    │ 0     │ 0     │ M3 - 5 / 2^2     │ 386   │
│ F2  │ IV     │ 4p    │ 498   │ Ab2  │ III    │ m2z   │ 112   │ m3 - (2 * 3) / 5 │ 316   │
│ G2  │ IV     │ 5p    │ 702   │ Bb2  │ III    │ m3z   │ 316   │ m3 - (2 * 3) / 5 │ 316   │
│ Ab2 │ IV     │ m6z   │ 814   │ C3   │ III    │ 4p    │ 498   │ M3 - 5 / 2^2     │ 386   │
│ Bb2 │ III    │ m3z   │ 316   │ D3   │ II     │ 0     │ 0     │ M3 - 5 / 2^2     │ 386   │
│ C3  │ III    │ 4p    │ 498   │ Eb3  │ II     │ m2z   │ 112   │ m3 - (2 * 3) / 5 │ 316   │
│ D3  │ III    │ 5p    │ 702   │ F3   │ II     │ m3z   │ 316   │ m3 - (2 * 3) / 5 │ 316   │
│ Eb3 │ III    │ m6z   │ 814   │ G3   │ II     │ 4p    │ 498   │ M3 - 5 / 2^2     │ 386   │
│ G3  │ II     │ 4p    │ 498   │ Bb3  │ I      │ m2z   │ 112   │ m3 - (2 * 3) / 5 │ 316   │
│ Ab3 │ II     │ d5z   │ 610   │ C4   │ I      │ m3p   │ 294   │ M3 - 5 / 2^2     │ 386   │
│ Bb3 │ II     │ m6z   │ 814   │ D4   │ I      │ 4p    │ 498   │ M3 - 5 / 2^2     │ 386   │
│ C4  │ II     │ m7p   │ 996   │ Eb4  │ I      │ d5z   │ 610   │ m3 - (2 * 3) / 5 │ 316   │
│ D4  │ II     │ 0-1   │ 1200  │ F4   │ I      │ m6z   │ 814   │ m3 - (2 * 3) / 5 │ 316   │
│ Eb4 │ II     │ m2z-1 │ 1312  │ G4   │ I      │ m7p   │ 996   │ M3 - 5 / 2^2     │ 386   │
│ F4  │ II     │ m3p-1 │ 1494  │ Ab4  │ I      │ d8z   │ 1108  │ m3 - (2 * 3) / 5 │ 316   │
│ G4  │ II     │ 4p-1  │ 1698  │ Bb4  │ I      │ m2z-1 │ 1312  │ m3 - (2 * 3) / 5 │ 316   │
│ Ab4 │ II     │ d5z-1 │ 1810  │ C5   │ I      │ m3p-1 │ 1494  │ M3 - 5 / 2^2     │ 386   │
│ Bb4 │ II     │ m6z-1 │ 2014  │ D5   │ I      │ 4p-1  │ 1698  │ M3 - 5 / 2^2     │ 386   │
│ C5  │ II     │ m7p-1 │ 2196  │ Eb5  │ I      │ d5z-1 │ 1810  │ m3 - (2 * 3) / 5 │ 316   │
│ D5  │ II     │ 0-2   │ 2400  │ F5   │ I      │ m6z-1 │ 2014  │ m3 - (2 * 3) / 5 │ 316   │
│ Eb5 │ II     │ m2z-2 │ 2512  │ G5   │ I      │ m7p-1 │ 2196  │ M3 - 5 / 2^2     │ 386   │
│ F5  │ II     │ m3p-2 │ 2694  │ Ab5  │ I      │ d8z-1 │ 2308  │ m3 - (2 * 3) / 5 │ 316   │
│ G5  │ II     │ 4p-2  │ 2898  │ Bb5  │ I      │ m2z-2 │ 2512  │ m3 - (2 * 3) / 5 │ 316   │
│ Ab5 │ II     │ d5z-2 │ 3010  │ C6   │ I      │ m3p-2 │ 2694  │ M3 - 5 / 2^2     │ 386   │
│ Bb5 │ II     │ m6z-2 │ 3214  │ D6   │ I      │ 4p-2  │ 2898  │ M3 - 5 / 2^2     │ 386   │
│ C6  │ II     │ m7p-2 │ 3396  │ Eb6  │ I      │ d5z-2 │ 3010  │ m3 - (2 * 3) / 5 │ 316   │
└─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘
```

# Code documentation

The tip of the main branch is compiled with odoc and published to
github pages
[here](https://mbarbin.github.io/fingerboard/fingerboard/Fingerboard/index.html).
