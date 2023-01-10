# Fingerboard

[![Actions Status](https://github.com/mbarbin/fingerboard/workflows/CI/badge.svg)](https://github.com/mbarbin/fingerboard/actions/workflows/ci.yml)
[![Deploy odoc Actions Status](https://github.com/mbarbin/fingerboard/workflows/Deploy-odoc/badge.svg)](https://github.com/mbarbin/fingerboard/actions/workflows/deploy-odoc.yml)

This project is meant to help me reason about and build a "microtonal
geography of the cello fingerboard".

# A bit of history about the contents

In 2005, I spent a great number of hours practicing double stops on
the cello. As a result, I started researching contents on acoustic
intervals and temperaments, as they relate to intonation. I summarized
some findings and information gathered around that time in some
personal notes, as I was trying to understand, hear and practice the
use of different intonation systems (equal, pythagorean and just).

In the years that followed, I spent less time thinking about this
topic. A few years ago (perhaps 2018) I discovered a book called
CelloMind, by Hans Jorgen Jensen and Minna Rose Chung. The first part
of the book motivated me to revive my notes, and turn them into a
format that I could more easily verify and share. In 2022 I had the
idea of turning them into a piece of software that I could easily
improve over time, and this repo was born.

# What's in the repo?

The repo implements the tools necessary to model microtonal geography
systems based on various scales. As we go over some specific
pythagorean, just and tempered scales and double stops, some specific
notations are introduced for each of the fingerboard locations
required to play each note. We then can reason about and exhibit
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

# Motivations

## Programmatic representation

While going over my notes again in 2022 I realized that there were
quite a bit of typos and mistakes in them. One advantage of using a
programmatic representation is that the contents is checked in a
systematic fashion, and while it is possible that there are bugs in
the code, these defects can be flushed out and fixed overtime.

## Comparison with the contents of CelloMind

I was quite impressed by the book and wanted to understand which parts
were new to me, and which were overlapping with information that I had
already been able to gather from other sources.

## Publishing the contents

I figured that I would want sometimes to be able to refer to the
contents of this repo, either for my personal use or in conversations
with cellists or other musicians, as a quick online reference.

# Roadmap

I don't have a precise plan in mind at the moment, although there are
some aspects that I haven't had a chance to integrate and would like
to, such as showing precisely the accoustic intervals involved in
double stops with the various systems.

There are also some other systems that I'd like to incorporate, while
making the contents more easily understandable by a larger audience.
Please let me know if you have an interest for this repo.

# Code documentation

The tip of the main branch is compiled with odoc and published to
github pages
[here](https://mbarbin.github.io/fingerboard/fingerboard/Fingerboard/index.html).
