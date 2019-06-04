##  G56

G43 and G56 are proposed encodings of binary data into ASCII.  They are
analogous to [G86](https://github.com/galenhuntington/g86), with
the differences that they use smaller character sets, particularly
alphanumeric strings, and they are more experimental.  See the
description of G86 for the general idea.

These two encodings fill the gap between base 32, which is wasteful
if case sensitivity can be used, and base 64, which requires
non-alphanumeric ASCII characters.

### G43

G43 is very similar to G86.  The bytes are divided into chunks of
2, which are interpreted as base-258 digits.  The integer value is
then written in base 43.  As 43³ ⩾ 256², this allows every 2
bytes to be encoded as 3 characters from a 43-size set, for a 50%
length increase.

For the character set, I tentatively propose the digits, the uppercase
`BCDFGHJKLMPQVWXYZ`, and the same lowercase excluding `l`.

G43 is a simpler encoding than G56, but it would be unusual for this
gain in simplicity to be all that helpful, or that a character set
of size 43, but less than 56, is needed.  For this reason, from here
on only G56 is considered.

I also developed a medium-simple encoding G54, but it is hard to
imagine it being needed over G56.

### Outline (v0.1)

G56 expands chunks of five bytes to seven characters, for a 40%
size increase.  The character set used is all ASCII digits, uppercase
letters, and lowercase letters, in that order, with the exception of
`IOU` of both cases.

Consider a chunk _abcde_ of five bytes, thought of as numbers 0–255,
and construct the integer

12·56⁵·_a_ + 2·56⁴·_b_ + 24·56²·_c_ + 5·56·_d_ + _e_.

Write this integer in base 56 (big-endian), using the above character
set to represent “digits” 0–55, zero-padded on the left if needed
to make exactly seven characters, to get the encoding into ASCII.

For a final chunk of less than five bytes, pad bytes of value zero on
the end to bring it up to five bytes, and then for the final encoding
remove 1, 2, 4, or 5 `0` characters from the end, according as whether
the number of bytes padded on was 1, 2, 3, or 4.

Although larger integers are used than in G43 and G86, all calculations
can be comfortably done within 64-bit registers.  A message of _n_
bytes is encoded as _n_+⌈2&#xfeff;_n_/5⌉ characters.  As in G86,
it preserves lexicographic order, and has the initial segment property.

The encoding is reversible because each coefficient exceeds the next by
a factor of at least 256.  We also see the total cannot exceed 56⁷.

### Examples

The input `Hello, world!` (as bytes) is length 13, so it will be
encoded in three chunks, with the last padded with two zero bytes.
Plugging into the above formula, we get the integers:

```
477826981519; 291424773082; 715717764608.
```

The result has two `0`s on the end, which we remove, to yield the
encoding:

```
FTbpRez9R8v9x2PBZE8
```

The G56 encoding of the first 256 bits of π in binary:

```
7kEndAMbwHbRgRWad1S23Eer3x0b8sfMJZ8A0x9W5NPCt
```

A 128-bit binary string will be encoded as 23 characters, which is
competitive with UUIDs at 36 characters.

### Future work and variations

The encoding of bytes into an integer can be done in a few ways while
still having the intended properties.  Specifically, the 24 on _c_
could also be 23, and the 12 on _a_ could be either 10 or 11, for
six possible encodings.  The choice is basically arbitrary, but I
chose 24 and 12 because they are what one might call nice numbers,
and also larger numbers “fill out” the target space more.

The choice of characters is similar to that in Crockford Base 32,
except of course both cases are used, and `L` is included.  This
removes letters most likely to be confused with other characters,
and also helps avoid spelling words, particularly undesirable ones.

So, while there are some degrees of freedom, the current choices are
likely optimal.

### Implementation

As in G86, a reference implementation in Haskell is included, which
is also not streaming:

```bash
g56 < data.bin > data.txt
g56 -d < data.txt > data.bin
```

Either `cabal install` or `stack install` can be used to build.
