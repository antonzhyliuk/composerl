# ComposERL #

ComposERL is a tiny music composition helper implemented in Erlang. It provides functionality to generate mathematical matches for keys based on a list of notes. Additionally, it offers features to query information about notes, chords (triads, sevenths, ninths, and eleventh), and scales in any key.

### Usage Example:

To find the mathematical matches for keys based on a list of notes, you can use the following example:

`composerl:key(['C#', 'G#', 'F#', 'F', 'E', 'D#', 'B']).`

The result will be a list of key matches with their respective scores:

```
[{0.8571428571428571,{key,'B',5}},
 {0.8571428571428571,{key,'B',4}},
 {0.8571428571428571,{key,'B',1}},
 {0.8571428571428571,{key,'A#',7}},
 {0.8571428571428571,{key,'A#',3}},
 {0.8571428571428571,{key,'A',4}},
 {0.8571428571428571,{key,'G#',6}},
 {0.8571428571428571,{key,'G#',3}},
 {0.8571428571428571,{key,'G#',2}},
 {0.8571428571428571,{key,'F#',5}},
 {0.8571428571428571,{key,'F#',2}},
 {0.8571428571428571,{key,'F#',1}},
 {0.8571428571428571,{key,'F',7}},
 {0.8571428571428571,{key,'E',4}},
 {0.8571428571428571,{key,'E',1}},
 {0.8571428571428571,{key,'D#',7}},
 {0.8571428571428571,{key,'D#',6}},
 {0.8571428571428571,{key,'D#',3}},
 {0.8571428571428571,{key,'C#',6}},
 {0.8571428571428571,{key,'C#',5}},
 {0.8571428571428571,{key,'C#',2}},
 {0.7142857142857143,{key,'B',2}},
 ...
```

Additionally, you can ask ComposERL about notes and chords(triads, sevenths, ninths, and eleventh) in any key. The key is a record:

`-record(key, {root :: note(),
	       mode :: mode()}).`

The mode can be a number or an atom:

 1. Ionian mode (major)
 2. Dorian mode
 3. Phrygian mode
 4. Lydian mode
 5. Mixolydian mode
 6. Aeolian mode (minor)
 7. Locrian mode

```
composerl:notes({key, 'F#', dorian}).
['F#','G#','A','B','C#','D#','E']
```

```
composerl:chords({key, 'F#', 5}, 4).
[['F#','A#','C#','E'],
 ['G#','B','D#','F#'],
 ['A#','C#','E','G#'],
 ['B','D#','F#','A#'],
 ['C#','E','G#','B'],
 ['D#','F#','A#','C#'],
 ['E','G#','B','D#']]
```

### TO DO:
1. Add more scales. (pentatonic, harmonic minor, octatonic, etc...)
2. Output to midi.
3. Tooling for composing chord progressions.
