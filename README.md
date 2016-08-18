# ComposERL #
Tiny music composition helper implemented in Erlang.

### usage example:

Recently I've created very mellow progression of intervals during playing with my midi keyboard, for introducing new functions into composition I should decide to which key composition currently belongs (for breaking this next :). I extract all notes from midi and feed composerl:

`composerl:key(['C#', 'G#', 'F#', 'F', 'E', 'D#', 'B']).`

and composerl suggest to me bunch of keys:

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

Now I know in which direction composition can flow naturally (In diatonic sense).

Additionally you can ask composerl about notes and chords(triads, sevenths, ninths and eleventh) in any key. Key is record with declaration:

`-record(key, {root :: note(),
	       mode :: mode()}).`

Mode can be number or atom:

 1. Ionian mode (major)
 2. Dorian mode
 3. Phrygian mode
 4. Lydian mode
 5. Mixolydian mode
 6. æolian mode (minor)
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



### Æ
Music is Math.