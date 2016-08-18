-module(composerl).
-author("Anton Zhiliuk").

-export([notes/1, chords/2, key/1]).

-type note() :: 'C'|'C#'|'D'|'D#'|'E'|'F'|'F#'|'G'|'G#'|'A'|'A#'|'B'.
-type mode() :: 1..7 | major | minor.
-type interval() :: w | s.
-type chord() :: [note()].

-record(key, { root :: note(),
	       mode :: mode()}).

-spec mode_intervals(mode()) -> [interval()].
mode_intervals(1) -> [w, w, h, w, w, w, h];
mode_intervals(2) -> [w, h, w, w, w, h, w];
mode_intervals(3) -> [h, w, w, w, h, w, w];
mode_intervals(4) -> [w, w, w, h, w, w, h];
mode_intervals(5) -> [w, w, h, w, w, h, w];
mode_intervals(6) -> [w, h, w, w, h, w, w];
mode_intervals(7) -> [h, w, w, h, w, w, w];
mode_intervals(major) -> mode_intervals(1);
mode_intervals(minor) -> mode_intervals(6);
mode_intervals(ionian) -> mode_intervals(1);
mode_intervals(dorian) -> mode_intervals(2);
mode_intervals(phrygian) -> mode_intervals(3);
mode_intervals(lydian) -> mode_intervals(4);
mode_intervals(mixolydian) -> mode_intervals(5);
mode_intervals(aeolian) -> mode_intervals(6);
mode_intervals(locrian) -> mode_intervals(7).

%% return all chromatic semitones from given note.
-spec chromatica(note()) -> [note()].
chromatica('C') ->  ['C','C#','D','D#','E','F','F#','G','G#','A','A#','B'];
chromatica('C#') -> ['C#','D','D#','E','F','F#','G','G#','A','A#','B','C'];
chromatica('D') ->  ['D','D#','E','F','F#','G','G#','A','A#','B','C','C#'];
chromatica('D#') -> ['D#','E','F','F#','G','G#','A','A#','B','C','C#','D'];
chromatica('E') ->  ['E','F','F#','G','G#','A','A#','B','C','C#','D','D#'];
chromatica('F') ->  ['F','F#','G','G#','A','A#','B','C','C#','D','D#','E'];
chromatica('F#') -> ['F#','G','G#','A','A#','B','C','C#','D','D#','E','F'];
chromatica('G') ->  ['G','G#','A','A#','B','C','C#','D','D#','E','F','F#'];
chromatica('G#') -> ['G#','A','A#','B','C','C#','D','D#','E','F','F#','G'];
chromatica('A') ->  ['A','A#','B','C','C#','D','D#','E','F','F#','G','G#'];
chromatica('A#') -> ['A#','B','C','C#','D','D#','E','F','F#','G','G#','A'];
chromatica('B') ->  ['B','C','C#','D','D#','E','F','F#','G','G#','A','A#'].

-spec notes(#key{}) -> [note()].
notes(#key{root = Root, mode = Mode}) ->
    notes(chromatica(Root), mode_intervals(Mode), []).

-spec notes([note()], [interval()], []) -> [note()].
notes(_, [], Acc) ->
    lists:reverse(Acc);
notes([Note1,_Note2|Notes], [w|Intervals], Acc) ->
    notes(Notes, Intervals, [Note1|Acc]);
notes([Note|Notes], [h|Intervals], Acc) ->
    notes(Notes, Intervals, [Note|Acc]).

-spec rotations([note()], 3..6) -> [NoteRotation] when NoteRotation :: [note()].
rotations(List, RotationsCount) ->
    rotations(rotate(2, List), RotationsCount - 1, [List]).

rotations(_, 0, Acc) ->
    lists:reverse(Acc);
rotations(List, RotationsCount, Acc) ->
    rotations(rotate(2, List), RotationsCount - 1, [List|Acc]).

rotate(Num, List) ->
    {List1, List2} = lists:split(Num, List),
    List2 ++ List1.

-spec transpose([list()]) -> [list()].
transpose([List|Lists]) ->
    InitialAcc = lists:map(fun(El) -> [El] end, List),
    Transposition = transpose(Lists, InitialAcc),
    lists:map(fun lists:reverse/1, Transposition).
transpose([], Acc) ->
    Acc;
transpose([List|Lists], Acc) ->
    NewAcc = add_column(List, Acc, []),
    transpose(Lists, NewAcc).

add_column([El|Els], [List|Lists], Acc) ->
    add_column(Els, Lists, [[El|List]|Acc]);
add_column([], [], Acc) -> lists:reverse(Acc).

-spec chords(#key{}, 3..6) -> [chord()].
chords(Key, ChordSize) ->
    Rotations = rotations(notes(Key), ChordSize),
    transpose(Rotations).

-spec all_keys() -> [#key{}].
all_keys() ->
    lists:flatmap(fun(Note) ->
			  lists:map(fun(Mode) ->
					    #key{root = Note, mode = Mode}
				    end,  lists:seq(1, 7))
		  end, chromatica('C')).


match_rate(SampleList, Notes) ->
    Matches = lists:foldl(fun(El, Acc) ->
				  case lists:member(El, SampleList) of
				      true -> Acc + 1;
				      false -> Acc
				  end
			  end, 0, Notes),
    Matches / length(Notes).

-spec key([note()]) -> [{MatchRate, #key{}}] when MatchRate :: float().
key(Notes) ->
    Rates = lists:map(fun(Key) ->
			      {match_rate(Notes, notes(Key)), Key}
		      end, all_keys()),
    FilteredRates = lists:filter(fun({Rate, _}) ->
					 Rate > 0
				 end, Rates),
    lists:sort(fun({Rate1, _}, {Rate2, _}) ->
		       Rate1 > Rate2
	       end, FilteredRates).
