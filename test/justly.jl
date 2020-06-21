using AudioSchedules: compound_wave, Cycles, envelope, Hook, Line, schedule_within, @q_str, Map
using FileIO: save
import LibSndFile
using Unitful: Hz, s
using Waveforms: sawtoothwave

cd("/home/brandon/Music")

function pluck(time; decay = -2.5/s, ramp = 0.005s, peak = 1)
    envelope(0, Line => ramp, peak, Hook(-2.5/s, -1/ramp) => time - ramp, 0)
end

function justly(chords, the_sample_rate;
    key = 440Hz,
    seconds_per_beat = 1s,
    wave = compound_wave(Val(7)),
    make_envelope = pluck
)
    triples = []
    clock = 0.0s
    for notes in chords
        ratio, beats = notes[1]
        key = key * ratio
        for (ratio, beats) in notes[2:end]
            push!(triples, (
                Map(wave, Cycles(key * ratio)),
                clock,
                make_envelope(beats * seconds_per_beat),
            ))
        end
        clock = clock + beats * seconds_per_beat
    end
    schedule_within(triples, the_sample_rate)
end

MAJOR_4 = [
    [1 => 0.5, 1 => 4],
    [1 => 0.5, 3/2 => 3.5],
    [1 => 0.5, 2 => 3],
    [1 => 0.5, 3/2 => 2.5],
    [1 => 0.5, q"5/4o1" => 2],
    [1 => 0.5, 3/2 => 1.5],
    [1 => 0.5, 2 => 1],
    [1 => 0.5, 3/2 => 0.5],
]
MINOR_4 = [
    [1 => 0.5, 1 => 4],
    [1 => 0.5, 3/2 => 3.5],
    [1 => 0.5, 2 => 3],
    [1 => 0.5, 3/2 => 2.5],
    [1 => 0.5, q"6/5o1" => 2],
    [1 => 0.5, 3/2 => 1.5],
    [1 => 0.5, 2 => 1],
    [1 => 0.5, 3/2 => 0.5],
]
MAJOR_2 = [
    [1 => 0.5, 1 => 2],
    [1 => 0.5, 3/2 => 1.5],
    [1 => 0.5, q"5/4o1" => 1],
    [1 => 0.5, 3/2 => 0.5],
]
MINOR_2 = [
    [1 => 0.5, 1 => 2],
    [1 => 0.5, 3/2 => 1.5],
    [1 => 0.5, q"6/5o1" => 1],
    [1 => 0.5, 3/2 => 0.5],
]
SONG = [
    [1 => 0], # Alas my love you
    MINOR_4...,
    [q"9/5o-1" => 0], # do me wrong to
    MAJOR_4...,
    [q"5/9o1" => 0], # cast me off so
    MINOR_4...,
    [q"3/2o-1" => 0], # courteously; And
    MAJOR_4...,
    [q"4/5o1" => 0], # I have loved you
    MAJOR_4...,
    [q"3/2o-1" => 0], # oh so long, de-
    MAJOR_4...,
    [q"5/9o1" => 0], # lighting
    MINOR_2...,
    [q"3/2o-1" => 0], # in your
    MAJOR_2...,
    [q"2/3o1" => 0], # company.
    MINOR_4...,
    [q"6/5" => 0], # Greensleeves was
    MAJOR_4...,
    [q"3/2o-1" => 0], # my delight,
    MAJOR_4...,
    [q"5/9o1" => 0], # Greensleeves my
    MINOR_4...,
    [q"3/2o-1" => 0], # heart of gold
    MAJOR_4...,
    [q"4/5o1" => 0], # Greensleeves my
    MAJOR_4...,
    [q"3/2o-1" => 0], # heart of joy
    MAJOR_4...,
    [q"5/9o1" => 0], # who but my
    MINOR_2...,
    [q"3/2o-1" => 0], # lady
    MAJOR_2...,
    [q"2/3o1" => 0], # Greensleeves
    MINOR_4...
]

a_schedule = justly(SONG, 44100Hz, key = 176Hz, seconds_per_beat = 0.4s)
read(a_schedule, length(a_schedule))
