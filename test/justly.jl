using AudioSchedules: AudioSchedule, Cycles, Envelope, Hook, Line, Plan, plan_within, @q_str, repeat!, schedule!, StrictMap
using FileIO: save
import LibSndFile
import SampledSignals
using Unitful: Hz, s
using Waveforms: sawtoothwave

cd("/home/brandon/Music")

const BEAT = 0.7s
const BEATS = 100

function justly(chords, the_sample_rate;
    key = 440Hz,
    seconds_per_beat = 1s,
    ramp = 0.1s,
)
    audio_schedule = AudioSchedule()
    clock = 0.0s
    for notes in chords
        ratio, beats = notes[1]
        key = key * ratio
        for (ratio, beats) in notes[2:end]
            schedule!(
                audio_schedule,
                StrictMap(sawtoothwave, Cycles(key * ratio)),
                clock,
                Envelope(
                    (0, 1, 0),
                    (ramp, beats * seconds_per_beat - ramp),
                    (Line, Hook(-1/s, -1/0.05s))
                ),
            )
        end
        clock = clock + beats * seconds_per_beat
    end
    plan_within(audio_schedule, the_sample_rate)
end

SONG = [
    [1 => 1, 1 => 4],
    [1 => 1, 3/2 => 3],
    [1 => 2, q"5/4o1" => 2],
    [3/4 => 1, 5/4 => 4],
    [1 => 1, q"o1" => 3],
    [1 => 2, q"3/2o1" => 1],
    [q"5/9o1" => 1, 1 => 4],
    [1 => 1, 3/2 => 3],
    [1 => 2, q"6/5o1" => 1],
    [6/5 => 1, q"3/2o-1" => 4],
    [1 => 1, 5/4 => 3],
    [1 => 2, q"o1" => 1],
    [2/3 => 1, 5/4 => 4],
    [1 => 1, q"o1" => 3],
    [1 => 2, q"3/2o1" => 1],
    [3/4 => 1, 3/2 => 4],
    [1 => 1, q"5/4o1" => 3],
    [1 => 2, q"o2" => 1],
    [3/2 => 1, 1 => 4],
    [1 => 1, 3/2 => 3],
    [1 => 2, q"5/4o1" => 1],
    [2/3 => 1, 1 => 4],
    [1 => 1, q"5/4o1" => 3],
    [1 => 2, q"3/2o1" => 1],

    [q"o2" => -32],
    [1 => 1, q"o1" => 1],
    [1 => 0.5, q"5/4" => 0.5],
    [1 => 0.5, q"4/3" => 0.5],
    [1 => 2, q"3/2" => 2],
    [3/4 => 1, q"5/4o1" => 1],
    [1 => 0.5, q"o1" => 0.5],
    [1 => 0.5, q"9/8o1" => 0.5],
    [1 => 2, q"5/4o1" => 2],
    [q"5/9o1" => 1, q"6/5o1" => 1],
    [1 => 0.5, q"3/2" => 0.5],
    [1 => 0.5, q"8/5" => 0.5],
    [1 => 2, q"9/5" => 2],
    [6/5 => 1, 3/2 => 1],
    [1 => 0.5, 5/4 => 0.5],
    [1 => 0.5, 4/3 => 0.5],
    [1 => 2, 3/2 => 2],
    [4/3 => 1, 5/4 => 1],
    [1 => 0.5, 5/4 => 0.5],
    [1 => 0.5, 11/8 => 0.5],
    [1 => 1, 3/2 => 1],
    [1 => 1, 1 => 1],
    [3/4 => 1, 5/4 => 1],
    [1 => 0.5, 5/4 => 0.5],
    [1 => 0.5, 4/3 => 0.5],
    [1 => 2, 3/2 => 2],
    [3/4 => 1, q"o1" => 1],
    [1 => 0.5, q"o1" => 0.5],
    [1 => 0.5, q"9/8o1" => 0.5],
    [1 => 1, q"5/4o1" => 1],
    [1 => 1, 7/4 => 1],
    [4/3 => 4, 5/4 => 4]
]

plan = justly(SONG, 44100Hz, key = 161.81Hz, seconds_per_beat = 0.5s)
save("court.wav", read(plan, length(plan)))
