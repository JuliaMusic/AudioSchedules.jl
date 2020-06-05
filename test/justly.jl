using AudioSchedules: AudioSchedule, Cycles, Envelope, Line, plan_within, @q_str, schedule!, StrictMap
using FileIO: save
import LibSndFile
using Unitful: Hz, s
using Waveforms: sawtoothwave

function justly(chords, the_sample_rate;
    key = 440Hz,
    seconds_per_beat = 1s,
    ramp = 0.1s,
)
    schedule = AudioSchedule()
    clock = 0.0s
    for notes in chords
        ratio, beats = notes[1]
        key = key * ratio
        for (ratio, beats) in notes[2:end]
            schedule!(
                schedule,
                StrictMap(sawtoothwave, Cycles(key * ratio)),
                clock,
                Envelope(
                    (0, 1, 1, 0),
                    (ramp, beats * seconds_per_beat - ramp - ramp, ramp),
                    (Line, Line, Line),
                ),
            )
        end
        clock = clock + beats * seconds_per_beat
    end
    plan_within(schedule, the_sample_rate)
end

cd(@__DIR__)

SONG = [
    [q"1" => 4, q"1" => 4, q"3/2" => 8, q"5/4o1" => 16],
    [q"15/8o-1" => 4, q"1" => 4],
    [q"2/3o1" => 4, q"4/5" => 16, q"2/3o1" => 4],
    [q"4/5" => 4, q"3/2" => 4],
    [q"2/3o1" => 4, q"5/4" => 4, q"o1" => 4],
    [q"3/2o-1" => 4, q"3/2" => 20, q"5/4o1" => 4],
    [q"3/2o-1" => 8, q"5/4" => 8, q"3/2o1" => 8],
    [q"2/3o1" => 4, q"1" => 4, q"5/4o1" => 16],
    [q"15/8o-1" => 4, q"1" => 4],
    [q"2/3o1" => 4, q"4/5" => 8, q"2/3o1" => 4],
    [q"4/5" => 4, q"3/2" => 12],
    [q"3/2o-1" => 2, q"5/4" => 8, q"3/2" => 8, q"7/4o1" => 2],
    [q"1" => 2, q"3/2o1" => 2],
    [q"1" => 2, q"5/4o1" => 2],
    [q"1" => 2, q"o1" => 2],
    [q"2/3o1" => 8, q"1" => 8, q"5/4" => 8, q"3/2" => 8, q"o1" => 8]
]

plan = justly(SONG, 44100Hz, key = 220Hz, seconds_per_beat = 0.4s)
save("test.ogg", read(plan, length(plan)))
