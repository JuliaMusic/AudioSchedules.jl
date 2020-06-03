using AudioSchedules
using FileIO: save
using JSON: parsefile
import LibSndFile
using Unitful: Hz, s

function make_envelope(duration, level = 1, ramp = 0.05s)
    Envelope((0, level, level, 0), (ramp, duration - ramp - ramp, ramp), (Line, Line, Line))
end

function make_interval(note)
    note["numerator"] / note["denominator"] * 2.0^note["octave"]
end

function justly!(schedule, song, key, seconds_per_beat)
    clock = 0.0s
    for chord in song
        notes = chord["notes"]
        modulation = notes[1]
        key = key * make_interval(modulation)
        for note in notes[2:end]
            schedule!(
                schedule,
                equal_loudness(StrictMap(sin, Cycles((key * make_interval(note))))),
                clock,
                make_envelope(note["beats"] * seconds_per_beat),
            )
        end
        clock = clock + modulation["beats"] * seconds_per_beat
    end
end

function justly(filename; sample_rate = 44100Hz, key = 440Hz, seconds_per_beat = 1s)
    test_schedule = AudioSchedule(sample_rate)
    parsed = parsefile(string(filename, ".json"))
    justly!(test_schedule, parsed, key, seconds_per_beat)
    final = AudioSchedule(sample_rate)
    justly!(final, parsed, key, seconds_per_beat)
    final_plan = plan!(final)
    readjust!(plan!(test_schedule), final_plan)
    save(string(filename, ".ogg"), read(final_plan, length(final_plan)))
end

justly("test", seconds_per_beat = 0.25s)
