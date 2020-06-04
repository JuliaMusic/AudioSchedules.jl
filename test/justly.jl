using AudioSchedules
using FileIO: save
using JSON: parsefile
import LibSndFile
using Unitful: Hz, s
using Waveforms: sawtoothwave

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
                equal_loudness(StrictMap(sawtoothwave, Cycles((key * make_interval(note))))),
                clock,
                make_envelope(note["beats"] * seconds_per_beat),
            )
        end
        clock = clock + modulation["beats"] * seconds_per_beat
    end
end

function justly(filename; sample_rate = 44100Hz, key = 440Hz, seconds_per_beat = 1s)
    schedule = AudioSchedule()
    justly!(schedule, parsefile(string(filename, ".json")), key, seconds_per_beat)
    plan = plan_within(schedule, sample_rate)
    save(string(filename, ".ogg"), read(plan, length(plan)))
end

justly("all_i_have_to_do_is_dream", seconds_per_beat = 1.25s)
