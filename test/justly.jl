using PortAudio: PortAudioStream
using Unitful: Hz, s
import JSON

function make_envelope(duration, ramp = 0.05s)
    Envelope((0, 0.1, 0.1, 0), (ramp, duration - ramp - ramp, ramp), (Line, Line, Line))
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
                StrictMap(sin, Cycles((key * make_interval(note)))),
                clock,
                make_envelope(note["beats"] * seconds_per_beat),
            )
        end
        clock = clock + modulation["beats"] * seconds_per_beat
    end
end

const SAMPLE_RATE = 44100Hz
PortAudioStream(samplerate = SAMPLE_RATE / Hz) do stream
    a_schedule = AudioSchedule(SAMPLE_RATE)
    justly!(a_schedule, JSON.parsefile(joinpath(@__DIR__, "all_i_have_to_do_is_dream.json")), 440Hz, 1.25s)
    a_plan = plan!(a_schedule)
    write(stream.sink, a_plan, length(a_plan))
end
