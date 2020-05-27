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
    chord = JSON.parse(song)[2]
    for chord in JSON.parse(song)
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
a_schedule = AudioSchedule(SAMPLE_RATE)
justly!(a_schedule, read("all_i_have_to_do_is_dream.json", String), 440Hz, 2s)
a_plan = Plan(a_schedule)
stream = PortAudioStream(samplerate = SAMPLE_RATE / Hz)
write(stream.sink, a_plan, length(a_plan))
