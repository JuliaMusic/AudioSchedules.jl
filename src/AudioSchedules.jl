module AudioSchedules

import Base:
    eltype,
    empty!,
    iterate,
    IteratorEltype,
    IteratorSize,
    push!,
    show,
    write
using Base: EltypeUnknown, SizeUnknown
using Base.FastMath: sin_fast
using DataStructures: SAIterationState, SortedSet
using InfiniteArrays: Fill, ∞
using MacroTools: @capture
using PortAudio: PortAudioStream, write_buffer
import SampledSignals: nchannels, SampleBuf, samplerate
using Unitful: dB, Hz, s, ustrip

export Hz, s

const FLOAT_HERTZ = typeof(1.0Hz)
const FLOAT_PER_SECOND = typeof(1.0 / s)
const FLOAT_SECONDS = typeof(0.0s)

include("series.jl")

"""
    function Scale(ratio)

A simple wrapper that will multiply inputs by the ratio.

```jldoctest
julia> using AudioSchedules


julia> Scale(3)(2)
6
```
"""
struct Scale{Ratio}
    ratio::Ratio
end

(scale::Scale)(thing) = thing * scale.ratio

precompile(Scale{Float64}, (Float64,))

export Scale

# TODO: use FFT here?
struct SawTooth{overtones} end

"""
    SawTooth(overtones)

Build a saw-tooth wave from its partials, starting with the fundamental (1), up to
`overtones`.

To increase richness but also buziness, increase `overtones`.

```jldoctest
julia> using AudioSchedules


julia> SawTooth(3)(π / 4)
0.9185207636218614
```
"""
SawTooth(overtones) = SawTooth{overtones}()

export SawTooth

const ADJUST = 2 / pi

function (saw::SawTooth{overtones})(an_angle) where {overtones}
    ADJUST * sum(ntuple(let an_angle = an_angle
        function (overtone)
            sin_fast(overtone * an_angle) / overtone
        end
    end, Val{overtones}()))
end

precompile(SawTooth{1}, (Float64,))
precompile(SawTooth{2}, (Float64,))
precompile(SawTooth{3}, (Float64,))
precompile(SawTooth{4}, (Float64,))
precompile(SawTooth{5}, (Float64,))
precompile(SawTooth{6}, (Float64,))
precompile(SawTooth{7}, (Float64,))
precompile(SawTooth{8}, (Float64,))
precompile(SawTooth{9}, (Float64,))
precompile(SawTooth{10}, (Float64,))

export AudioSchedule

struct AudioSchedule
    sample_rate::FLOAT_HERTZ
    # instruments and the time they are first scheduled
    orchestra::Vector{Tuple{Any, FLOAT_SECONDS}}
    # which instruments in the orchestra are currently active
    activated::Vector{Bool}
    # times to turn instruments on and off
    triggers::SortedSet{Tuple{FLOAT_SECONDS, Int}}
end

"""
    AudioSchedule(; sample_rate = 44100Hz)

Create an empty `AudioSchedule`.
You can [`push!`](@ref) new synthesizers to the audio_schedule.
Provide 4 arguments to `push!`: the schedule, the synthesizer, the start time,
and an envelope that you create with [`@envelope`](@ref).

```jldoctest audio_schedule
julia> using AudioSchedules

julia> audio_schedule = AudioSchedule()
0.0 s 44100.0 Hz AudioSchedule

julia> push!(audio_schedule, Map(sin, Cycles(440Hz)), 0s, @envelope(
           0,
           Line => 1s,
           0.2,
           Line => 1s,
           0,
       ))

julia> push!(audio_schedule, Map(sin, Cycles(660Hz)), 2s, @envelope(
            0,
            Line => 1s,
            0.2,
            Line => 1s,
            0,
        ))

julia> audio_schedule
4.0 s^2 44100.0 Hz AudioSchedule
```

You can iterate over an `AudioSchedule`. Each element will just be a vector
of amplitudes.

```jldoctest audio_schedule
julia> length(first(audio_schedule))
44100

julia> collect(AudioSchedule())
Any[]
```

You can use write the audio_schedule directly to a `PortAudio.PortAudioStream`.
The `PortAudioStream` must have exactly 1 output channel, and a matching
sample rate.

```jldoctest audio_schedule
julia> using PortAudio: PortAudioStream


julia> PortAudioStream(0, 1, warn_xruns = false) do stream
           write(stream, audio_schedule)
       end

julia> PortAudioStream(0, 2) do stream
            write(stream, audio_schedule)
        end
ERROR: ArgumentError: PortAudioStream{Float32}
  Samplerate: 44100.0Hz
  2 channel sink: "default" does not have 1 output channel
[...]

julia> PortAudioStream(0, 1, samplerate = 48000) do stream
            write(stream, audio_schedule)
        end
ERROR: ArgumentError: Sample rates of PortAudioStream{Float32}
  Samplerate: 48000.0Hz
  1 channel sink: "default" and 4.0 s^2 44100.0 Hz AudioSchedule do not match
[...]
```

You can save an `AudioSchedule` as a `SampledSignals.SampleBuf`.

```jldoctest audio_schedule
julia> using SampledSignals: SampleBuf


julia> saved = SampleBuf(audio_schedule)
176400-frame, 1-channel SampleBuf{Float64, 1}
4.0s sampled at 44100.0Hz
▃▄▄▄▄▅▅▅▅▅▅▅▅▆▆▆▆▆▆▆▆▆▆▆▆▆▆▅▅▅▅▅▅▅▅▄▄▄▄▃▃▄▄▄▄▅▅▅▅▅▅▅▅▆▆▆▆▆▆▆▆▆▆▆▆▆▆▅▅▅▅▅▅▅▅▄▄▄▄▃
```

You can `empty!` an `AudioSchedule` and reuse it.

```jldoctest audio_schedule
julia> empty!(audio_schedule)

julia> audio_schedule
0.0 s 44100.0 Hz AudioSchedule
```
"""
function AudioSchedule(;
    sample_rate = 44100Hz,
    orchestra = Tuple{Any, FLOAT_SECONDS}[],
    activated = Bool[],
    triggers = SortedSet{Tuple{FLOAT_SECONDS, Int}}()

)
    AudioSchedule(sample_rate, orchestra, activated, triggers)
end

precompile(AudioSchedule, ())

IteratorEltype(::Type{AudioSchedule}) = EltypeUnknown()
IteratorSize(::Type{AudioSchedule}) = SizeUnknown()

function empty!(audio_schedule::AudioSchedule)
    empty!(audio_schedule.orchestra)
    empty!(audio_schedule.activated)
    empty!(audio_schedule.triggers)
    nothing
end

precompile(empty!, (AudioSchedule,))

function samplerate(audio_schedule::AudioSchedule)
    ustrip(Hz, audio_schedule.sample_rate)
end

function nchannels(audio_schedule::AudioSchedule)
    1
end

"""
    duration(audio_schedule::AudioSchedule)

Find the duration of an `AudioSchedule` in seconds.

```jldoctest
julia> using AudioSchedules

julia> audio_schedule = AudioSchedule();

julia> push!(audio_schedule, Map(sin, Cycles(440Hz)), 0s, @envelope(
            0,
            Line => 1s,
            1,
            Line => 1s,
            0,
        ))

julia> duration(audio_schedule)
2.0 s^2
```
"""
function duration(audio_schedule::AudioSchedule)
    triggers = audio_schedule.triggers
    if length(triggers) > 0
        # time from first instrument turned on to last turned off
        (last(triggers)[1] - first(triggers)[1])s
    else
        # zero for an empty schedule
        0.0s
    end
end

precompile(duration, (AudioSchedule,))
export duration

function show(io::IO, audio_schedule::AudioSchedule)
    print(io, "$(duration(audio_schedule)) $(audio_schedule.sample_rate) AudioSchedule")
end

function envelope_macro_pieces(start_level_expression, pair_expression, end_level_expression, the_rest...)
    if !(@capture pair_expression envelopefunction_ => duration_)
        throw(ArgumentError("$pair_expression is not a pair"))
    end
    Expr(:call, (=>),
        Expr(:call, envelopefunction,
            start_level_expression, 
            duration, 
            end_level_expression
        ),
        Expr(:call, float, duration)
    ),
    envelope_macro_pieces(end_level_expression, the_rest...)...
end

function envelope_macro_pieces(_)
    ()
end

function envelope_macro(arguments...)
    Expr(:tuple, envelope_macro_pieces(arguments...)...)
end

"""
    @envelope(arguments...)

Create an envelope.
Start with the start amplitude (a number between 0 and 1).
Then, specify a pair, `segment_function` => `duration`, where `duration` is the duration of the segment.
Then, specify the end amplitude (again, a number between 0 and 1).
So for example,

For example, `@envelope(0, Line => 1s, 1)` will create an envelope with 1 segment.
The segment is created with `segment_function(start_level, duration, end_level) => duration`.
So, for this example, the segment will be `Line(0, 1s, 1) => 1s`.

After you finished your first segment, you can add as many more segments as you'd like.
The end level of the previous segment will be the start level of the next segment.

For example, `@envelope(0, Line => 1s, 1, Line => 1s, 0)` will create an envelope with 2 segments:

1. `Line(0, 1s, 1) => 1s`
2. `Line(1, 1s, 0) => 1s`

```jldoctest
julia> using AudioSchedules

julia> @envelope(0, Line => 1s, 1, Line => 1s, 0)
(Line(0.0, 1.0 s^-1) => 1.0 s, Line(1.0, -1.0 s^-1) => 1.0 s)

julia> @envelope(1, 2, 3)
ERROR: LoadError: ArgumentError: 2 is not a pair
[...]
```
"""
macro envelope(arguments...)
    esc(envelope_macro(arguments...))
end

export @envelope

"""
    push!(audio_schedule::AudioSchedule, synthesizer, start_time,
        start_level, shape => duration, end_level, more_segments...
    )

Add a synthesizer to a [`AudioSchedule`](@ref), where `synthesizer` is anything that supports [`make_series`](@ref),
`start_time` has units of time (like `s`), and the rest of the arguments specify the shape of the envelope.

For all envelope [`segment`](@ref), call

```
segment(shape, start_level, duration, end_level)
```

`duration` should have units of time (like `s`). For example,

```
push!(audio_schedule, synthesizer, start_time, @envelope(0, Line => 1s, 1, Line => 1s, 0))
```

will call segment twice:

```
segment(Line, 0, 1s, 1)
segment(Line, 1, 1s, 0)
```
"""
function push!(audio_schedule::AudioSchedule, (@nospecialize wave), start_time, (@nospecialize envelope))
    triggers = audio_schedule.triggers
    orchestra = audio_schedule.orchestra
    activated = audio_schedule.activated
    time_in_envelope = 0.0s
    for (shape, duration) in envelope
        synthesizer = Map(
            *,
            Skip(wave, time_in_envelope),
            shape,
        )
        push!(orchestra, (synthesizer, start_time + time_in_envelope))
        push!(activated, false)
        instrument_index = length(orchestra)
        push!(triggers, (start_time + time_in_envelope, instrument_index))
        time_in_envelope = time_in_envelope + duration
        push!(triggers, (start_time + time_in_envelope, instrument_index))
    end
    nothing
end

export push!

function iterate(audio_schedule::AudioSchedule)
    iteration = iterate(audio_schedule.triggers)
    # Done if no more triggers
    if iteration === nothing
        nothing
    else
        # Start at 0s
        trigger_iterate(audio_schedule, 0.0s, iteration...)
    end
end

precompile(iterate, (AudioSchedule,))

# TODO: precompile
function iterate(audio_schedule::AudioSchedule, (schedule_time, trigger_state))
    iteration = iterate(audio_schedule.triggers, trigger_state)
    if iteration === nothing
        nothing
    else
        trigger_iterate(audio_schedule, schedule_time, iteration...)
    end
end


precompile(iterate, (AudioSchedule, Tuple{FLOAT_SECONDS, SAIterationState}))

function trigger_iterate(
    audio_schedule,
    schedule_time,
    (trigger_time, instrument_index),
    trigger_state,
)
    sample_rate = audio_schedule.sample_rate
    activated = audio_schedule.activated
    triggers = audio_schedule.triggers
    # pull the rest of the triggers at the current time
    while schedule_time == trigger_time
        activated[instrument_index] = !activated[instrument_index]
        iteration = iterate(triggers, trigger_state)
        if iteration === nothing
            return nothing
        end
        (trigger_time, instrument_index), trigger_state = iteration
    end
    active = view(audio_schedule.orchestra, activated)
    combined = view((
        if isempty(active)
            Fill(0.0, ∞)
        else
            make_series(Map(
                +,
                Iterators.map(
                    let schedule_time = schedule_time
                        function ((instrument, instrument_start_time),)
                            Skip(instrument, schedule_time - instrument_start_time)
                        end
                    end,
                    active
                )...
            ), sample_rate)
        end
    ), 1:round(Int, (trigger_time - schedule_time) * sample_rate))
    # pull the next trigger
    activated[instrument_index] = !activated[instrument_index]
    (combined, (trigger_time, trigger_state))
end

function write_series!(buffer, series, buffer_at)
    buffer_data = buffer.data
    series_at = 0
    series_length = length(series)
    frames_per_buffer = buffer.frames_per_buffer
    left_in_buffer = frames_per_buffer - buffer_at
    if series_length >= left_in_buffer
        buffer_data[:, (buffer_at + 1):frames_per_buffer] .= 
            transpose(view(series, (series_at + 1):(series_at + left_in_buffer)))
        write_buffer(buffer)
        buffer_at = 0
        series_at = series_at + left_in_buffer
        while (series_length - series_at) >= frames_per_buffer
            buffer_data .= 
                transpose(view(series, (series_at + 1):(series_at + frames_per_buffer)))
            write_buffer(buffer)
            buffer_at = 0
            series_at = series_at + frames_per_buffer
        end
        number_left = series_length - series_at
        if series_length - series_at > 0
            buffer_data[:, (buffer_at + 1):number_left] .=
                transpose(view(series, (series_at + 1):series_length))
        end
        number_left
    else
        last_buffer_at = buffer_at + series_length
        buffer_data[:, (buffer_at + 1):last_buffer_at] .= transpose(series)
        last_buffer_at
    end
end

function write(
    stream::PortAudioStream,
    audio_schedule::AudioSchedule
)
    sink_messenger = stream.sink_messenger
    if nchannels(sink_messenger) != 1
        throw(ArgumentError("$stream does not have 1 output channel"))
    end
    if samplerate(stream) != samplerate(audio_schedule)
        throw(ArgumentError("Sample rates of $stream and $audio_schedule do not match"))
    end
    buffer = sink_messenger.buffer
    buffer_at = 0
    for series in audio_schedule
        buffer_at = write_series!(buffer, series, buffer_at)
    end
    if buffer_at > 0
        write_buffer(buffer, buffer_at)
    end
    nothing
end

precompile(write, (PortAudioStream, AudioSchedule))

function SampleBuf(audio_schedule::AudioSchedule)
    SampleBuf(
        reduce(vcat, audio_schedule),
        ustrip(Hz, audio_schedule.sample_rate)
    )
end

precompile(SampleBuf, (AudioSchedule,))

end
