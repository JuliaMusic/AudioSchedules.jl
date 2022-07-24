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
using Base.Threads: Atomic
using DataStructures: SAIterationState, SortedSet
using InfiniteArrays: Fill, ∞
using MacroTools: @capture
using PortAudio:
    Messenger,
    PortAudioStream,
    SampledSignalsReader,
    SampledSignalsWriter,
    write_buffer
import SampledSignals: nchannels, SampleBuf, samplerate
using Unitful: dB, Hz, s, ustrip

export Hz, s

const DEFAULT_STREAM_TYPE = PortAudioStream{Messenger{Float32, SampledSignalsWriter, Tuple{Matrix{Float32}, Int, Int}, Int}, Messenger{Float32, SampledSignalsReader, Tuple{Matrix{Float32}, Int, Int}, Int}}

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

export AudioSchedule

struct AudioSchedule
    sample_rate::typeof(1.0Hz)
    # instruments and the time they are first scheduled
    orchestra::Vector{Tuple{Any, typeof(0.0s)}}
    # which instruments in the orchestra are currently active
    activated::Vector{Bool}
    # times to turn instruments is_on and off
    triggers::SortedSet{Tuple{typeof(0.0s), Int}}
    is_on::Atomic{Bool}
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
4.0 s 44100.0 Hz AudioSchedule
```

You can iterate over an `AudioSchedule`. Each element will just be a vector
of amplitudes.

```jldoctest audio_schedule
julia> length(first(audio_schedule))
44100

julia> length(collect(audio_schedule))
4

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
ERROR: ArgumentError: PortAudioStream does not have 1 output channel
[...]

julia> PortAudioStream(0, 1, samplerate = 48000) do stream
            write(stream, audio_schedule)
        end
ERROR: ArgumentError: Sample rates of PortAudioStream (48000.0) and AudioSchedule (44100.0) do not match
[...]
```

You can turn an `AudioSchedule` off in real time by setting `is_on[] = false`, for
example,

```jldoctest audio_schedule
julia> using Base.Threads: @spawn

julia> using PortAudio: PortAudioStream

julia> @sync begin
            @spawn PortAudioStream(0, 1, warn_xruns = false) do stream
                write(stream, audio_schedule)
            end
            sleep(2)
            audio_schedule.is_on[] = false
        end;
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
    sample_rate = 44100.0Hz,
    orchestra = Tuple{Any, typeof(0.0s)}[],
    activated = Bool[],
    triggers = SortedSet{Tuple{typeof(0.0s), Int}}(),
    is_on = Atomic{Bool}(true)

)
    AudioSchedule(sample_rate, orchestra, activated, triggers, is_on)
end

precompile(AudioSchedule, ())

IteratorEltype(::Type{AudioSchedule}) = EltypeUnknown()

precompile(IteratorEltype, (Type{AudioSchedule},))

IteratorSize(::Type{AudioSchedule}) = SizeUnknown()

precompile(IteratorSize, (Type{AudioSchedule},))

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

precompile(samplerate, (AudioSchedule,))

function nchannels(audio_schedule::AudioSchedule)
    1
end

precompile(nchannels, (AudioSchedule,))

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
2.0 s
```
"""
function duration(audio_schedule::AudioSchedule)
    triggers = audio_schedule.triggers
    if length(triggers) > 0
        # time from first instrument turned is_on to last turned off
        last(triggers)[1] - first(triggers)[1]
    else
        # zero for an empty schedule
        0.0s
    end
end

precompile(duration, (AudioSchedule,))
export duration

function show(io::IO, audio_schedule::AudioSchedule)
    the_duration = duration(audio_schedule)
    show(io, round(typeof(the_duration), the_duration, digits = 3))
    print(io, " ")
    sample_rate = audio_schedule.sample_rate
    show(io, round(typeof(sample_rate), sample_rate, digits = 3))
    print(io, " AudioSchedule")
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
(Line(0.0, 1.0 s⁻¹) => 1.0 s, Line(1.0, -1.0 s⁻¹) => 1.0 s)

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

precompile(iterate, (AudioSchedule, Tuple{typeof(0.0s), SAIterationState}))

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
    # that is, all triggers less than 1 sample away
    while trigger_time - schedule_time < 1 / sample_rate
        @inbounds activated[instrument_index] = !activated[instrument_index]
        iteration = iterate(triggers, trigger_state)
        if iteration === nothing
            return nothing
        end
        (trigger_time, instrument_index), trigger_state = iteration
    end
    active = @inbounds view(audio_schedule.orchestra, activated)
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
    @inbounds activated[instrument_index] = !activated[instrument_index]
    (combined, (trigger_time, trigger_state))
end

precompile(trigger_iterate, (AudioSchedule, typeof(0.0s), SAIterationState, Tuple{typeof(0.0s), Int}, SAIterationState))

@noinline function write_series!(buffer, series, buffer_at, is_on)
    buffer_data = buffer.data
    series_at = 0
    series_length = length(series)
    frames_per_buffer = buffer.frames_per_buffer
    left_in_buffer = frames_per_buffer - buffer_at
    while (series_length - series_at) >= left_in_buffer
        @inbounds buffer_data[:, (buffer_at + 1):frames_per_buffer] .= 
            transpose(view(series, (series_at + 1):(series_at + left_in_buffer)))
        write_buffer(buffer; acquire_lock = false)
        series_at = series_at + left_in_buffer
        buffer_at = 0
        left_in_buffer = frames_per_buffer - buffer_at
        if !(is_on[])
            return buffer_at, false
        end
    end
    last_buffer_at = buffer_at + series_length - series_at
    @inbounds buffer_data[:, (buffer_at + 1):last_buffer_at] .= 
        transpose(view(series, (series_at + 1):series_length))
    buffer_at = last_buffer_at
    buffer_at, true
end

"""
    compile(stream::PortAudioStream, audio_schedule::AudioSchedule)

Compile an AudioSchedule by only writing the first sample from each segment to
the stream buffer.

```jldoctest
julia> using AudioSchedules

julia> audio_schedule = AudioSchedule();

julia> push!(audio_schedule, Map(sin, Cycles(440Hz)), 0s, @envelope(
            0,
            Line => 1s,
            1,
            Line => 1s,
            0,
        ));

julia> using PortAudio: PortAudioStream

julia> PortAudioStream(0, 1) do stream
            compile(stream, audio_schedule)
        end
```
"""
function compile(stream::PortAudioStream, audio_schedule::AudioSchedule)
    buffer = stream.sink_messenger.buffer
    is_on = audio_schedule.is_on
    for series in audio_schedule
        write_series!(buffer, view(series, 1:1), 0, is_on)
    end
    nothing
end

precompile(compile, (DEFAULT_STREAM_TYPE, AudioSchedule))

export compile

function write(
    stream::PortAudioStream,
    audio_schedule::AudioSchedule
)
    sink_messenger = stream.sink_messenger
    if nchannels(sink_messenger) != 1
        throw(ArgumentError("PortAudioStream does not have 1 output channel"))
    end
    if samplerate(stream) != samplerate(audio_schedule)
        throw(ArgumentError("Sample rates of PortAudioStream ($(samplerate(stream))) and AudioSchedule ($(samplerate(audio_schedule))) do not match"))
    end
    is_on = audio_schedule.is_on
    buffer = sink_messenger.buffer
    buffer_at = 0
    is_on_value = is_on[]
    for series in audio_schedule
        if !is_on_value
            break
        end
        buffer_at, is_on_value = write_series!(buffer, series, buffer_at, is_on)
    end
    if buffer_at > 0
        write_buffer(buffer, buffer_at; acquire_lock = false)
    end
    nothing
end

precompile(write, (DEFAULT_STREAM_TYPE, AudioSchedule))

function SampleBuf(audio_schedule::AudioSchedule)
    SampleBuf(
        reduce(vcat, audio_schedule),
        ustrip(Hz, audio_schedule.sample_rate)
    )
end

precompile(SampleBuf, (AudioSchedule,))

end
