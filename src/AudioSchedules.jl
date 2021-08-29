module AudioSchedules

import Base:
    eltype,
    iterate,
    IteratorEltype,
    IteratorSize,
    show,
    write
using Base: EltypeUnknown, HasEltype, IsInfinite, IteratorEltype, IteratorSize, @propagate_inbounds, SizeUnknown
using Base.FastMath: sin_fast
using Base.Iterators: repeated, Stateful
using Base.Meta: ParseError
using DataStructures: SortedSet
using Interpolations: CubicSplineInterpolation
using NLsolve: nlsolve
using PortAudio: Messanger, PortAudioStream, Scribe, write_buffer
import PortAudio: get_input_type, get_output_type
using RegularExpressions: capture, CONSTANTS, of, pattern, raw, short
import SampledSignals: nchannels, SampleBuf, samplerate
using Unitful: dB, Hz, μPa, s

const FREQUENCY = typeof(1.0Hz)
const RATE = typeof(1.0 / s)

include("miscellaneous.jl")
include("iterators.jl")

export AudioSchedule

const ORCHESTRA = Vector{Any}
const ACTIVATED = Vector{Bool}
const TRIGGERS = SortedSet{Tuple{typeof(1.0s), Int}}

struct AudioSchedule
    sample_rate::FREQUENCY
    orchestra::ORCHESTRA
    activated::ACTIVATED
    triggers::TRIGGERS
end

export AudioSchedule

"""
    AudioSchedule(; sample_rate = 44100Hz)

Create an empty `AudioSchedule`.
You can [`add!`](@ref) new synthesizers to the schedule.
Specify a `sample_rate` with units per time, like `1/s` or `Hz`.

```jldoctest audio_schedule
julia> using AudioSchedules


julia> using Unitful: s, Hz


julia> a_schedule = AudioSchedule()
0.0 s 44100.0 Hz AudioSchedule

julia> add!(a_schedule, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 0.05, Line => 1s, 1, Line => 1s, 0)

julia> a_schedule
3.0 s 44100.0 Hz AudioSchedule
```

You can use write the schedule directly to a `PortAudioStream`.
However, to do so, the `PortAudioStream` must have a [`Weaver`](@ref) writer.
The `PortAudioStream` must have exactly 0 input channels, 1 output channel, and a matching
sample rate.

```jldoctest audio_schedule
julia> using PortAudio: PortAudioStream

julia> PortAudioStream(0, 1, writer = Weaver()) do stream
            write(stream, a_schedule)
        end

julia> a_schedule
0.0 s 44100.0 Hz AudioSchedule
```

After you play an AudioSchedule, it will be empty again.
You can save it as a `SampledSignals.SampleBuf` if you want to play it again.

```jldoctest audio_schedule
julia> using SampledSignals: SampleBuf

julia> new_schedule = AudioSchedule();

julia> add!(new_schedule, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 0.05, Line => 1s, 1, Line => 1s, 0)

julia> saved = SampleBuf(new_schedule)
132300-frame, 1-channel SampleBuf{Float64, 2}
3.0s sampled at 44100.0Hz
▁▂▂▃▃▃▃▃▃▃▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▅▅▅▆▆▆▆▆▆▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▆▆▆▆▆▆▅▅▄

julia> PortAudioStream(0, 1) do stream
            write(stream, saved)
        end
132300
```
"""
AudioSchedule(; sample_rate = 44100Hz) = AudioSchedule(sample_rate, ORCHESTRA(), ACTIVATED(), TRIGGERS())

IteratorSize(::Type{AudioSchedule}) = SizeUnknown()
IteratorEltype(::Type{AudioSchedule}) = EltypeUnknown()

samplerate(schedule::AudioSchedule) = schedule.sample_rate / Hz
nchannels(schedule::AudioSchedule) = 1

function duration(schedule::AudioSchedule)
    triggers = schedule.triggers
    if length(triggers) > 0
        last(triggers)[1] - first(triggers)[1]
    else
        0.0s
    end
end

function show(io::IO, schedule::AudioSchedule)
    print(io, "$(duration(schedule)) $(schedule.sample_rate) AudioSchedule")
end

function make_envelope(start_level, (shape, duration), end_level, more_segments...)
    segments(shape, start_level, duration, end_level)...,
    make_envelope(end_level, more_segments...)...
end
function make_envelope(_)
    ()
end

function add_iterator!(schedule::AudioSchedule, iterator, start_time, duration)
    triggers = schedule.triggers
    orchestra = schedule.orchestra
    push!(orchestra, iterator)
    push!(schedule.activated, false)
    index = length(orchestra)
    push!(triggers, (start_time, index))
    push!(triggers, (start_time + duration, index))
    nothing
end

function triples(sample_rate, synthesizer, start_time, envelope...)
    time_box = Ref(start_time * 1.0)
    stateful_wave = Stateful(make_iterator(synthesizer, sample_rate))
    # it doesn't help to use an iterator here
    # because this is a tuple and so can be optimized away
    map(
        function ((shape_synthesizer, duration),)
            time = time_box[]
            time_box[] = time + duration
            (
                Iterators.map(
                    *,
                    stateful_wave,
                    Stateful(make_iterator(shape_synthesizer, sample_rate)),
                ),
                time,
                duration,
            )
        end,
        make_envelope(envelope...),
    )
end

"""
    add!(schedule::AudioSchedule, synthesizer, start_time,
        start_level, shape => duration, end_level, more_segments...
    )

Add a synthesizer to a [`AudioSchedule`](@ref), where `synthesizer` is anything that supports [`make_iterator`](@ref),
`start_time` has units of time (like `s`), and the rest of the arguments specify the shape of the envelope.

For all envelope [`segments`](@ref), call

```
segments(shape, start_level, duration, end_level)
```

`duration` should have units of time (like `s`). For example,

```
add!(schedule, synthesizer, start_time, 0, Line => 1s, 1, Line => 1s, 0)
```

will call segments twice:

```
segments(Line, 0, 1s, 1)
segments(Line, 1, 1s, 0)
```
"""
function add!(schedule::AudioSchedule, synthesizer, start_time, piece_1, rest...)
    sample_rate = schedule.sample_rate
    for (shaped, time, duration) in
        triples(sample_rate, synthesizer, start_time, piece_1, rest...)
        add_iterator!(schedule, shaped, time, duration)
    end
    nothing
end

export add!

function iterate(schedule::AudioSchedule)
    triggers = schedule.triggers
    if isempty(triggers)
        nothing
    else
        inner_iterate(schedule, 0.0s, pop!(triggers)...)
    end
end
function iterate(schedule::AudioSchedule, (time, index))
    triggers = schedule.triggers
    if isempty(triggers)
        nothing
    else
        activated = schedule.activated
        activated[index] = !activated[index]
        inner_iterate(schedule, time, pop!(triggers)...)
    end
end

function inner_iterate(schedule, time, trigger_time, index)
    triggers = schedule.triggers
    activated = schedule.activated
    while time == trigger_time
        activated[index] = !activated[index]
        if isempty(triggers)
            return nothing
        end
        (trigger_time, index) = pop!(triggers)
    end
    (
        Iterators.map(+, view(schedule.orchestra, activated)...),
        round(Int, (trigger_time - time) * schedule.sample_rate),
    ),
    (trigger_time, index)
end

# TODO: reset!
# TODO: make a SampledSignal without sacrificing performance
# TODO: figure out a way to avoid peaking !!!

"""
    struct Weaver <: Scribe end

A special `PortAudio` `Scribe`.
You can use a `Weaver` to write an [`AudioSchedule`](@ref) directly to your speakers.
"""
struct Weaver <: Scribe end

get_input_type(::Weaver, _) = Nothing
get_output_type(::Weaver, _) = Nothing

export Weaver

# offset is the difference between iterator pseudo-indices and buffer indices
function fill_buffer!(iterator, buffer, a_range = eachindex(buffer.data))
    # TODO: most of the time this will be the same range
    # is there an extra optimization we can make here?
    for index in a_range
        # TODO: the number of rows is small and constant
        # is there an extra optimization we can make here?
        @inbounds buffer.data[index] = iterate(iterator)[1]
    end
end

@noinline function write_stateful!(stateful, stateful_to, buffer, buffer_from)
    frames_per_buffer = buffer.frames_per_buffer
    full = buffer_from + stateful_to
    if full < frames_per_buffer
        # the buffer is ahead of the iterator, so we start with a negative offset
        fill_buffer!(stateful, buffer, (buffer_from + 1):full)
        full
    else
        fill_buffer!(stateful, buffer, (buffer_from + 1):frames_per_buffer)
        write_buffer(buffer)
        buffers, buffer_to = divrem(stateful_to - (frames_per_buffer - buffer_from), frames_per_buffer)
        for _ in 1:buffers
            fill_buffer!(stateful, buffer)
            write_buffer(buffer)
        end
        fill_buffer!(stateful, buffer, 1:buffer_to)
        buffer_to
    end
end

function write(buffer::SampleBuf, schedule::AudioSchedule)
    if nchannels(buffer) != 1
        throw(ArgumentError("$buffer does not have 1 channel"))
    end
    if samplerate(buffer) != samplerate(schedule)
        throw(ArgumentError("Sample rates of $buffer and $schedule do not match"))
    end
    for (stateful, stateful_to) in schedule
        buffer_at = write_stateful!(stateful, stateful_to, buffer, buffer_at)
    end
end

function write(stream::PortAudioStream{<:Messanger{<:Any, Weaver}, <:Any}, schedule::AudioSchedule)
    if nchannels(stream.source_messanger) != 0
        throw(ArgumentError("$stream does not have 0 input channels"))
    end
    sink_messanger = stream.sink_messanger
    if nchannels(sink_messanger) != 1
        throw(ArgumentError("$stream does not have 1 output channel"))
    end
    if samplerate(stream) != samplerate(schedule)
        throw(ArgumentError("Sample rates of $stream and $schedule do not match"))
    end
    buffer = sink_messanger.buffer
    buffer_at = 0
    for (stateful, stateful_to) in schedule
        buffer_at = write_stateful!(stateful, stateful_to, buffer, buffer_at)
    end
    write_buffer(buffer, buffer_at)
    nothing
end

@noinline function write_stateful!(stateful, stateful_to, buffer::SampleBuf, buffer_from)
    goal = buffer_from + stateful_to
    fill_buffer!(stateful, buffer, (buffer_from + 1):goal)
    goal
end

function SampleBuf(schedule::AudioSchedule)
    collection = collect(schedule)
    number = sum(Iterators.map(
        pair -> pair[2],
        collection
    ))
    buffer = SampleBuf(Float64, samplerate(schedule), number, 1)
    buffer_at = 0
    for (stateful, stateful_to) in collection
        buffer_at = write_stateful!(stateful, stateful_to, buffer, buffer_at)
    end
    buffer
end

include("equal_loudness.jl")

end
