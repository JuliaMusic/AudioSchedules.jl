module AudioSchedules

import Base: eltype, iterate, IteratorEltype, IteratorSize, length, read!, setindex!, show
using Base: Generator, EltypeUnknown, IsInfinite, HasEltype, HasLength, RefValue
using Base.Iterators: repeated
using DataStructures: OrderedDict
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: Hz, s, SampleSource
const TAU = 2 * pi

mutable struct IteratorSource{Iterator} <: SampleSource
    iterator::Iterator
    state::Any
    nchannels::Int
    samplerate::Float64
end

@inline function IteratorSource(iterator, samplerate; nchannels = 1)
    # TODO: save first item
    _, state = iterate(iterator)
    IteratorSource(iterator, state, nchannels, samplerate)
end

@inline eltype(source::IteratorSource) = Float64

@inline nchannels(source::IteratorSource) = source.nchannels

@inline samplerate(source::IteratorSource) = source.samplerate

# TODO: introduce more function barriers
@inline function unsafe_read!(source, buf, frameoffset, framecount)
    iterator = source.iterator
    state = source.state
    for index in eachindex(buf)
        result = iterate(iterator, state)
        if result === nothing
            source.state = state
            return index - 1
        else
            item, state = result
            @inbounds buf[index] = item
        end
    end
    source.state = state
    return length(buf)
end

@inline function iterate_no_nothing(something, state...)
    result = iterate(something, state...)
    if result === nothing
        error("Unexpected end of iterator")
    end
    result::Tuple{Any,Any}
end

struct FlattenedIterator{OuterIterator}
    outer_iterator::OuterIterator
end

@inline IteratorSize(::Type{<:FlattenedIterator}) = HasLength()

@inline function length(flattened::FlattenedIterator)
    sum(Generator((@inline function ((_, _, samples),)
        samples
    end), flattened.outer_iterator))
end

# TODO: think about this
@inline IteratorEltype(::Type{<:FlattenedIterator}) = EltypeUnknown()

function iterate(flattened::FlattenedIterator)
    outer_iterate(flattened)
end
function iterate(flattened::FlattenedIterator, state)
    inner_iterate(flattened, state)
end

function outer_iterate(flattened, state...)
    outer_result = iterate(flattened.outer_iterator, state...)
    if outer_result === nothing
        nothing
    else
        (inner_iterator, state_boxes, has_left), outer_state = outer_result
        inner_state = map(getindex, state_boxes)
        inner_iterate(
            flattened,
            (outer_state, inner_iterator, has_left, state_boxes, inner_state),
        )
    end
end

function inner_iterate(
    flattened,
    (outer_state, inner_iterator, has_left, state_boxes, inner_state),
)
    if has_left > 0
        has_left = has_left - 1
        inner_item, inner_state = iterate_no_nothing(inner_iterator, inner_state)
        return inner_item, (outer_state, inner_iterator, has_left, state_boxes, inner_state)
    else
        map(setindex!, state_boxes, inner_state)
        outer_iterate(flattened, outer_state)
    end
end

"""
    abstract type Synthesizer

Synthesizers need only support [`make_iterator`](@ref).
"""
abstract type Synthesizer end
export Synthesizer

"""
    make_iterator(synthesizer, samplerate)

Return an iterator that will the play the `synthesizer` at a given `samplerate`
"""
@inline make_iterator(synthesizer, samplerate) = synthesizer
export make_iterator

struct InfiniteMapIterator{AFunction,Iterators}
    a_function::AFunction
    iterators::Iterators
end

@inline IteratorSize(::Type{<:InfiniteMapIterator}) = IsInfinite

@inline IteratorEltype(::Type{<:InfiniteMapIterator}) = EltypeUnknown

@inline function iterate(something::InfiniteMapIterator, states...)
    items_states = map(iterate_no_nothing, something.iterators, states...)
    something.a_function(map(first, items_states)...), map(last, items_states)
end

"""
    InfiniteMap(a_function, synthesizers...)

Map `a_function` over `synthesizers`, assuming that none of the `synthesizers` will end
early.
"""
struct InfiniteMap{AFunction,Synthesizers} <: Synthesizer
    a_function::AFunction
    synthesizers::Synthesizers
    InfiniteMap(a_function::AFunction, synthesizers...) where {AFunction} =
        new{AFunction,typeof(synthesizers)}(a_function, synthesizers)
end
export InfiniteMap

@inline function make_iterator(a_map::InfiniteMap, samplerate)
    InfiniteMapIterator(
        a_map.a_function,
        map((
            let samplerate = samplerate
                @inline function (synthesizer)
                    make_iterator(synthesizer, samplerate)
                end
            end
        ), a_map.synthesizers),
    )
end

struct LineIterator
    start::Float64
    plus::Float64
end

@inline IteratorSize(::Type{LineIterator}) = IsInfinite

@inline IteratorEltype(::Type{LineIterator}) = HasEltype

@inline eltype(::Type{LineIterator}) = Float64

@inline function iterate(line::LineIterator, state = line.start)
    state, state + line.plus
end

"""
    Line(start_value, end_value, duration)

A line from `start_value` to `end_value` that lasts for `duration`.
"""
struct Line <: Synthesizer
    start_value::Float64
    end_value::Float64
    duration::Float64
    @inline Line(start_value, end_value, duration) =
        new(start_value, end_value, duration / s)
end
export Line

@inline function make_iterator(line::Line, samplerate)
    start_value = line.start_value
    LineIterator(start_value, (line.end_value - start_value) / (samplerate * line.duration))
end

struct CyclesIterator
    start::Float64
    plus::Float64
end

@inline IteratorSize(::Type{CyclesIterator}) = IsInfinite

@inline IteratorEltype(::Type{CyclesIterator}) = HasEltype

@inline eltype(::Type{CyclesIterator}) = Float64

@inline function iterate(ring::CyclesIterator, state = ring.start) where {Element}
    next_state = state + ring.plus
    if next_state >= TAU
        next_state = next_state - TAU
    end
    state, next_state
end

"""
    Cycles(frequency)

Cycles from 0 2π to repeat at a `frequency`.
"""
struct Cycles <: Synthesizer
    frequency::Float64
    @inline Cycles(frequency) = new(frequency / Hz)
end

export Cycles

@inline function make_iterator(cycles::Cycles, samplerate)
    CyclesIterator(0, cycles.frequency / samplerate * TAU)
end

"""
    Envelope(levels, durations, shapes)

Shapes are all functions which return [`Synthesizer`](@ref)s:

```
shape(start_value, end_value, duration) -> Synthesizer
```

`durations` and `levels` list the time and level of the boundaries of segments of the
envelope. For example,

```
Envelope([0.0, 1.0, 1.0, 0.0], [.05 s, 0.9 s, 0.05 s], [Line, Line, Line])
```

will create an envelope with three segments:

```
Line(0.0, 1.0, 0.05 s)
Line(1.0, 1.0, 0.9 s)
Line(s1.0, 0.0, 0.05 s)
```

See the example for [`AudioSchedule`](@ref).
"""
struct Envelope{Levels,Durations,Shapes}
    levels::Levels
    durations::Durations
    shapes::Shapes
    @inline function Envelope(
        levels::Levels,
        durations::Durations,
        shapes::Shapes,
    ) where {Levels,Durations,Shapes}
        @assert length(durations) == length(shapes) == length(levels) - 1
        new{Levels,Durations,Shapes}(levels, durations, shapes)
    end
end
export Envelope

mutable struct Instrument{Iterator,State}
    iterator::Iterator
    state_box::Ref{State}
    is_on::Bool
end

mutable struct AudioSchedule{Sink}
    orchestra::Dict{Symbol,Instrument}
    triggers::OrderedDict{Float64,Vector{Tuple{Symbol,Bool}}}
    sink::Sink
    consumed::Bool
end

"""
    AudioSchedule(sink)

Create a `AudioSchedule` to schedule changes to sink.

```jldoctest schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> using PortAudio: PortAudioStream

julia> stream = PortAudioStream(samplerate = 44100);

julia> a_schedule = AudioSchedule(stream.sink)
AudioSchedule with triggers at () seconds
```

Add a synthesizer to the schedule with [`schedule!`](@ref). You can schedule for a duration
in seconds, or use an [`Envelope`](@ref).

```jldoctest schedule
julia> envelope = Envelope((0, 0.25, 0), (0.05s, 0.95s), (Line, Line));

julia> schedule!(a_schedule, InfiniteMap(sin, Cycles(440Hz)), 0s, envelope)

julia> schedule!(a_schedule, InfiniteMap(sin, Cycles(440Hz)), 1s, envelope)

julia> schedule!(a_schedule, InfiniteMap(sin, Cycles(550Hz)), 1s, envelope)

julia> a_schedule
AudioSchedule with triggers at (0.0, 0.05, 1.0, 1.05, 2.0) seconds
```

Then, you can play it with [`play`](@ref).

```jldoctest schedule
julia> play(a_schedule)
```

You can only play a schedule once. If you would like to play it again, you must explicitly
[`restart!`](@ref) it. The sound quality will be better the second time round to due to
less compile time.

```jldoctest schedule
julia> play(a_schedule)
ERROR: EOFError: read end of file
[...]

julia> restart!(a_schedule)

julia> play(a_schedule)

julia> close(stream)
```
"""
@inline AudioSchedule(sink::Sink) where {Sink} = AudioSchedule{Sink}(
    Dict{Symbol,Instrument}(),
    OrderedDict{Float64,Vector{Tuple{Symbol,Bool}}}(),
    sink,
    false,
)
export AudioSchedule

"""
    schedule!(schedule::AudioSchedule, synthesizer, start_time, duration)

Schedule an audio synthesizer to be added to the `schedule`, starting at `start_time` and
lasting for `duration`. You can also pass an [`Envelope`](@ref) as a duration. See the
example for [`AudioSchedule`](@ref). Note: the schedule will discard the first sample in
the iterator during scheduling.
"""
@inline function schedule!(a_schedule::AudioSchedule, synthesizer, start_time, duration)
    start_time_unitless = start_time / s
    iterator = make_iterator(synthesizer, samplerate(a_schedule.sink))
    triggers = a_schedule.triggers
    label = gensym("instrument")
    stop_time = start_time_unitless + duration / s
    # TODO: don't discard first sample
    _, state = iterate(iterator)
    a_schedule.orchestra[label] = Instrument(iterator, Ref(state), false)
    on_trigger = (label, true)
    if haskey(triggers, start_time_unitless)
        push!(triggers[start_time_unitless], on_trigger)
    else
        triggers[start_time_unitless] = [on_trigger]
    end
    off_trigger = (label, false)
    if haskey(triggers, stop_time)
        push!(triggers[stop_time], off_trigger)
    else
        triggers[stop_time] = [off_trigger]
    end
    nothing
end

"""
    restart!(a_schedule::AudioSchedule)

Restart a schedule.
"""
function restart!(a_schedule::AudioSchedule)
    for instrument in values(a_schedule.orchestra)
        _, state = iterate(instrument.iterator)
        instrument.state_box[] = state
    end
    a_schedule.consumed = false
    nothing
end
export restart!

@inline function schedule!(
    a_schedule::AudioSchedule,
    synthesizer,
    start_time,
    envelope::Envelope,
)
    the_samplerate = samplerate(a_schedule.sink)
    durations = envelope.durations
    levels = envelope.levels
    shapes = envelope.shapes
    for index = 1:length(durations)
        duration = durations[index]
        schedule!(
            a_schedule,
            InfiniteMap(
                *,
                synthesizer,
                shapes[index](levels[index], levels[index+1], duration),
            ),
            start_time,
            duration,
        )
        start_time = start_time + duration
    end
end
export schedule!

@inline show(io::IO, a_schedule::AudioSchedule) =
    print(io, "AudioSchedule with triggers at $((keys(a_schedule.triggers)...,)) seconds")

@noinline function conduct(sink, ::Tuple{})
    repeated(0.0), ()
end

@noinline function conduct(sink, instruments)
    state_boxes = map((@inline function (instrument)
        instrument.state_box
    end), instruments)
    make_iterator(
        InfiniteMap(+, map((@inline function (instrument)
            instrument.iterator
        end), instruments)...),
        samplerate(sink),
    ),
    state_boxes
end

"""
    play(a_schedule::AudioSchedule)

Play an [`AudioSchedule`](@ref). See the example for [`AudioSchedule`](@ref).
"""
@inline function play(a_schedule::AudioSchedule)
    if a_schedule.consumed
        throw(EOFError())
    else
        sink = a_schedule.sink
        the_samplerate = samplerate(sink)
        outer_iterator =
            let time = Ref(0.0),
                orchestra = a_schedule.orchestra,
                sink = sink,
                the_sample_rate = the_samplerate

                [
                    begin
                        iterator, state_boxes = conduct(
                            sink,
                            ((
                                instrument for
                                instrument in values(orchestra) if instrument.is_on
                            )...,),
                        )
                        for (label, is_on) in trigger_list
                            orchestra[label].is_on = is_on
                        end
                        samples = round(Int, (end_time - time[]) * the_sample_rate)
                        time[] = end_time
                        iterator, state_boxes, samples
                    end for (end_time, trigger_list) in pairs(a_schedule.triggers)
                ]
            end
        _, first_state_boxes, _ = first(outer_iterator)
        flattened = FlattenedIterator(outer_iterator)
        write(sink, IteratorSource(flattened, the_samplerate), length(flattened))
        a_schedule.consumed = true
    end
    return nothing
end

end
