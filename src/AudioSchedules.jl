module AudioSchedules

import Base: eltype, iterate, IteratorEltype, IteratorSize, length, read!, setindex!, show
using Base: Generator, EltypeUnknown, IsInfinite, HasEltype, HasLength, RefValue
using Base.Iterators: repeated
using DataStructures: OrderedDict
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: Hz, s, SampleSource
const TAU = 2 * pi

mutable struct ScheduleIterator{InnerIterator, StateBoxes} <: SampleSource
    outer_iterator::Vector{Tuple{InnerIterator, StateBoxes, Int}}
    outer_state::Int
    inner_iterator::InnerIterator
    state_boxes::StateBoxes
    inner_state::Tuple
    has_left::Int
    samplerate::Int
end

function ScheduleIterator(outer_iterator::Vector{Tuple{InnerIterator, StateBoxes, Int}}, samplerate) where {InnerIterator, StateBoxes}
    # TODO: save first item
    (inner_iterator, state_boxes, has_left), outer_state = iterate(outer_iterator)
    ScheduleIterator{InnerIterator, StateBoxes}(outer_iterator, outer_state, inner_iterator, state_boxes, map(getindex, state_boxes), has_left, samplerate)
end

eltype(source::ScheduleIterator) = Float64

nchannels(source::ScheduleIterator) = 1

samplerate(source::ScheduleIterator) = source.samplerate

function length(source::ScheduleIterator)
    sum((samples for (_, _, samples) in source.outer_iterator))
end

@noinline function inner_fill!(inner_iterator, inner_state, buf, a_range)
    for index in a_range
        item::Float64, inner_state = iterate(inner_iterator, inner_state)
        @inbounds buf[index] = item
    end
    inner_state
end

function unsafe_read!(source::ScheduleIterator, buf, frameoffset, framecount; from = 1)
    has_left = source.has_left
    inner_iterator = source.inner_iterator
    inner_state = source.inner_state
    empties = framecount - from + 1
    if (has_left >= empties)
        source.has_left = has_left - empties
        source.inner_state = inner_fill!(inner_iterator, inner_state, buf, from:framecount)
        framecount
    else
        until = from + has_left - 1
        map(setindex!, source.state_boxes, inner_fill!(inner_iterator, inner_state, buf, from:until))
        outer_result = iterate(source.outer_iterator, source.outer_state)
        if outer_result === nothing
            until
        else
            (source.inner_iterator, state_boxes, source.has_left), source.outer_state = outer_result
            source.inner_state = map(getindex, state_boxes)
            source.state_boxes = state_boxes
            unsafe_read!(source, buf, frameoffset, framecount, from = until + 1)
        end
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
make_iterator(synthesizer, samplerate) = synthesizer
export make_iterator

struct InfiniteMapIterator{AFunction,Iterators}
    a_function::AFunction
    iterators::Iterators
end

IteratorSize(::Type{<:InfiniteMapIterator}) = IsInfinite

IteratorEltype(::Type{<:InfiniteMapIterator}) = EltypeUnknown

@inline function iterate_no_nothing(iterator, state...)
    iterate(iterator, state...)::Tuple{Any, Any}
end

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

function make_iterator(a_map::InfiniteMap, samplerate)
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

IteratorSize(::Type{LineIterator}) = IsInfinite

IteratorEltype(::Type{LineIterator}) = HasEltype

eltype(::Type{LineIterator}) = Float64

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

function make_iterator(line::Line, samplerate)
    start_value = line.start_value
    LineIterator(start_value, (line.end_value - start_value) / (samplerate * line.duration))
end

struct CyclesIterator
    start::Float64
    plus::Float64
end

IteratorSize(::Type{CyclesIterator}) = IsInfinite

IteratorEltype(::Type{CyclesIterator}) = HasEltype

eltype(::Type{CyclesIterator}) = Float64

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
    Cycles(frequency) = new(frequency / Hz)
end

export Cycles

function make_iterator(cycles::Cycles, samplerate)
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
    function Envelope(
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
AudioSchedule(sink::Sink) where {Sink} = AudioSchedule{Sink}(
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
function schedule!(a_schedule::AudioSchedule, synthesizer, start_time, duration)
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

function schedule!(a_schedule::AudioSchedule, synthesizer, start_time, envelope::Envelope)
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

show(io::IO, a_schedule::AudioSchedule) =
    print(io, "AudioSchedule with triggers at $((keys(a_schedule.triggers)...,)) seconds")

function conduct(sink, ::Tuple{})
    repeated(0.0), ()
end

function conduct(sink, instruments)
    state_boxes = map(instrument -> instrument.state_box, instruments)
    make_iterator(
        InfiniteMap(+, map(instrument -> instrument.iterator, instruments)...),
        samplerate(sink),
    ),
    state_boxes
end

"""
    play(a_schedule::AudioSchedule)

Play an [`AudioSchedule`](@ref). See the example for [`AudioSchedule`](@ref).
"""
function play(a_schedule::AudioSchedule)
    if a_schedule.consumed
        throw(EOFError())
    else
        sink = a_schedule.sink
        the_sample_rate = samplerate(sink)
        time = Ref(0.0)
        orchestra = a_schedule.orchestra
        outer_iterator = [
            begin
                iterator, state_boxes = conduct(
                    sink,
                    ((
                        instrument for instrument in values(orchestra) if instrument.is_on
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
        flattened = ScheduleIterator(outer_iterator, the_sample_rate)
        write(sink, flattened, length(flattened))
        a_schedule.consumed = true
    end
    return nothing
end
export play

end
