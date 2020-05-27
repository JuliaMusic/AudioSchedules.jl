module AudioSchedules

import Base: eltype, iterate, IteratorEltype, IteratorSize, length, read!, setindex!, show
using Base: Generator, EltypeUnknown, IsInfinite, HasEltype, HasLength, RefValue, tail
using Base.Iterators: repeated, Stateful
using DataStructures: SortedDict
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: Hz, s, SampleSource
const TAU = 2 * pi

mutable struct StrictStateful{Iterator,Item,State}
    iterator::Iterator
    item_state::Tuple{Item,State}
end
IteratorSize(::Type{<:StrictStateful}) = IsInfinite()
IteratorEltype(::Type{StrictStateful{<:Any,Item,<:Any}}) where {Item} = Item
StrictStateful(iterator) = StrictStateful(iterator, iterate(iterator))
function iterate(stateful::StrictStateful, state = nothing)
    last_item, state = stateful.item_state
    stateful.item_state = iterate(stateful.iterator, state)
    last_item, nothing
end

mutable struct Plan{InnerIterator} <: SampleSource
    outer_iterator::Vector{Tuple{InnerIterator,Int}}
    outer_state::Int
    inner_iterator::InnerIterator
    item::Float64
    has_left::Int
    the_sample_rate::Int
end

function Plan(
    outer_iterator::Vector{Tuple{InnerIterator,Int}},
    the_sample_rate,
) where {InnerIterator}
    # TODO: save first item
    (inner_iterator, has_left), outer_state = iterate(outer_iterator)
    item, _ = iterate(inner_iterator)
    Plan{InnerIterator}(
        outer_iterator,
        outer_state,
        inner_iterator,
        item,
        has_left,
        the_sample_rate,
    )
end

eltype(source::Plan) = Float64

nchannels(source::Plan) = 1

samplerate(source::Plan) = source.the_sample_rate

function length(source::Plan)
    sum((samples for (_, samples) in source.outer_iterator))
end

# pull out all the type stable parts from the super unstable one below

@noinline function inner_fill!(inner_iterator, item, buf, a_range)
    for index in a_range
        @inbounds buf[index] = item
        item, inner_state = iterate(inner_iterator)
    end
    item
end

@noinline function switch_iterator!(source, buf, frameoffset, framecount, ::Nothing, until)
    until
end
@noinline function switch_iterator!(
    source,
    buf,
    frameoffset,
    framecount,
    outer_result,
    until,
)
    (inner_iterator, source.has_left), source.outer_state = outer_result
    source.item, _ = iterate(inner_iterator)
    source.inner_iterator = inner_iterator
    unsafe_read!(source, buf, frameoffset, framecount, until + 1)
end

function unsafe_read!(source::Plan, buf, frameoffset, framecount, from = 1)
    has_left = source.has_left
    inner_iterator = source.inner_iterator
    item = source.item
    empties = framecount - from + 1
    if (has_left >= empties)
        source.has_left = has_left - empties
        source.item = inner_fill!(inner_iterator, item, buf, from:framecount)
        framecount
    else
        until = from + has_left - 1
        inner_fill!(inner_iterator, item, buf, from:until)
        outer_result = iterate(source.outer_iterator, source.outer_state)
        switch_iterator!(source, buf, frameoffset, framecount, outer_result, until)
    end
end

"""
    abstract type Synthesizer

Synthesizers need only support [`make_iterator`](@ref).
"""
abstract type Synthesizer end
export Synthesizer

"""
    make_iterator(synthesizer, the_sample_rate)

Return an iterator that will the play the `synthesizer` at `the_sample_rate`
"""
make_iterator(synthesizer, the_sample_rate) = synthesizer
export make_iterator

struct StrictMapIterator{AFunction,Iterators}
    a_function::AFunction
    iterators::Iterators
end

IteratorSize(::Type{<:StrictMapIterator}) = IsInfinite

IteratorEltype(::Type{<:StrictMapIterator}) = EltypeUnknown

@inline function iterate(something::StrictMapIterator, state...)
    items_states = map(iterate, something.iterators, state...)
    something.a_function(map(first, items_states)...), map(last, items_states)
end

"""
    StrictMap(a_function, synthesizers...)

Map `a_function` over `synthesizers`, assuming that none of the `synthesizers` will end
before they are scheduled to.
"""
struct StrictMap{AFunction,Synthesizers} <: Synthesizer
    a_function::AFunction
    synthesizers::Synthesizers
    StrictMap(a_function::AFunction, synthesizers...) where {AFunction} =
        new{AFunction,typeof(synthesizers)}(a_function, synthesizers)
end
export StrictMap

function make_iterator(a_map::StrictMap, the_sample_rate)
    StrictMapIterator(
        a_map.a_function,
        map(synthesizer -> make_iterator(synthesizer, the_sample_rate), a_map.synthesizers),
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

function make_iterator(line::Line, the_sample_rate)
    start_value = line.start_value
    LineIterator(
        start_value,
        (line.end_value - start_value) / (the_sample_rate * line.duration),
    )
end

struct CyclesIterator
    start::Float64
    plus::Float64
end

IteratorSize(::Type{CyclesIterator}) = IsInfinite

IteratorEltype(::Type{CyclesIterator}) = HasEltype

eltype(::Type{CyclesIterator}) = Float64

@inline function iterate(ring::CyclesIterator, state = ring.start)
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

function make_iterator(cycles::Cycles, the_sample_rate)
    CyclesIterator(0, cycles.frequency / the_sample_rate * TAU)
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
Line(1.0, 0.0, 0.05 s)
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

const ORCHESTRA = Dict{Symbol,Tuple{StrictStateful,Bool}}
const TRIGGERS = SortedDict{Float64,Vector{Tuple{Symbol,Bool}}}

mutable struct AudioSchedule
    orchestra::ORCHESTRA
    triggers::TRIGGERS
    the_sample_rate::Int
end

"""
    AudioSchedule(the_sample_rate)

Create an `AudioSchedule`.

```jldoctest schedule
julia> using AudioSchedules

julia> using SampledSignals: s, Hz

julia> a_schedule = AudioSchedule(44100Hz)
AudioSchedule with triggers at () seconds
```

Add a synthesizer to the schedule with [`schedule!`](@ref).

```jldoctest schedule
julia> envelope = Envelope((0, 0.25, 0), (0.05s, 0.95s), (Line, Line));

julia> schedule!(a_schedule, StrictMap(sin, Cycles(440Hz)), 0s, envelope)

julia> schedule!(a_schedule, StrictMap(sin, Cycles(440Hz)), 1s, envelope)

julia> schedule!(a_schedule, StrictMap(sin, Cycles(550Hz)), 1s, envelope)

julia> a_schedule
AudioSchedule with triggers at (0.0, 0.05, 1.0, 1.05, 2.0) seconds
```

Then, you can create a `SampledSource` from the schedule using [`Plan`](@ref).

```jldoctest schedule
julia> using SampledSignals: unsafe_read!

julia> a_plan = Plan(a_schedule);
```

You can find the number of samples in a `Plan` with length.

```jldoctest schedule
julia> the_length = length(a_plan)
88200
```

You can use `Plan` as a source for samples.

```jldoctest schedule
julia> buf = Vector{Float64}(undef, the_length);

julia> unsafe_read!(a_plan, buf, 0, the_length);

julia> buf[1:4] == [0.0, 1.417806391094423e-5, 4.23948845413273e-5, 8.44006285522918e-5]
true
```
"""
AudioSchedule(the_sample_rate) =
    AudioSchedule(ORCHESTRA(), TRIGGERS(), the_sample_rate / Hz)
export AudioSchedule

function schedule_segment!(a_schedule::AudioSchedule, iterator, start_time, duration)
    start_time_unitless = start_time / s
    triggers = a_schedule.triggers
    label = gensym("instrument")
    stop_time = start_time_unitless + duration / s
    a_schedule.orchestra[label] =
        StrictStateful(iterator), false
    start_trigger = label, true
    if haskey(triggers, start_time_unitless)
        push!(triggers[start_time_unitless], start_trigger)
    else
        triggers[start_time_unitless] = [start_trigger]
    end
    stop_trigger = label, false
    if haskey(triggers, stop_time)
        push!(triggers[stop_time], stop_trigger)
    else
        triggers[stop_time] = [stop_trigger]
    end
    nothing
end

"""
    schedule!(schedule::AudioSchedule, synthesizer::Synthesizer, start_time, envelope::Envelope)

Schedule an audio synthesizer to be added to the `schedule`, starting at `start_time` with
the duration and volume contained in an [`Envelope`](@ref). See the example for
[`AudioSchedule`](@ref).
"""
function schedule!(a_schedule::AudioSchedule, synthesizer, start_time, envelope::Envelope)
    the_sample_rate = a_schedule.the_sample_rate
    durations = envelope.durations
    levels = envelope.levels
    shapes = envelope.shapes
    index = 1
    stateful_iterator = StrictStateful(make_iterator(synthesizer, the_sample_rate))
    for index = 1:length(durations)
        duration = durations[index]
        schedule_segment!(
            a_schedule,
            StrictMapIterator(
                *,
                (
                    stateful_iterator,
                    make_iterator(
                        shapes[index](levels[index], levels[index+1], duration),
                        the_sample_rate,
                    ),
                ),
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

function conduct(::Tuple{})
    repeated(0.0)
end

function conduct(iterators)
    StrictMapIterator(+, iterators)
end

"""
    Plan(a_schedule::AudioSchedule)

Return a `SampledSource` for the schedule.
"""
function Plan(a_schedule::AudioSchedule)
    the_sample_rate = a_schedule.the_sample_rate
    orchestra = a_schedule.orchestra
    time = Ref(0.0)
    Plan(
        [
            begin
                together = conduct(((
                    iterator for (iterator, is_on) in values(orchestra) if is_on
                )...,))
                for (label, is_on) in trigger_list
                    iterator, _ = orchestra[label]
                    orchestra[label] = iterator, is_on
                end
                samples = round(Int, (end_time - time[]) * the_sample_rate)
                time[] = end_time
                together, samples
            end for (end_time, trigger_list) in pairs(a_schedule.triggers)
        ],
        the_sample_rate,
    )
end
export Plan

end
