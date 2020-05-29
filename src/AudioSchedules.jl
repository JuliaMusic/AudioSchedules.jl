module AudioSchedules

import Base: eltype, iterate, IteratorEltype, IteratorSize, length, read!, setindex!, show
using Base: Generator, EltypeUnknown, IsInfinite, HasEltype, HasLength, RefValue, tail
using Base.Iterators: Repeated, repeated, Stateful
using DataStructures: SortedDict
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: Hz, s, SampleSource
const TAU = 2 * pi

@inline function add(first_one, rest...)
    +(first_one, rest...)
end
@inline function add()
    0.0
end

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
make_iterator

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

Cycles from 0 to 2π to repeat at a `frequency`. See the example for [`AudioSchedule`](@ref).
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

mutable struct Plan{InnerIterator} <: SampleSource
    outer_iterator::Vector{Tuple{InnerIterator,Int}}
    outer_state::Int
    inner_iterator::InnerIterator
    has_left::Int
    the_sample_rate::Int
end

const SEGMENT = StrictMapIterator{typeof(*),<:Tuple{StrictStateful,StrictStateful}}

function Plan(
    outer_iterator::Vector{Tuple{InnerIterator,Int}},
    the_sample_rate,
) where {InnerIterator}
    outer_result = iterate(outer_iterator)
    if outer_result === nothing
        error("The schedule was empty or had already been consumed")
    end
    (inner_iterator, has_left), outer_state = outer_result
    Plan{InnerIterator}(
        outer_iterator,
        outer_state,
        inner_iterator,
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

@inline get_iterator(stateful::StrictStateful) = stateful.iterator
@inline get_item_state(stateful::StrictStateful) = stateful.item_state
@inline set_item_state!(stateful::StrictStateful, item, state) =
    stateful.item_state = (item, state)

@noinline function inner_fill!(
    inner_iterator::StrictMapIterator{<:Any,<:NTuple{N,SEGMENT} where {N}},
    buf,
    a_range,
)
    wave_shapes = map(segment -> segment.iterators, inner_iterator.iterators)

    wave_statefuls = map(first, wave_shapes)
    waves = map(get_iterator, wave_statefuls)
    wave_item_states = map(get_item_state, wave_statefuls)
    wave_items = map(first, wave_item_states)
    wave_states = map(last, wave_item_states)

    shape_statefuls = map(last, wave_shapes)
    shapes = map(get_iterator, shape_statefuls)
    shape_item_states = map(get_item_state, shape_statefuls)
    shape_items = map(first, shape_item_states)
    shape_states = map(last, shape_item_states)

    for index in a_range
        buf[index] = inner_iterator.a_function(map(*, wave_items, shape_items)...)

        wave_item_states = map(iterate, waves, wave_states)
        wave_items = map(first, wave_item_states)
        wave_states = map(last, wave_item_states)

        shape_item_states = map(iterate, shapes, shape_states)
        shape_items = map(first, shape_item_states)
        shape_states = map(last, shape_item_states)
    end
    map(set_item_state!, wave_statefuls, wave_items, wave_states)
    map(set_item_state!, shape_statefuls, shape_items, shape_states)
    nothing
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
    (source.inner_iterator, source.has_left), source.outer_state = outer_result
    unsafe_read!(source, buf, frameoffset, framecount, until + 1)
end

function unsafe_read!(source::Plan, buf, frameoffset, framecount, from = 1)
    has_left = source.has_left
    inner_iterator = source.inner_iterator
    empties = framecount - from + 1
    if (has_left >= empties)
        source.has_left = has_left - empties
        inner_fill!(inner_iterator, buf, from:framecount)
        framecount
    else
        until = from + has_left - 1
        inner_fill!(inner_iterator, buf, from:until)
        outer_result = iterate(source.outer_iterator, source.outer_state)
        switch_iterator!(source, buf, frameoffset, framecount, outer_result, until)
    end
end

const SEGMENT_STRICT = StrictMapIterator{
    typeof(*),
    <:Tuple{
        StrictStateful{<:Any,Float64,<:Any},
        StrictStateful{LineIterator,Float64,Float64},
    },
}

const OUTER_ITEM = Tuple{
    StrictMapIterator{
        typeof(add),
        <:NTuple{
            <:Any,
            SEGMENT_STRICT
        },
    },
    Int,
}

const ORCHESTRA = Dict{Symbol,Tuple{SEGMENT_STRICT,Bool}}
const TRIGGERS = SortedDict{Float64,Vector{Tuple{Symbol,Bool}}}

struct AudioSchedule
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

Then, you can create a `SampledSource` from the schedule using [`plan!`](@ref).

```jldoctest schedule
julia> using SampledSignals: unsafe_read!

julia> a_plan = plan!(a_schedule);
```

You can find the number of samples in a `Plan` with length.

```jldoctest schedule
julia> the_length = length(a_plan)
88200
```

You can use the plan as a source for samples.

```jldoctest schedule
julia> buf = Vector{Float64}(undef, the_length);

julia> unsafe_read!(a_plan, buf, 0, the_length);

julia> buf[1:4] ≈ [0.0, 7.1029846e-6, 2.8356127e-5, 6.3592327e-5]
true
```

You can only `plan!` a schedule once.

```jldoctest schedule
julia> plan!(a_schedule)
ERROR: The schedule was empty or had already been consumed
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
    a_schedule.orchestra[label] = iterator, false
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
    stateful_iterator = StrictStateful(make_iterator(synthesizer, the_sample_rate))
    for index = 1:length(durations)
        duration = durations[index]
        schedule_segment!(
            a_schedule,
            StrictMapIterator(
                *,
                (
                    stateful_iterator,
                    StrictStateful(make_iterator(
                        shapes[index](levels[index], levels[index+1], duration),
                        the_sample_rate,
                    )),
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

"""
    plan!(a_schedule::AudioSchedule)

Return a `SampledSource` for the schedule.
"""
function plan!(a_schedule::AudioSchedule)
    the_sample_rate = a_schedule.the_sample_rate
    orchestra = a_schedule.orchestra
    outer_iterator = OUTER_ITEM[]
    time = 0.0
    for (end_time, trigger_list) in pairs(a_schedule.triggers)
        together = StrictMapIterator(
            add,
            ((iterator for (iterator, is_on) in values(orchestra) if is_on)...,),
        )
        for (label, is_on) in trigger_list
            iterator, _ = orchestra[label]
            orchestra[label] = iterator, is_on
        end
        samples = round(Int, (end_time - time) * the_sample_rate)
        time = end_time
        push!(outer_iterator, (together, samples))
    end
    empty!(a_schedule.triggers)
    Plan(outer_iterator, the_sample_rate)
end
export plan!

end
