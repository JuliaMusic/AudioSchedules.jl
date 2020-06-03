module AudioSchedules

import Base: eltype, extrema, iterate, IteratorEltype, IteratorSize, length, map!, read!, setindex!, show
using Base: Generator, EltypeUnknown, IsInfinite, HasEltype, HasLength, RefValue, tail
using Base.Iterators: Repeated, repeated, Stateful
using DataStructures: SortedDict
using Interpolations: CubicSplineInterpolation
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: Hz, s, SampleSource
const TAU = 2 * pi
using Unitful: Hz, μPa, dB

@inline function zip_unrolled(expected, them::Vararg{Any})
    map(first, them), zip_unrolled(expected, map(tail, them)...)...
end
@inline function zip_unrolled(expected, them::Vararg{Tuple{}})
    ()
end
@inline function zip_unrolled(expected, them::Vararg{Any,0})
    ntuple((@inline function (thing)
        ()
    end), expected)
end

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

@inline function detach(iterator::StrictStateful)
    iterator.iterator, iterator.item_state...
end
@inline function reattach!(iterator::StrictStateful, item, state)
    iterator.item_state = (item, state)
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
export make_iterator

struct StrictMapIterator{AFunction,Iterators}
    a_function::AFunction
    iterators::Iterators
end

IteratorSize(::Type{<:StrictMapIterator}) = IsInfinite

IteratorEltype(::Type{<:StrictMapIterator}) = EltypeUnknown

@inline function iterate(something::StrictMapIterator, state...)
    items, states = zip_unrolled(Val(2), map(iterate, something.iterators, state...)...)
    something.a_function(items...), states
end

@inline flatten_unrolled(first_one, rest...) = first_one..., flatten_unrolled(rest...)...
@inline flatten_unrolled() = ()
@inline function take_tuple(items, model)
    kept, trashed = take_tuple(tail(items), tail(model))
    (first(items), kept...), trashed
end
@inline function take_tuple(items, ::Tuple{})
    (), items
end
@inline function partition_models(items, first_model, more_models...)
    kept, trashed = take_tuple(items, first_model)
    kept, partition_models(trashed, more_models...)...
end
@inline partition_models(items) = ()

function StrictMapIterator(a_function, maps::Tuple{StrictMapIterator, Vararg{StrictMapIterator}})
    functions = map(a_map -> a_map.a_function, maps)
    iteratorss = map(a_map -> a_map.iterators, maps)
    models =
        map(iterators -> ntuple(iterator -> missing, length(iterators)), iteratorss)
    StrictMapIterator(
        let a_function = a_function, functions = functions, models = models
            @inline function (clump...)
                clumps = a_function(map(
                    let a_function = a_function
                        @inline function (a_function, clump)
                            a_function(clump...)
                        end
                    end,
                    functions,
                    partition_models(clump, models...),
                )...)
            end
        end,
        flatten_unrolled(iteratorss...),
    )
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
        (line.end_value - start_value) / (the_sample_rate / Hz * line.duration),
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

Cycles from 0 to 2π to repeat at a `frequency`.
"""
struct Cycles <: Synthesizer
    frequency::Float64
    Cycles(frequency) = new(frequency / Hz)
end

export Cycles

function make_iterator(cycles::Cycles, the_sample_rate)
    CyclesIterator(0, cycles.frequency / (the_sample_rate / Hz) * TAU)
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

const MapOfStatefuls = StrictMapIterator{<:Any, <:NTuple{<:Any, StrictStateful}}

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

function map!(a_function, a_plan::Plan)
    outer_iterator = a_plan.outer_iterator
    map!(
        function ((iterator, number),)
            StrictMapIterator(a_function, (iterator,)), number
        end,
        outer_iterator,
        outer_iterator
    )
    nothing
end

@noinline function seek_extrema!(a_map::MapOfStatefuls, number, lower, upper)
    a_function = a_map.a_function
    statefuls = a_map.iterators
    iterators, items, states = zip_unrolled(Val(3), map(detach, statefuls)...)
    for _ in 1:number
        result = a_function(items...)
        lower = min(lower, result)
        upper = max(upper, result)
        items, states = zip_unrolled(Val(2), map(iterate, iterators, states)...)
    end
    map(reattach!, statefuls, items, states)
    lower, upper
end

"""
    extrema!(a_plan::Plan)

Find the extrema of a plan. This will consume the plan.

```jldoctest schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> a_schedule = AudioSchedule();

julia> schedule!(a_schedule, StrictMap(sin, Cycles(440Hz)), 0s, Envelope((1, 1), (1s,), (Line,)))

julia> extrema!(Plan(a_schedule, 44100Hz)) .≈ (-0.99999974, 0.99999974)
(true, true)
```
"""
function extrema!(a_plan::Plan)
    lower = Inf
    upper = -Inf
    for (iterator, number) in a_plan.outer_iterator
        lower, upper = seek_extrema!(iterator, number, lower, upper)
    end
    lower, upper
end
export extrema!

@noinline function inner_fill!(a_map::StrictMapIterator{<:Any, <:NTuple{<:Any, StrictStateful}}, buf, a_range)
    a_function = a_map.a_function
    statefuls = a_map.iterators
    iterators, items, states = zip_unrolled(Val(3), map(detach, statefuls)...)
    for index in a_range
        buf[index] = a_function(items...)
        items, states = zip_unrolled(Val(2), map(iterate, iterators, states)...)
    end
    map(reattach!, statefuls, items, states)
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

const TRIPLES = Vector{Tuple{Synthesizer, Any, Envelope}}

struct AudioSchedule
    triples::TRIPLES
end

"""
    AudioSchedule(the_sample_rate)

Create an `AudioSchedule`.

```jldoctest schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> a_schedule = AudioSchedule();
```

Add a synthesizer to the schedule with [`schedule!`](@ref).

```jldoctest schedule
julia> envelope = Envelope((0, 0.25, 0), (0.05s, 0.95s), (Line, Line));

julia> schedule!(a_schedule, StrictMap(sin, Cycles(440Hz)), 0s, envelope)

julia> schedule!(a_schedule, StrictMap(sin, Cycles(440Hz)), 1s, envelope)

julia> schedule!(a_schedule, StrictMap(sin, Cycles(550Hz)), 1s, envelope)
```

Then, you can create a `SampledSource` from the schedule using [`Plan`](@ref).

```jldoctest schedule
julia> a_plan = Plan(a_schedule, 44100Hz);
```

You can find the number of samples in a `Plan` with length.

```jldoctest schedule
julia> the_length = length(a_plan)
88200
```

You can use the plan as a source for samples.

```jldoctest schedule
julia> using SampledSignals: unsafe_read!

julia> buf = Vector{Float64}(undef, the_length);

julia> unsafe_read!(a_plan, buf, 0, the_length);

julia> buf[1:4] ≈ [0.0, 7.1029846e-6, 2.8356127e-5, 6.3592327e-5]
true
```
"""
AudioSchedule() = AudioSchedule(TRIPLES())

export AudioSchedule

"""
    schedule!(schedule::AudioSchedule, synthesizer::Synthesizer, start_time, envelope::Envelope)

Schedule an audio synthesizer to be added to the `schedule`, starting at `start_time` with
the duration and volume contained in an [`Envelope`](@ref).
"""
function schedule!(a_schedule::AudioSchedule, synthesizer::Synthesizer, start_time, envelope::Envelope)
    push!(a_schedule.triples, (synthesizer, start_time, envelope))
    nothing
end

export schedule!

"""
    Plan(a_schedule::AudioSchedule)

Return a `SampledSource` for the schedule.
"""
function Plan(a_schedule::AudioSchedule, the_sample_rate)
    triggers = SortedDict{Float64,Vector{Tuple{Symbol,Bool}}}()
    orchestra = Dict{Symbol,Tuple{StrictMapIterator,Bool}}()
    for (synthesizer, start_time, envelope) in a_schedule.triples
        durations = envelope.durations
        levels = envelope.levels
        shapes = envelope.shapes
        stateful_iterator = StrictStateful(make_iterator(synthesizer, the_sample_rate))
        for index = 1:length(durations)
            duration = durations[index]
            start_time_seconds = start_time / s
            label = gensym("instrument")
            stop_time_seconds = start_time_seconds + duration / s
            orchestra[label] = StrictMapIterator(
                *,
                (stateful_iterator,
                StrictStateful(make_iterator(
                    shapes[index](levels[index], levels[index+1], duration),
                    the_sample_rate,
                ))
                )
            ), false
            start_trigger = label, true
            if haskey(triggers, start_time_seconds)
                push!(triggers[start_time_seconds], start_trigger)
            else
                triggers[start_time_seconds] = [start_trigger]
            end
            stop_trigger = label, false
            if haskey(triggers, stop_time_seconds)
                push!(triggers[stop_time_seconds], stop_trigger)
            else
                triggers[stop_time_seconds] = [stop_trigger]
            end
            start_time = start_time + duration
        end
    end
    time = Ref(0.0s)
    outer_iterator = [
        begin
            trigger_time = (trigger_time_unitless)s
            samples = round(Int, (trigger_time - time[]) * the_sample_rate)
            time[] = trigger_time
            together = StrictMapIterator(
                add,
                ((iterator for (iterator, is_on) in values(orchestra) if is_on)...,),
            )
            for (label, is_on) in trigger_list
                iterator, _ = orchestra[label]
                orchestra[label] = iterator, is_on
            end
            together, samples
        end for (trigger_time_unitless, trigger_list) in pairs(triggers)
    ]
    Plan(outer_iterator, the_sample_rate / Hz)
end

export Plan

"""
    plan_within(a_schedule::AudioSchedule, the_sample_rate; maximum_volume = 1.0)

Make a plan, then adjust the volume to `maximum_volume`.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> schedule = AudioSchedule();

julia> note = StrictMap(sin, Cycles(440Hz)), 0s, Envelope((1, 1), (1s,), (Line,));

julia> schedule!(schedule, note...); schedule!(schedule, note...)

julia> plan = plan_within(schedule, 44100Hz);

julia> extrema!(plan) .≈ (-1.0, 1.0)
(true, true)
```
"""
function plan_within(a_schedule::AudioSchedule, the_sample_rate; maximum_volume = 1.0)
    lower, upper = extrema!(Plan(a_schedule, the_sample_rate))
    final_plan = Plan(a_schedule, the_sample_rate)
    map!(
        let scale = 1.0 / max(abs(lower), abs(upper))
            @inline function (amplitude)
                amplitude * scale
            end
        end,
        final_plan
    )
    final_plan
end
export plan_within

include("equal_loudness.jl")

end
