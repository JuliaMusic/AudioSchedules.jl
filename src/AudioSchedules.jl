module AudioSchedules

import Base: eltype, iterate, IteratorEltype, IteratorSize, length, read!, setindex!, show
using Base: Generator, EltypeUnknown, IsInfinite, HasEltype, HasLength, RefValue
using Base.Iterators: repeated, Stateful
using DataStructures: OrderedDict
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: Hz, s, SampleSource
const TAU = 2 * pi

mutable struct Plan{InnerIterator} <: SampleSource
    outer_iterator::Vector{Tuple{InnerIterator,Int}}
    outer_state::Int
    inner_iterator::InnerIterator
    item::Float64
    inner_state::Any
    has_left::Int
    the_sample_rate::Int
end

function Plan(
    outer_iterator::Vector{Tuple{InnerIterator,Int}},
    the_sample_rate,
) where {InnerIterator}
    # TODO: save first item
    (inner_iterator, has_left), outer_state = iterate(outer_iterator)
    item, inner_state = iterate(inner_iterator)
    Plan{InnerIterator}(
        outer_iterator,
        outer_state,
        inner_iterator,
        item,
        inner_state,
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

@noinline function inner_fill!(inner_iterator, item, inner_state, buf, a_range)
    for index in a_range
        @inbounds buf[index] = item
        item::Float64, inner_state = iterate(inner_iterator, inner_state)
    end
    item, inner_state
end

@noinline function switch_iterator!(source, buf, frameoffset, framecount, ::Nothing, until)
    until
end
@noinline function switch_iterator!(
    source,
    buf,
    frameoffset,
    framecount,
    outer_result::Tuple{Tuple{Any,Any},Any},
    until,
)
    (inner_iterator, source.has_left), source.outer_state = outer_result
    source.item, source.inner_state = iterate(inner_iterator)
    source.inner_iterator = inner_iterator
    unsafe_read!(source, buf, frameoffset, framecount, from = until + 1)
end

function unsafe_read!(source::Plan, buf, frameoffset, framecount; from = 1)
    has_left = source.has_left
    inner_iterator = source.inner_iterator
    item = source.item
    inner_state = source.inner_state
    empties = framecount - from + 1
    if (has_left >= empties)
        source.has_left = has_left - empties
        source.item, source.inner_state =
            inner_fill!(inner_iterator, item, inner_state, buf, from:framecount)
        framecount
    else
        until = from + has_left - 1
        inner_fill!(inner_iterator, item, inner_state, buf, from:until)
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

struct InfiniteMapIterator{AFunction,Iterators}
    a_function::AFunction
    iterators::Iterators
end

IteratorSize(::Type{<:InfiniteMapIterator}) = IsInfinite

IteratorEltype(::Type{<:InfiniteMapIterator}) = EltypeUnknown

@inline function iterate(something::InfiniteMapIterator, states...)
    items_states = map(iterate, something.iterators, states...)
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

function make_iterator(a_map::InfiniteMap, the_sample_rate)
    InfiniteMapIterator(
        a_map.a_function,
        map(
            (
                let the_sample_rate = the_sample_rate
                    @inline function (synthesizer)
                        make_iterator(synthesizer, the_sample_rate)
                    end
                end
            ),
            a_map.synthesizers,
        ),
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

const ORCHESTRA = Dict{Symbol,Synthesizer}
const TRIGGERS = OrderedDict{Float64,Vector{Tuple{Symbol,Bool}}}

mutable struct AudioSchedule
    orchestra::ORCHESTRA
    triggers::TRIGGERS
end

# TODO: consume schedules or plans?
# TODO: remove sample rate dependency for schedules?
"""
    AudioSchedule()

Create an `AudioSchedule`.

```jldoctest schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> a_schedule = AudioSchedule()
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

Then, you can create a `SampledSource` from the schedule using [`Plan`](@ref).

```jldoctest schedule
julia> using SampledSignals: unsafe_read!

julia> a_plan = Plan(a_schedule, 44100Hz);
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

julia> buf[1:4] == [0.0, 7.102984600764591e-6, 2.835612782188846e-5, 6.359232681199096e-5]
true
```
"""
AudioSchedule() = AudioSchedule(ORCHESTRA(), TRIGGERS())
export AudioSchedule

"""
    schedule!(schedule::AudioSchedule, synthesizer::Synthesizer, start_time, duration)

Schedule an audio synthesizer to be added to the `schedule`, starting at `start_time` and
lasting for `duration`. You can also pass an [`Envelope`](@ref) as a duration. See the
example for [`AudioSchedule`](@ref).
"""
function schedule!(a_schedule::AudioSchedule, synthesizer::Synthesizer, start_time, duration)
    start_time_unitless = start_time / s
    triggers = a_schedule.triggers
    label = gensym("instrument")
    stop_time = start_time_unitless + duration / s
    a_schedule.orchestra[label] = synthesizer
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

function schedule!(a_schedule::AudioSchedule, synthesizer::Synthesizer, start_time, envelope::Envelope)
    durations = envelope.durations
    levels = envelope.levels
    shapes = envelope.shapes
    index = 1
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

function conduct(::Tuple{})
    repeated(0.0)
end

function conduct(iterators)
    InfiniteMapIterator(+, iterators)
end

"""
    Plan(a_schedule::AudioSchedule)

Return a `SampledSource` for the schedule.
"""
function Plan(a_schedule::AudioSchedule, the_sample_rate)
    the_sample_rate_unitless = the_sample_rate / Hz
    time = Ref(0.0)
    stateful_orchestra = Dict(
        (label, (Stateful(make_iterator(synthesizer, the_sample_rate_unitless)), false)) for (label, synthesizer) in pairs(a_schedule.orchestra)
    )
    all_pairs = collect(pairs(a_schedule.triggers))
    end_time, trigger_list = all_pairs[2]
    Plan(
        [
            begin
                together = conduct(((
                    iterator for (iterator, is_on) in values(stateful_orchestra) if is_on
                )...,))
                for (label, is_on) in trigger_list
                    iterator, _ = stateful_orchestra[label]
                    stateful_orchestra[label] = iterator, is_on
                end
                samples = round(Int, (end_time - time[]) * the_sample_rate_unitless)
                time[] = end_time
                together, samples
            end for (end_time, trigger_list) in pairs(a_schedule.triggers)
        ],
        the_sample_rate_unitless,
    )
end
export Plan

end
