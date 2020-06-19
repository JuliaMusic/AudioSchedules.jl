module AudioSchedules

import Base:
    eltype,
    extrema,
    iterate,
    IteratorEltype,
    IteratorSize,
    length,
    map!,
    read!,
    setindex!,
    show
using Base: Generator, EltypeUnknown, IsInfinite, HasEltype, HasLength, RefValue, tail
using Base.Iterators: cycle, Repeated, repeated, Stateful
using DataStructures: SortedDict
using Interpolations: CubicSplineInterpolation
using NLsolve: nlsolve
using RegularExpressions: capture, of, pattern, raw, short
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: SampleSource, SampleBuf
const TAU = 2 * pi
using Unitful: Hz, μPa, dB, s

include("utilities.jl")

"""
    compound_wave(overtones, dampen)

Create a complex wave. Create a function of this form:

```
angle -> sum((sin(overtone * angle) / overtone^dampen for overtone in 1:overtones))
```

```jldoctest
julia> using AudioSchedules

julia> compound_wave(25, 2)(π/4)
0.9809432244745053
```
"""
function compound_wave(overtones, exponent)
    let overtones = overtones, exponent = exponent
        function (an_angle)
            sum(ntuple(
                let an_angle = an_angle, exponent = exponent
                    @inline function (overtone)
                        sin(overtone * an_angle) / overtone ^ exponent
                    end
                end,
                overtones,
            ))
        end
    end
end
export compound_wave

"""
    get_duration(synthesizer)

Get the duration of a synthesizer in seconds, for synthesizers with an inherent length.
Note that the synthesizer cannot end during the duration (which means there must be one more
sample at the end of the duration).
"""
function get_duration(buffer::SampleBuf)
    ((length(buffer) - 1) / buffer.samplerate)s
end
export get_duration

"""
    make_iterator(synthesizer, the_sample_rate)

Return an iterator that will the play the `synthesizer` at `the_sample_rate`
"""
function make_iterator(buffer::SampleBuf, the_sample_rate)
    # TODO: support resampling
    @assert buffer.samplerate == the_sample_rate / Hz
    cycle(buffer.data)
end

export make_iterator

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

struct StrictMapIterator{AFunction,Iterators}
    a_function::AFunction
    iterators::Iterators
    function StrictMapIterator(
        a_function,
        maps
    )
        functions = map(get_function, maps)
        iteratorss = map(get_iterators, maps)
        models = map(iterators -> ntuple(iterator -> missing, length(iterators)), iteratorss)
        combine = let a_function = a_function, functions = functions, models = models
            @inline function (clump...)
                a_function(map(
                    let a_function = a_function
                        @inline function (a_function, clump)
                            a_function(clump...)
                        end
                    end,
                    functions,
                    partition_models(clump, models...),
                )...)
            end
        end
        mushed = flatten_unrolled(iteratorss...)
        new{typeof(combine), typeof(mushed)}(combine, mushed)
    end
end

get_function(something::StrictMapIterator) = something.a_function
get_iterators(something::StrictMapIterator) = something.iterators
get_function(something) = identity
get_iterators(something) = (something,)

IteratorSize(::Type{<:StrictMapIterator}) = IsInfinite
IteratorEltype(::Type{<:StrictMapIterator}) = EltypeUnknown

@inline function iterate(something::StrictMapIterator, state...)
    items, states = zip_unrolled(Val(2), map(iterate, something.iterators, state...)...)
    something.a_function(items...), states
end

"""
    StrictMap(a_function, synthesizers...)

Map `a_function` over `synthesizers`, assuming that none of the `synthesizers` will end
before they are scheduled to.
"""
struct StrictMap{AFunction,Synthesizers}
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
struct Line
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
struct Cycles
    frequency::Float64
    Cycles(frequency) = new(frequency / Hz)
end

export Cycles

function make_iterator(cycles::Cycles, the_sample_rate)
    CyclesIterator(0, cycles.frequency / (the_sample_rate / Hz) * TAU)
end

"""
    Grow(start, rate)

Exponentially grow or decay from `start`, at a continuous `rate`.
"""
struct Grow
    start::Float64
    rate::Float64
    Grow(start, rate) = new(start, (rate)s)
end

export Grow

struct GrowIterator
    start::Float64
    multiplier::Float64
end

IteratorSize(::Type{GrowIterator}) = IsInfinite

IteratorEltype(::Type{GrowIterator}) = HasEltype

eltype(::Type{GrowIterator}) = Float64

@inline function iterate(grow::GrowIterator, state = grow.start)
    state, state * grow.multiplier
end

function make_iterator(grow::Grow, the_sample_rate)
    GrowIterator(grow.start, ℯ^(grow.rate / (the_sample_rate / Hz)))
end

"""
    Hook(rate, slope)

Make a hook shape, with an exponential curve growing at `rate`, followed by a line with
`slope`. Useful as part as an [`Envelope`](@ref).

```jldoctest hook
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> audio_schedule = AudioSchedule();

julia> envelope = Envelope((0.0, 1.0, 0.0), (0.05s, 0.95s), (Line, Hook(-1/s, -1/0.05s),));

julia> schedule!(audio_schedule, StrictMap(sin, Cycles(440Hz)), 0s, envelope)

julia> plan = plan_within(audio_schedule, 44100Hz);

julia> read(plan, length(plan));
```

Not all hooks are solvable:

```jldoctest hook
julia> audio_schedule = AudioSchedule();

julia> envelope = Envelope((1.0, 0.0), (1s,), (Hook(1/1s, 0.5/1s),));

julia> schedule!(audio_schedule, StrictMap(sin, Cycles(440Hz)), 0s, envelope)

julia> Plan(audio_schedule, 44100Hz)
ERROR: Unsolvable hook
```
"""
struct Hook
    rate::Float64
    slope::Float64
    Hook(rate, slope) = new((rate)s, (slope)s)
end

export Hook

"""
    Envelope(levels, durations, shapes)

Shapes are all functions which return synthesizers:

```
shape(start_value, end_value, duration) -> synthesizer
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

Use `Envelope`s with [`schedule!`](@ref).

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> audio_schedule = AudioSchedule();

julia> envelope = Envelope((0.0, 1.0, 0.0), (1s, 1s), (Line, Line));

julia> schedule!(audio_schedule, StrictMap(sin, Cycles(440Hz)), 0s, envelope)

julia> plan = Plan(audio_schedule, 44100Hz);

julia> read(plan, length(plan));
```

You can also use compound shapes like [`Hook`](@ref).
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

const MapOfStatefuls = StrictMapIterator{<:Any,<:NTuple{<:Any,StrictStateful}}

function Plan(
    outer_iterator::Vector{Tuple{InnerIterator,Int}},
    the_sample_rate,
) where {InnerIterator}
    outer_result = iterate(outer_iterator)
    if outer_result === nothing
        error("The schedule was empty")
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

function map!(a_function, plan::Plan)
    outer_iterator = plan.outer_iterator
    map!(
        function ((iterator, number),)
            StrictMapIterator(a_function, (iterator,)), number
        end,
        outer_iterator,
        outer_iterator,
    )
    nothing
end

@noinline function seek_extrema!(a_map::MapOfStatefuls, number, lower, upper)
    a_function = a_map.a_function
    statefuls = a_map.iterators
    iterators, items, states = zip_unrolled(Val(3), map(detach, statefuls)...)
    for _ = 1:number
        result = a_function(items...)
        lower = min(lower, result)
        upper = max(upper, result)
        items, states = zip_unrolled(Val(2), map(iterate, iterators, states)...)
    end
    map(reattach!, statefuls, items, states)
    lower, upper
end

"""
    extrema!(plan::Plan)

Find the extrema of a plan. This will consume the plan.

```jldoctest audio_schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> audio_schedule = AudioSchedule();

julia> schedule!(audio_schedule, StrictMap(sin, Cycles(440Hz)), 0s, Envelope((1, 1), (1s,), (Line,)))

julia> extrema!(Plan(audio_schedule, 44100Hz)) .≈ (-0.99999974, 0.99999974)
(true, true)
```
"""
function extrema!(plan::Plan)
    lower = Inf
    upper = -Inf
    for (iterator, number) in plan.outer_iterator
        lower, upper = seek_extrema!(iterator, number, lower, upper)
    end
    lower, upper
end
export extrema!

@noinline function inner_fill!(a_map::MapOfStatefuls, buf, a_range)
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

const TRIPLES = Vector{Tuple{Any,Any,Any}}

struct AudioSchedule
    triples::TRIPLES
end

"""
    AudioSchedule(the_sample_rate)

Create an `AudioSchedule`. Add synthesizers to the audio schedule with [`schedule!`](@ref),
then use [`Plan`](@ref) to play the schedule.

```jldoctest audio_schedule
julia> using AudioSchedules

julia> AudioSchedule()
AudioSchedule(Tuple{Any,Any,Any}[])
```
"""
AudioSchedule() = AudioSchedule(TRIPLES())

export AudioSchedule

"""
    schedule!(audio_schedule::AudioSchedule, synthesizer, start_time, duration = get_duration(synthesizer))
    schedule!(audio_schedule::AudioSchedule, synthesizer, start_time, envelope::Envelope)

Schedule an audio synthesizer to be added to the `audio_schedule`, starting at `start_time` with
`duration`, or with the duration contained in an [`Envelope`](@ref). If no duration is given,
use [`get_duration`](@ref) to determine a duration.

```jldoctest audio_schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> audio_schedule = AudioSchedule();

julia> schedule!(audio_schedule, StrictMap(sin, Cycles(440Hz)), 0s, 1s)
```
"""
function schedule!(
    audio_schedule::AudioSchedule,
    synthesizer,
    start_time,
    duration_or_envelope = get_duration(synthesizer),
)
    push!(audio_schedule.triples, (synthesizer, start_time, duration_or_envelope))
    nothing
end
export schedule!

function shape_wave(stateful_wave, shape_synthesizer, the_sample_rate)
    StrictMapIterator(
        *,
        (stateful_wave, StrictStateful(make_iterator(shape_synthesizer, the_sample_rate))),
    )
end

function add_segment!(
    shape,
    duration,
    start_level,
    end_level,
    start_time,
    stateful_wave,
    the_sample_rate,
    arguments...
)
    add_to_plan!(
        start_time,
        duration,
        shape_wave(
            stateful_wave,
            shape(start_level, end_level, duration),
            the_sample_rate,
        ),
        the_sample_rate,
        arguments...
    )
end

function add_segment!(
    hook::Hook,
    duration,
    start_level,
    end_level,
    start_time,
    stateful_wave,
    the_sample_rate,
    arguments...
)
    rate = hook.rate
    duration_unitless = duration / s
    solved = nlsolve([duration_unitless / 2], autodiff = :forward) do residuals, arguments
        first_period = arguments[1]
        residuals[1] =
            end_level + hook.slope * (first_period - duration_unitless) -
            start_level * exp(rate * first_period)
    end
    if !solved.f_converged
        error("Unsolvable hook")
    end
    first_period = (solved.zero[1])s
    second_period = duration - first_period
    add_to_plan!(
        start_time,
        first_period,
        shape_wave(stateful_wave, Grow(start_level, rate/s), the_sample_rate),
        the_sample_rate,
        arguments...
    )
    add_to_plan!(
        start_time + first_period,
        second_period,
        shape_wave(
            stateful_wave,
            Line(start_level * exp(rate/s * first_period), end_level, second_period),
            the_sample_rate,
        ),
        the_sample_rate,
        arguments...
    )
end

function add_to_plan!(
    start_time,
    duration::Envelope,
    arguments...
)
    durations = duration.durations
    levels = duration.levels
    shapes = duration.shapes
    for index = 1:length(durations)
        duration = durations[index]
        add_segment!(
            shapes[index],
            duration,
            levels[index],
            levels[index+1],
            start_time,
            arguments...
        )
        start_time = start_time + duration
    end
    nothing
end

"""
    function repeat!(audio_schedule::AudioSchedule, synthesizer, start_time, gap, count)

Repeat the `synthesizer`, starting at `start_time`, `count` times, with `gap` time between
each repeat.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: Hz, s

julia> using FileIO: load

julia> import LibSndFile

julia> audio_schedule = AudioSchedule();

julia> cd(joinpath(pkgdir(AudioSchedules), "test"))

julia> repeat!(audio_schedule, load("clunk.wav"), 0s, 1s, 5)

julia> plan = Plan(audio_schedule, 44100Hz);

julia> read(plan, length(plan));
```
"""
function repeat!(audio_schedule::AudioSchedule, synthesizer, start_time, gap, count)
    duration = get_duration(synthesizer)
    for _ = 1:count
        push!(audio_schedule.triples, (synthesizer, start_time, duration))
        start_time = start_time + gap
    end
end
export repeat!

function add_to_plan!(
    start_time,
    duration,
    iterator,
    the_sample_rate,
    orchestra,
    triggers,
)
    start_time_seconds = start_time / s
    label = gensym("instrument")
    stop_time_seconds = start_time_seconds + duration / s
    orchestra[label] = iterator, false
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
    nothing
end

"""
    Plan(audio_schedule::AudioSchedule)

Return a `SampledSource` for an [`AudioSchedule`](@ref).

```jldoctest audio_schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> audio_schedule = AudioSchedule();

julia> schedule!(audio_schedule, StrictMap(sin, Cycles(440Hz)), 0s, 1s)

julia> plan = Plan(audio_schedule, 44100Hz);
```

You can find the number of samples in a `Plan` with length.

```jldoctest audio_schedule
julia> the_length = length(plan)
44100
```

You can use the plan as a source for samples.

```jldoctest audio_schedule
julia> read(plan, the_length);
```

You can't plan an empty schedule

```jldoctest audio_schedule
julia> Plan(AudioSchedule(), 44100Hz)
ERROR: The schedule was empty
[...]
```
"""
function Plan(audio_schedule::AudioSchedule, the_sample_rate)
    triggers = SortedDict{Float64,Vector{Tuple{Symbol,Bool}}}()
    orchestra = Dict{Symbol,Tuple{Any,Bool}}()
    for (synthesizer, start_time, duration_or_envelope) in audio_schedule.triples
        add_to_plan!(
            start_time,
            duration_or_envelope,
            StrictStateful(make_iterator(synthesizer, the_sample_rate)),
            the_sample_rate,
            orchestra,
            triggers,
        )
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
    plan_within(audio_schedule::AudioSchedule, the_sample_rate; maximum_volume = 1.0)

Make a plan, then adjust the volume to `maximum_volume`.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> audio_schedule = AudioSchedule();

julia> note = (StrictMap(sin, Cycles(440Hz)), 0s, 1s);

julia> schedule!(audio_schedule, note...); schedule!(audio_schedule, note...)

julia> plan = plan_within(audio_schedule, 44100Hz);

julia> extrema!(plan) .≈ (-1.0, 1.0)
(true, true)
```
"""
function plan_within(audio_schedule::AudioSchedule, the_sample_rate; maximum_volume = 1.0)
    lower, upper = extrema!(Plan(audio_schedule, the_sample_rate))
    final_plan = Plan(audio_schedule, the_sample_rate)
    map!(let scale = 1.0 / max(abs(lower), abs(upper))
        @inline function (amplitude)
            amplitude * scale
        end
    end, final_plan)
    final_plan
end
export plan_within

const DIGITS = of(:maybe, raw("-")), of(:some, short(:digit))
const QUOTIENT = pattern(
    of(:maybe, capture(DIGITS..., name = "numerator")),
    of(:maybe, raw("/"), capture(DIGITS..., name = "denominator")),
    of(:maybe, "o", capture(DIGITS..., name = "octave")),
)

get_parse(something, default) = parse(Int, something)
get_parse(::Nothing, default) = default

"""
    q"interval"

Create a musical interval. You can specify a numerator (which defaults to 1)
and denominator (which defaults to 1) and an octave shift (which defaults to 0).

```jldoctest
julia> using AudioSchedules

julia> q"1"
1//1

julia> q"3/2"
3//2

julia> q"2/3o1"
4//3

julia> q"2/3o-1"
1//3

julia> q"o2"
4//1
```
"""
macro q_str(interval_string::AbstractString)
    a_match = match(QUOTIENT, interval_string)
    if a_match === nothing
        error("Can't parse interval $interval_string")
    end
    esc(
        get_parse(a_match["numerator"], 1) // get_parse(a_match["denominator"], 1) *
        (2 // 1)^get_parse(a_match["octave"], 0),
    )
end
export @q_str

include("equal_loudness.jl")

end
