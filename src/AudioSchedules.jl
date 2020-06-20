module AudioSchedules

using Unitful
using Unitful: @dimension, @refunit

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
using Unitful: @dimension, Hz, μPa, dB, @refunit, s, hr

include("utilities.jl")

"""
    compound_wave(overtones, dampen)

Create a compound wave. Create a function of this form:

```
radians -> sum((sin(overtone * radians) / overtone for overtone in 1:overtones))
```

To increase richness but also buziness, increase `overtones`. To decrease buziness but also
buziness, decrease `dampen`.

```jldoctest
julia> using AudioSchedules

julia> compound_wave(3)(π/4)
1.4428090415820634
```
"""
function compound_wave(overtones)
    let overtones = overtones
        function (an_angle)
            sum(ntuple(
                let an_angle = an_angle
                    @inline function (overtone)
                        sin(overtone * an_angle) / overtone
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

Get the duration of a synthesizer (with units of time, like `s`), for synthesizers with an
inherent length. Note that there must be one extra sample at the end of the duration.

```jldoctest
julia> using AudioSchedules

julia> using FileIO: load

julia> import LibSndFile

julia> cd(joinpath(pkgdir(AudioSchedules), "test"))

julia> get_duration(load("clunk.wav"))
0.351859410430839 s
```
"""
function get_duration(buffer::SampleBuf)
    ((length(buffer) - 1) / buffer.samplerate)s
end
export get_duration

"""
    make_iterator(synthesizer, the_sample_rate)

Return an iterator that will the play the `synthesizer` at `the_sample_rate` (with frequency
units, like `Hz`). The iterator should yield ratios between -1 and 1.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: Hz

julia> first(make_iterator(Cycles(440Hz), 44100Hz))
0.0
```
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
    function StrictMapIterator(a_function, maps)
        functions = map(get_function, maps)
        iteratorss = map(get_iterators, maps)
        models =
            map(iterators -> ntuple(iterator -> missing, length(iterators)), iteratorss)
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
        new{typeof(combine),typeof(mushed)}(combine, mushed)
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
before they are scheduled to. Supports [`make_iterator`](@ref)
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
    Line(start, slope)

A line from `start` (unitless) with `slope` (with units per time like `1/s`). Supports
[`make_iterator`](@ref).

```jldoctest
julia> using AudioSchedules

julia> using Unitful: Hz, s

julia> first(make_iterator(Line(0, 1/s), 44100Hz))
0.0
```
"""
struct Line
    start::Float64
    slope::Rate
end
export Line

function make_iterator(line::Line, the_sample_rate)
    LineIterator(line.start, line.slope / the_sample_rate)
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

Cycles from 0 to 2π to repeat at a `frequency` (with frequency
units, like `Hz`). Supports [`make_iterator`](@ref).

```jldoctest
julia> using AudioSchedules

julia> using Unitful: Hz

julia> first(make_iterator(Cycles(440Hz), 44100Hz))
0.0
```
"""
struct Cycles
    frequency::Frequency
end

export Cycles

function make_iterator(cycles::Cycles, the_sample_rate)
    CyclesIterator(0, cycles.frequency / the_sample_rate * TAU)
end

"""
    Grow(start, rate)

Exponentially grow or decay from `start` (with units of sound volume like ``), at a
continuous `rate` (with units of proportion per time like `1/s`). Supports
[`make_iterator`](@ref).

```jldoctest
julia> using AudioSchedules

julia> using Unitful: Hz, s

julia> first(make_iterator(Grow(1, 1/s), 44100Hz))
1.0
```
"""
struct Grow
    start::Float64
    rate::Rate
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
    GrowIterator(grow.start, ℯ^(grow.rate / the_sample_rate))
end

"""
    Hook(rate, slope)

Make a hook shape, with an exponential curve growing at a continuous `rate` (with units per
time like `1/s`), followed by a line with `slope` (with units per time like  `1/s`). Use
with [`envelope`](@ref).

```jldoctest hook
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> envelope(1, Hook(1/s, 1/s) => 2s, ℯ + 1)
2-element Array{Any,1}:
 (Grow(1.0, 1.0 s^-1), 1.0 s)
 (Line(2.718281828459045, 1.0 s^-1), 1.0 s)
```
"""
struct Hook
    rate::Rate
    slope::Rate
end

export Hook

"""
    add_segment!(an_envelope, shape, duration, start_level, end_level)

Called by [`envelope`](@ref). Add a segment to `an_envelope` based on `shape`.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s

julia> an_envelope = [];

julia> add_segment!(an_envelope, 1, Grow, 1s, ℯ)

julia> an_envelope
1-element Array{Any,1}:
 (Grow(1.0, 1.0 s^-1), 1 s)
```
"""
function add_segment!(an_envelope, start_level, ::Type{Grow}, duration, end_level)
    push!(
        an_envelope,
        (Grow(start_level, log(end_level / start_level) / duration), duration),
    )
    nothing
end
export add_segment!

function add_segment!(an_envelope, start_level, ::Type{Line}, duration, end_level)
    push!(an_envelope, (Line(start_level, (end_level - start_level) / duration), duration))
    nothing
end

function add_segment!(an_envelope, start_level, hook::Hook, duration, end_level)
    rate = hook.rate
    slope = hook.slope
    solved = nlsolve([duration / 2 / s], autodiff = :forward) do residuals, arguments
        first_period = arguments[1]s
        residuals[1] =
            end_level + slope * (first_period - duration) -
            start_level * exp(rate * first_period)
    end
    if !solved.f_converged
        error("Unsolvable hook")
    end
    first_period = solved.zero[1]s
    push!(an_envelope, (Grow(start_level, rate), first_period))
    push!(
        an_envelope,
        (Line(start_level * exp(rate * first_period), slope), duration - first_period),
    )
    nothing
end

function envelope!(an_envelope, start_level, (shape, duration), end_level, more_segments...)
    add_segment!(an_envelope, start_level, shape, duration, end_level)
    envelope!(an_envelope, end_level, more_segments...)
    nothing
end
function envelope!(an_envelope, end_level)
    nothing
end

"""
    envelope(start_level, shape => duration, end_level, more_segments...)

For each envelope segment, call

```
add_segment!(an_envelope, start_level, shape, duration, end_level)
```

`duration` should have units of time (like `s`). For example,

```
envelope(0, Line => 1s, 1, Line => 1s, 0)
```

will add two segments:

```
add_segment!(an_envelope, 0, Line => 1s, 1)
add_segment!(an_envelope, 1, Line => 1s, 0)
```

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s

julia> envelope(0, Line => 1s, 1, Line => 1s, 0)
2-element Array{Any,1}:
 (Line(0.0, 1.0 s^-1), 1 s)
 (Line(1.0, -1.0 s^-1), 1 s)
```
"""
function envelope(more_segments...)
    an_envelope = []
    envelope!(an_envelope, more_segments...)
    an_envelope
end

export envelope

mutable struct AudioSchedule{InnerIterator} <: SampleSource
    outer_iterator::Vector{Tuple{InnerIterator,Int}}
    outer_state::Int
    inner_iterator::InnerIterator
    has_left::Int
    the_sample_rate::Frequency
end

const MapOfStatefuls = StrictMapIterator{<:Any,<:NTuple{<:Any,StrictStateful}}

function AudioSchedule(
    outer_iterator::Vector{Tuple{InnerIterator,Int}},
    the_sample_rate,
) where {InnerIterator}
    outer_result = iterate(outer_iterator)
    if outer_result === nothing
        error("AudioSchedules require at least one triple")
    end
    (inner_iterator, has_left), outer_state = outer_result
    AudioSchedule{InnerIterator}(
        outer_iterator,
        outer_state,
        inner_iterator,
        has_left,
        the_sample_rate,
    )
end

eltype(source::AudioSchedule) = Float64

nchannels(source::AudioSchedule) = 1

samplerate(source::AudioSchedule) = source.the_sample_rate / Hz

function length(source::AudioSchedule)
    sum((samples for (_, samples) in source.outer_iterator))
end

function map!(a_function, a_schedule::AudioSchedule)
    outer_iterator = a_schedule.outer_iterator
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
    extrema!(a_schedule::AudioSchedule)

Find the extrema of `a_schedule`. This will consume the schedule.

```jldoctest audio_schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> triple = (StrictMap(sin, Cycles(440Hz)), 0s, 1s);

julia> extrema!(AudioSchedule([triple, triple], 44100Hz)) .≈ (-1.9999995, 1.9999995)
(true, true)
```
"""
function extrema!(a_schedule::AudioSchedule)
    lower = Inf
    upper = -Inf
    for (iterator, number) in a_schedule.outer_iterator
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

function unsafe_read!(source::AudioSchedule, buf, frameoffset, framecount, from = 1)
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

function add_to_plan!(start_time, envelope::Vector, stateful_wave, the_sample_rate, orchestra, triggers)
    for (shape_synthesizer, duration) in envelope
        add_to_plan!(
            start_time,
            duration,
            StrictMapIterator(
                *,
                (
                    stateful_wave,
                    StrictStateful(make_iterator(shape_synthesizer, the_sample_rate)),
                ),
            ),
            the_sample_rate, orchestra, triggers
        )
        start_time = start_time + duration
    end
    nothing
end

function add_to_plan!(start_time, duration, iterator, the_sample_rate, orchestra, triggers)
    label = gensym("instrument")
    stop_time = start_time + duration
    orchestra[label] = iterator, false
    start_trigger = label, true
    if haskey(triggers, start_time)
        push!(triggers[start_time], start_trigger)
    else
        triggers[start_time] = [start_trigger]
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
    AudioSchedule(triples, the_sample_rate)

Return a `SampledSource`. `triples` should be a vector of triples in the form

```
(synthesizer, start_time, duration_or_envelope)
```

where `synthesizer` is anything that supports [`make_iterator`](@ref), `start_time` has
units of time (like `s`), and `duration_or_envelope` is either a duration (with units of
time, like `s`) or an [`envelope`](@ref).

```jldoctest audio_schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> an_envelope = envelope(0, Line => 1s, 1, Line => 1s, 0);

julia> triple = (StrictMap(sin, Cycles(440Hz)), 0s, an_envelope);

julia> a_schedule = AudioSchedule([triple], 44100Hz);
```

You can find the number of samples in an `AudioSchedule` with length.

```jldoctest audio_schedule
julia> the_length = length(a_schedule)
88200
```

You can use the schedule as a source for samples.

```jldoctest audio_schedule
julia> read(a_schedule, the_length);
```

The schedule must have at least one triple.

```jldoctest audio_schedule
julia> AudioSchedule([], 44100Hz)
ERROR: AudioSchedules require at least one triple
[...]
```
"""
function AudioSchedule(triples, the_sample_rate)
    triggers = SortedDict{Time,Vector{Tuple{Symbol,Bool}}}()
    orchestra = Dict{Symbol,Tuple{Any,Bool}}()
    for (synthesizer, start_time, duration_or_envelope) in triples
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
            trigger_time = (trigger_time_unitless)
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
    AudioSchedule(outer_iterator, the_sample_rate)
end

export AudioSchedule

"""
    schedule_within(triples, the_sample_rate; maximum_volume = 1.0)

Make an [`AudioSchedule`](@ref) with `triples` and `the_sample_rate`, then adjust the volume
to `maximum_volume`.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> triple = (StrictMap(sin, Cycles(440Hz)), 0s, 1s);

julia> a_schedule = schedule_within([triple, triple], 44100Hz);

julia> extrema!(a_schedule) .≈ (-1.0, 1.0)
(true, true)
```
"""
function schedule_within(
    triples,
    the_sample_rate;
    maximum_volume = 1.0,
)
    lower, upper = extrema!(AudioSchedule(triples, the_sample_rate))
    adjusted = AudioSchedule(triples, the_sample_rate)
    map!(let scale = 1.0 / max(abs(lower), abs(upper))
        @inline function (amplitude)
            amplitude * scale
        end
    end, adjusted)
    adjusted
end
export schedule_within

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
