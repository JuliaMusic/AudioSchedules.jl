module AudioSchedules

import Base:
    eltype, extrema, iterate, IteratorEltype, IteratorSize, length, read!, setindex!, show
using Base: Generator, IsInfinite, HasEltype
using Base.Iterators: cycle, Stateful, take
using DataStructures: SortedDict
using Interpolations: CubicSplineInterpolation
using NLsolve: nlsolve
using RegularExpressions: capture, CONSTANTS, of, pattern, raw, short
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: SampleSource, SampleBuf
const TAU = 2 * pi
using Unitful: Hz, μPa, dB, s

const Time = typeof(1.0s)
const Rate = typeof(1.0 / s)
const Frequency = typeof(1.0Hz)

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
            sum(ntuple(let an_angle = an_angle
                @inline function (overtone)
                    sin(overtone * an_angle) / overtone
                end
            end, overtones))
        end
    end
end
export compound_wave

"""
    get_duration(synthesizer)

Get the duration of a synthesizer (with units of time, like `s`), for synthesizers with an
inherent length.

```jldoctest
julia> using AudioSchedules

julia> using FileIO: load

julia> import LibSndFile

julia> cd(joinpath(pkgdir(AudioSchedules), "test"))

julia> get_duration(load("clunk.wav"))
0.3518820861678005 s
```
"""
function get_duration(buffer::SampleBuf)
    (length(buffer) / buffer.samplerate)s
end
export get_duration

"""
    make_iterator(synthesizer, the_sample_rate)

Return an iterator that will the play the `synthesizer` at `the_sample_rate` (with frequency
units, like `Hz`). The iterator should yield ratios between -1 and 1. Iterators will
typically be infinite, with endings determined by the schedule instead.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: Hz

julia> using FileIO: load

julia> import LibSndFile

julia> cd(joinpath(pkgdir(AudioSchedules), "test"))

julia> first(make_iterator(load("clunk.wav"), 44100Hz))
0.00168Q0f15
```
"""
function make_iterator(buffer::SampleBuf, the_sample_rate)
    # TODO: support resampling
    @assert (buffer.samplerate)Hz == the_sample_rate
    cycle(buffer.data)
end

export make_iterator

"""
    Map(a_function, synthesizers...)

Map `a_function` over `synthesizers`. Supports [`make_iterator`](@ref).

```jldoctest
julia> using AudioSchedules

julia> using Unitful: Hz

julia> first(make_iterator(Map(sin, Cycles(440Hz)), 44100Hz))
0.0
```
"""
struct Map{AFunction,Synthesizers}
    a_function::AFunction
    synthesizers::Synthesizers
    Map(a_function::AFunction, synthesizers...) where {AFunction} =
        new{AFunction,typeof(synthesizers)}(a_function, synthesizers)
end
export Map

function map_iterator(a_function, iterators...)
    Generator(let a_function = a_function
        function (items)
            a_function(items...)
        end
    end, zip(iterators...))
end

function make_iterator(a_map::Map, the_sample_rate)
    map_iterator(a_map.a_function, map(a_map.synthesizers) do synthesizer
        make_iterator(synthesizer, the_sample_rate)
    end...)
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

Exponentially grow or decay from `start` (unitless), at a continuous `rate` (with units per
time like `1/s`). Supports [`make_iterator`](@ref).

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
((Grow(1.0, 1.0 s^-1), 1.0 s), (Line(2.718281828459045, 1.0 s^-1), 1.0 s))
```
"""
struct Hook
    rate::Rate
    slope::Rate
end

export Hook

"""
    segments(an_envelope, shape, duration, start_level, end_level)

Called by [`envelope`](@ref). Return a tuple of pairs in the form ``(segment, duration)`,
where duration has units of time (like `s`).

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s

julia> segments(1, Grow, 1s, ℯ)
((Grow(1.0, 1.0 s^-1), 1 s),)
```
"""
function segments(start_level, ::Type{Grow}, duration, end_level)
    ((Grow(start_level, log(end_level / start_level) / duration), duration),)
end
export segments

function segments(start_level, ::Type{Line}, duration, end_level)
    ((Line(start_level, (end_level - start_level) / duration), duration),)
end

function segments(start_level, hook::Hook, duration, end_level)
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
    (Grow(start_level, rate), first_period),
    (Line(start_level * exp(rate * first_period), slope), duration - first_period)
end

"""
    envelope(start_level, shape => duration, end_level, more_segments...)

For each envelope segment, call

```
segments(start_level, shape, duration, end_level)
```

`duration` should have units of time (like `s`). For example,

```
envelope(0, Line => 1s, 1, Line => 1s, 0)
```

will add two segments:

```
segments(0, Line, 1s, 1)
segments(1, Line, 1s, 0)
```

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s

julia> envelope(0, Line => 1s, 1, Line => 1s, 0)
((Line(0.0, 1.0 s^-1), 1 s), (Line(1.0, -1.0 s^-1), 1 s))
```
"""
function envelope(start_level, (shape, duration), end_level, more_segments...)
    segments(start_level, shape, duration, end_level)...,
    envelope(end_level, more_segments...)...
end
function envelope(end_level)
    ()
end

export envelope

mutable struct AudioSchedule <: SampleSource
    outer_iterator::Vector{Tuple{Any,Int}}
    outer_state::Int
    inner_iterator::Any
    has_left::Int
    the_sample_rate::Frequency
end

eltype(source::AudioSchedule) = Float64

nchannels(source::AudioSchedule) = 1

samplerate(source::AudioSchedule) = source.the_sample_rate / Hz

function length(source::AudioSchedule)
    sum((samples for (_, samples) in source.outer_iterator))
end

# TODO: optimize
@noinline function seek_extrema!(iterator, number, lower, upper)
    new_lower, new_upper = extrema(take(iterator, number))
    min(lower, new_lower), max(upper, new_upper)
end

"""
    extrema!(a_schedule::AudioSchedule)

Find the extrema of `a_schedule`. This will consume the schedule.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> triple = (Map(sin, Cycles(440Hz)), 0s, 1s);

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

# TODO: optimize
@noinline function inner_fill!(iterator, buf, a_range)
    item, state = iterate(iterator)
    for index in a_range
        buf[index] = item
        item, state = iterate(iterator)
    end
    nothing
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

function add_to_plan!(
    start_time,
    envelope::Tuple,
    stateful_wave,
    the_sample_rate,
    orchestra,
    triggers,
)
    for (shape_synthesizer, duration) in envelope
        add_to_plan!(
            start_time,
            duration,
            map_iterator(
                *,
                stateful_wave,
                Stateful(make_iterator(shape_synthesizer, the_sample_rate)),
            ),
            the_sample_rate,
            orchestra,
            triggers,
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

julia> triple = (Map(sin, Cycles(440Hz)), 0s, an_envelope);

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
            Stateful(make_iterator(synthesizer, the_sample_rate)),
            the_sample_rate,
            orchestra,
            triggers,
        )
    end
    time = 0.0s
    outer_iterator = Tuple{Any,Int}[]
    for (trigger_time, trigger_list) in pairs(triggers)
        samples = round(Int, (trigger_time - time) * the_sample_rate)
        time = trigger_time
        iterators = ((iterator for (iterator, is_on) in values(orchestra) if is_on)...,)
        for (label, is_on) in trigger_list
            iterator, _ = orchestra[label]
            orchestra[label] = iterator, is_on
        end
        if samples > 0 && length(iterators) > 0
            push!(outer_iterator, (map_iterator(+, iterators...), samples))
        end
    end
    outer_result = iterate(outer_iterator)
    if outer_result === nothing
        error("AudioSchedules require at least one triple")
    end
    (inner_iterator, has_left), outer_state = outer_result
    AudioSchedule(outer_iterator, outer_state, inner_iterator, has_left, the_sample_rate)
end

export AudioSchedule

"""
    schedule_within(triples, the_sample_rate; maximum_volume = 1.0)

Make an [`AudioSchedule`](@ref) with `triples` and `the_sample_rate`, then adjust the volume
to `maximum_volume`.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> triple = (Map(sin, Cycles(440Hz)), 0s, 1s);

julia> a_schedule = schedule_within([triple, triple], 44100Hz);

julia> extrema!(a_schedule) .≈ (-1.0, 1.0)
(true, true)
```
"""
function schedule_within(triples, the_sample_rate; maximum_volume = 1.0)
    lower, upper = extrema!(AudioSchedule(triples, the_sample_rate))
    adjusted = AudioSchedule(triples, the_sample_rate)
    outer_iterator = adjusted.outer_iterator
    scale = 1.0 / max(abs(lower), abs(upper))
    map!(
        function ((iterator, number),)
            Generator(let scale = scale
                @inline function (amplitude)
                    amplitude * scale
                end
            end, iterator), number
        end,
        outer_iterator,
        outer_iterator,
    )
    adjusted
end
export schedule_within

const DIGITS = of(:maybe, raw("-")), of(:some, short(:digit))
const QUOTIENT = pattern(
    CONSTANTS.start,
    of(:maybe, capture(DIGITS..., name = "numerator")),
    of(:maybe, raw("/"), capture(DIGITS..., name = "denominator")),
    of(:maybe, "o", capture(DIGITS..., name = "octave")),
    CONSTANTS.stop
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

julia> q"1 + 1"
ERROR: LoadError: Can't parse interval 1 + 1
[...]
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
