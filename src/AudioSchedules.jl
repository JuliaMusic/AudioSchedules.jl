module AudioSchedules

# TODO: try out FFTW

import Base:
    eltype,
    extrema,
    iterate,
    IteratorEltype,
    IteratorSize,
    length,
    push_widen,
    read!,
    setindex!,
    show
using Base: Generator, IsInfinite, HasEltype, @kwdef, tail
using Base.FastMath: sin_fast
using Base.Iterators: repeated, Stateful, Repeated, Zip
using Base.Meta: ParseError
using DataStructures: SortedDict
using Interpolations: CubicSplineInterpolation
using NLsolve: nlsolve
using RegularExpressions: capture, CONSTANTS, of, pattern, raw, short
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: SampleSource, SampleBuf
using Base.Threads: @threads
using Unitful: Hz, μPa, dB, s

const TAU = 2 * pi
const TIME = typeof(1.0s)
const RATE = typeof(1.0 / s)
const FREQUENCY = typeof(1.0Hz)
const TRIGGERS = SortedDict{TIME, Vector{Tuple{Symbol, Bool}}}
const ORCHESTRA = Dict{Symbol, Tuple{Any, Bool}}

stateful(repeated::Repeated) = 
    repeated
stateful(iterator::Generator) = 
    Generator(iterator.f, stateful(iterator.iter))
stateful(iterator) = 
    Stateful(iterator)

"""
    skip(iterator, state, ahead)

`ahead = 1` is equivalent to getting the new state from `iterate(iterator, state)`.
Increasing `ahead` by `1` will be as if you iterated and then discarded.
"""
function skip(::Array, state, ahead)
    state + ahead
end

"""
    preview(iterator, state, ahead)

`ahead = 1` is equivalent to getting the item from `iterate(iterator, state)`.
Increasing `ahead` by `1` will be as if you iterated and then discarded.
"""
function preview(data::Array, state, ahead)
    data[state + ahead - 1]
end

function skip(::Repeated, __, ___)
    nothing
end

function preview(iterator::Repeated, _, __)
    iterator.x
end

function detach_state(a_map::Generator)
    stateful = a_map.iter
    iterator, item_state = detach_state(stateful)
    Generator(a_map.f, iterator), item_state
end

function attach_state!(a_map::Generator, item_state)
    attach_state!(a_map.iter, item_state)
    nothing
end

function detach_state(repeated::Repeated)
    repeated, nothing
end
function attach_state!(::Repeated, ::Nothing)
    nothing
end

function skip(a_map::Generator, state, ahead)
    skip(a_map.iter, state, ahead)
end

function preview(a_map::Generator, state, ahead)
    a_map.f(preview(a_map.iter, state, ahead))
end

function detach_state(a_zip::Zip)
    statefuls = a_zip.is
    iterators, item_states = map(tuple, map(detach_state, statefuls)...)
    zip(iterators...), item_states
end

function attach_state!(a_zip::Zip, item_states)
    map(attach_state!, a_zip.is, item_states)
    nothing
end

function skip(a_zip::Zip, states, ahead)
    map(let ahead = ahead
        function (iterator, state)
            skip(iterator, state, ahead)
        end
    end, a_zip.is, states)
end

function preview(a_zip::Zip, states, ahead)
    map(let ahead = ahead
        function (iterator, state)
            preview(iterator, state, ahead)
        end
    end, a_zip.is, states)
end

struct Laggy{Iterator}
    iterator::Iterator
end

IteratorSize(::Type{<:Laggy}) = IsInfinite()

IteratorEltype(::Type{<:Laggy{Iterator}}) where {Iterator} = IteratorEltype(Iterator)

eltype(::Type{<:Laggy{Iterator}}) where {Iterator} = eltype(Iterator)

function iterate(laggy::Laggy)
    iterate(laggy, iterate(laggy.iterator))
end
function iterate(::Laggy, ::Nothing)
    nothing
end
function iterate(laggy::Laggy, (last_item, state))
    last_item, iterate(laggy.iterator, state)
end

# if we set ahead = 2
# we get the next_state after state
# iterating state will give item, next_state. This is what would be correct if ahead = 1, because it is the new state
# so iterating next_state will give next_item, next_next_state. This is correct because ahead = 2
function skip(laggy::Laggy, (last_item, state), ahead)
    iterator = laggy.iterator
    iterate(iterator, skip(iterator, state, ahead - 1))
end

# if we set ahead = 2
# we preview iterator where ahead = 1
# and thus, get the item that goes with state
# last_item will be returned first, so this is the correct preview
function preview(laggy::Laggy, (last_item, state), ahead)
    preview(laggy.iterator, state, ahead - 1)
end

function detach_state(stateful::Stateful)
    Laggy(stateful.itr), stateful.nextvalstate
end

function attach_state!(stateful::Stateful, item_state)
    stateful.nextvalstate = item_state
    nothing
end

struct SawTooth{overtones} end

const ADJUST = 2 / pi

"""
    SawTooth(overtones)

Build a saw-tooth wave from its partials, starting with the fundamental (1), up to
`overtones`.

To increase richness but also buziness, increase `overtones`.

```jldoctest
julia> using AudioSchedules


julia> SawTooth(3)(π / 4)
0.9185207636218614
```
"""
SawTooth(overtones) = SawTooth{overtones}()

export SawTooth

function (saw::SawTooth{overtones})(an_angle) where {overtones}
    ADJUST * sum(ntuple(let an_angle = an_angle
        function (overtone)
            sin_fast(overtone * an_angle) / overtone
        end
    end, overtones))
end

"""
    make_iterator(synthesizer, sample_rate)

Return an iterator that will the play the `synthesizer` at `sample_rate` (with frequency
units, like `Hz`). The iterator should yield ratios between -1 and 1. Assumes that iterators
will never end while they are scheduled. In addition to supporting `iterate`, iterators should
also support [`AudioSchedules.preview`](@ref) and [`AudioSchedules.skip`](@ref), and iteration 
should have no side effects.

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz


julia> using FileIO: load


julia> using LibSndFile: LibSndFile


julia> cd(joinpath(pkgdir(AudioSchedules), "test"))


julia> first(make_iterator(load("clunk.wav"), 44100Hz))
0.00168Q0f15

julia> make_iterator(load("clunk.wav"), 48000Hz)
ERROR: ArgumentError: Sample rate mismatch
[...]
```
"""
function make_iterator(buffer::SampleBuf, sample_rate)
    if (buffer.samplerate)Hz != sample_rate
        throw(ArgumentError("Sample rate mismatch"))
    end
    buffer.data
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
struct Map{AFunction, Synthesizers}
    a_function::AFunction
    synthesizers::Synthesizers
    function Map(a_function::AFunction, synthesizers...) where {AFunction}
        new{AFunction, typeof(synthesizers)}(a_function, synthesizers)
    end
end
export Map

struct Splat{AFunction}
    a_function::AFunction
end
(splat::Splat)(arguments) = splat.a_function(arguments...)

repeated_0(iterator::Repeated) = iterator.x + 1 ≈ 1
repeated_0(_) = false

repeated_1(iterator::Repeated) = iterator.x ≈ 1
repeated_1(_) = false

function map_iterator(a_function, iterators...)
    Generator(a_function, iterators...)
end
function map_iterator(::typeof(+), iterators...)
    useful = filter(!repeated_0, iterators)
    useful_length = length(useful)
    if useful_length == 0
        Repeated(0)
    elseif useful_length == 1
        useful[1]
    else
        Generator(+, iterators...)
    end
end
function map_iterator(::typeof(*), iterators...)
    useful = filter(!repeated_1, iterators)
    useful_length = length(useful)
    if useful_length == 0
        Repeated(1)
    elseif useful_length == 1
        useful[1]
    else
        Generator(*, iterators...)
    end
end

function make_iterator(a_map::Map, sample_rate)
    map_iterator(
        a_map.a_function,
        map(let sample_rate = sample_rate
            function (synthesizer)
                make_iterator(synthesizer, sample_rate)
            end
        end, a_map.synthesizers)...,
    )
end

struct LineIterator
    start_level::Float64
    increment::Float64
end

IteratorSize(::Type{LineIterator}) = IsInfinite

IteratorEltype(::Type{LineIterator}) = HasEltype

eltype(::Type{LineIterator}) = Float64

function iterate(line::LineIterator, state = line.start_level)
    state, state + line.increment
end

function skip(line::LineIterator, state, ahead)
    state + ahead * line.increment
end

"""
    Line(start_level, slope)

A line from `start_level` (unitless) with `slope` (with units per time like `1/s`). Supports
[`make_iterator`](@ref) and [`segments`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> first(make_iterator(Line(0, 1 / s), 44100Hz))
0.0
```
"""
struct Line
    start_level::Float64
    slope::RATE
end
export Line

function make_iterator(line::Line, sample_rate)
    if abs(line.slope) < 1 / 1000s
        repeated(line.start_level)
    else
        LineIterator(line.start_level, line.slope / sample_rate)
    end
end

"""
    segments(shape, start_level, duration, end_level)

Called for each envelope segment passed to [`add!`](@ref). Return a tuple of pairs in the form `(segment, duration)`,
where duration has units of time (like `s`), for a segment of shape `shape`.

```jldoctest
julia> using AudioSchedules


julia> using Unitful: s


julia> segments(Grow, 1, 1s, ℯ)
((Grow(1.0, 1.0 s^-1), 1 s),)
```
"""
function segments(::Type{Line}, start_level, duration, end_level)
    ((Line(start_level, (end_level - start_level) / duration), duration),)
end
export segments

struct CyclesIterator
    start_level::Float64
    increment::Float64
end

IteratorSize(::Type{CyclesIterator}) = IsInfinite

IteratorEltype(::Type{CyclesIterator}) = HasEltype

eltype(::Type{CyclesIterator}) = Float64

function iterate(ring::CyclesIterator, state = ring.start_level)
    next_state = state + ring.increment
    if next_state >= TAU
        next_state = next_state - TAU
    end
    state, next_state
end

function skip(cycles::CyclesIterator, state, ahead)
    mod2pi(state + ahead * cycles.increment)
end

"""
    Cycles(frequency)

Cycles from 0 to 2π to repeat at a `frequency` (with frequency units, like `Hz`). Supports
[`make_iterator`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz


julia> first(make_iterator(Cycles(440Hz), 44100Hz))
0.0
```
"""
struct Cycles
    frequency::FREQUENCY
end
export Cycles

function make_iterator(cycles::Cycles, sample_rate)
    CyclesIterator(0, cycles.frequency / sample_rate * TAU)
end

struct GrowIterator
    start_level::Float64
    multiplier::Float64
end

IteratorSize(::Type{GrowIterator}) = IsInfinite

IteratorEltype(::Type{GrowIterator}) = HasEltype

eltype(::Type{GrowIterator}) = Float64

function iterate(grow::GrowIterator, state = grow.start_level)
    state, state * grow.multiplier
end

function skip(grow::GrowIterator, state, ahead)
    mod2pi(state * grow.multiplier^ahead)
end

function preview(iterator::Union{LineIterator, CyclesIterator, GrowIterator}, state, ahead)
    skip(iterator, state, ahead - 1)
end

"""
    Grow(start_level, rate)

Exponentially grow or decay from `start_level` (unitless), at a continuous `rate` (with units per
time like `1/s`). Supports [`make_iterator`](@ref) and [`segments`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> first(make_iterator(Grow(1, 1 / s), 44100Hz))
1.0
```
"""
struct Grow
    start_level::Float64
    rate::RATE
end
export Grow

function make_iterator(grow::Grow, sample_rate)
    GrowIterator(grow.start_level, exp(grow.rate / sample_rate))
end

function segments(::Type{Grow}, start_level, duration, end_level)
    ((Grow(start_level, log(end_level / start_level) / duration), duration),)
end

"""
    Hook(rate, slope)

Make a hook shape, with an exponential curve growing at a continuous `rate` (with units per
time like `1/s`), followed by a line with `slope` (with units per time like  `1/s`). Use
with [`add!`](@ref). Supports [`segments`](@ref). Not all hooks are solvable.

```jldoctest hook
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> plan = Plan(44100Hz)
Plan with triggers at ()

julia> add!(plan, Map(sin, Cycles(440Hz)), 0s, 1, Hook(1 / s, 1 / s) => 2s, ℯ + 1)

julia> add!(plan, Map(sin, Cycles(440Hz)), 0s, 1, Hook(1 / s, 1 / s) => 2s, 0)
ERROR: Unsolvable hook
[...]
```
"""
struct Hook
    rate::RATE
    slope::RATE
end
export Hook

function segments(hook::Hook, start_level, duration, end_level)
    rate = hook.rate
    slope = hook.slope
    solved = nlsolve(
        let start_level = start_level,
            end_level = end_level,
            rate = rate,
            slope = slope,
            duration = duration

            function (residuals, arguments)
                first_period = arguments[1]s
                residuals[1] =
                    end_level + slope * (first_period - duration) -
                    start_level * exp(rate * first_period)
                nothing
            end
        end,
        [duration / 2 / s],
        autodiff = :forward,
    )
    if !solved.f_converged
        error("Unsolvable hook")
    end
    first_period = solved.zero[1]s
    (Grow(start_level, rate), first_period),
    (Line(start_level * exp(rate * first_period), slope), duration - first_period)
end

mutable struct AudioSchedule{Iterator} <: SampleSource
    channel::Channel{Tuple{Iterator, Int}}
    sample_rate::FREQUENCY
    stateful::Union{Iterator, Nothing}
    has_left::Int
end

export AudioSchedule

AudioSchedule(channel::Channel{Tuple{Iterator, Int}}, sample_rate, stateful, has_left) where {Iterator} = 
    AudioSchedule{Iterator}(channel, sample_rate, stateful, has_left)

function AudioSchedule(channel, sample_rate)
    if isopen(channel) || isready(channel)
        AudioSchedule(channel, sample_rate, take!(channel)...)
    else
        AudioSchedule(channel, sample_rate, nothing, 0)
    end
end

eltype(::AudioSchedule) = Float64

nchannels(source::AudioSchedule) = 1

samplerate(source::AudioSchedule) = source.sample_rate / Hz

function show(io::IO, ::AudioSchedule)
    print(io, "AudioSchedule")
end

@noinline function update_peak(stateful, samples, peak)
    iterator, state = detach_state(stateful)
    for index in 1:samples
        item, state = iterate(iterator, state)
        peak = max(peak, abs(item))
    end
    peak
end

@noinline function inner_fill!(stateful, buf, a_range)
    iterator, state = detach_state(stateful)
    offset = first(a_range) - 1
    @threads for index in a_range
        @inbounds buf[index] = preview(iterator, state, index - offset)
    end
    attach_state!(stateful, skip(iterator, state, length(a_range)))
    nothing
end

function unsafe_read!(source::AudioSchedule, buf, frameoffset, framecount, from = 1)
    has_left = source.has_left
    stateful = source.stateful
    empties = framecount - from + 1
    if empties < has_left
        source.has_left = has_left - empties
        inner_fill!(stateful, buf, from:framecount)
        framecount
    else
        until = from + has_left - 1
        if has_left > 0
            inner_fill!(stateful, buf, from:until)
        end
        channel = source.channel
        if isopen(channel) || isready(channel)
            source.stateful, source.has_left = take!(channel)
            unsafe_read!(source, buf, frameoffset, framecount, until + 1)
        else
            until
        end
    end
end

"""
    Plan(sample_rate)

Create an empty plan for an audio schedule. Use [`add!`](@ref) to add new synthesizers for plan. Specify a `sample_rate`
with units per time, like `1/s`.

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz


julia> Plan(44100Hz)
Plan with triggers at ()
```
"""
struct Plan
    sample_rate::RATE
    orchestra::ORCHESTRA
    triggers::TRIGGERS
end

function duration(plan::Plan)
    triggers = plan.triggers
    if length(triggers) > 0
        last(triggers)[1] - first(triggers)[1]
    else
        0.0s
    end
end

export Plan

function show(io::IO, plan::Plan)
    print(io, "Plan with triggers at $((keys(plan.triggers)...,))")
end

Plan(sample_rate) = Plan(sample_rate, ORCHESTRA(), TRIGGERS())

function make_envelope(start_level, (shape, duration), end_level, more_segments...)
    segments(shape, start_level, duration, end_level)...,
    make_envelope(end_level, more_segments...)...
end
function make_envelope(_)
    ()
end

function add_iterator!(plan::Plan, iterator, start_time, duration)
    stop_time = start_time + duration
    label = gensym("segment")
    orchestra = plan.orchestra
    triggers = plan.triggers
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

function triples(sample_rate, synthesizer, start_time, envelope...)
    time_box = Ref(start_time * 1.0)
    stateful_wave = stateful(make_iterator(synthesizer, sample_rate))
    map(
        function ((shape_synthesizer, duration),)
            time = time_box[]
            time_box[] = time + duration
            (
                map_iterator(
                    *,
                    stateful_wave,
                    stateful(make_iterator(shape_synthesizer, sample_rate)),
                ),
                time,
                duration
            )
        end,
        make_envelope(envelope...)
    )
end

"""
    add!(plan::Plan, synthesizer, start_time)

Add a synthesizer to the plan, where `synthesizer` must support [`make_iterator`](@ref) and [`duration`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using FileIO: load


julia> using LibSndFile: LibSndFile

julia> using Unitful: Hz, s


julia> cd(joinpath(pkgdir(AudioSchedules), "test"))

julia> plan = Plan(44100Hz)
Plan with triggers at ()

julia> add!(plan, load("clunk.wav"), 0s)

julia> plan
Plan with triggers at (0.0 s, 0.3518820861678005 s)

julia> a_schedule = AudioSchedule(plan)
AudioSchedule

julia> read(a_schedule, duration(plan))
15518-frame, 1-channel SampleBuf{Float64, 2}
0.3518820861678005s sampled at 44100.0Hz
▇▇▇▇▇▇▇▆▆▆▆▅▅▅▅▅▅▅▅▅▅▅▅▅▅▄▅▅▄▄▅▄▄▄▄▄▄▄▄▄▄▄▃▄▄▄▃▄▄▃▄▄▄▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▂▃▃▃▂▃▃▂▂▂
```
"""
function add!(plan, synthesizer, start_time)
    add_iterator!(
        plan,
        stateful(make_iterator(synthesizer, plan.sample_rate)),
        start_time,
        duration(synthesizer),
    )
end

"""
    add!(plan::Plan, synthesizer, start_time,
        start_level, shape => duration, end_level, more_segments...
    )

Add a synthesizer to a [`Plan`](@ref), where `synthesizer` is anything that supports [`make_iterator`](@ref),
`start_time` has units of time (like `s`), and rest of the arguments specify the shape of the envelope.

For all envelope [`segments`](@ref), call

```
segments(shape, start_level, duration, end_level)
```

`duration` should have units of time (like `s`). For example,

```
add!(plan, synthesizer, start_time, 0, Line => 1s, 1, Line => 1s, 0)
```

will call segments twice:

```
segments(Line, 0, 1s, 1)
segments(Line, 1, 1s, 0)
```

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> plan = Plan(44100Hz)
Plan with triggers at ()

julia> add!(plan, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 1, Line => 1s, 0)


julia> plan
Plan with triggers at (0.0 s, 1.0 s, 2.0 s)

julia> empty_plan = Plan(44100Hz)
Plan with triggers at ()

julia> read(AudioSchedule(empty_plan), duration(empty_plan))
0-frame, 1-channel SampleBuf{Float64, 2}
0.0s sampled at 44100.0Hz
```
"""
function add!(plan::Plan, synthesizer, start_time, piece_1, rest...)
    sample_rate = plan.sample_rate
    triggers = plan.triggers
    orchestra = plan.orchestra
    stateful_wave = stateful(make_iterator(synthesizer, sample_rate))
    for (shaped, time, duration) in triples(sample_rate, synthesizer, start_time, piece_1, rest...)
        add_iterator!(plan, shaped, time, duration)
    end
    nothing
end

export add!

# TODO: reset!

"""
    AudioSchedule(plan::Plan)

Return a `SampledSource` from a [`Plan`](@ref).

```jldoctest audio_schedule
julia> using AudioSchedules


julia> using Unitful: s, Hz


julia> plan = Plan(44100Hz)
Plan with triggers at ()

julia> add!(plan, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 1, Line => 1s, 1, Line => 1s, 0)


julia> a_schedule = AudioSchedule(plan)
AudioSchedule
```

You can find get the duration of the `Plan` corresponding to an `AudioSchedule`.

```jldoctest audio_schedule
julia> the_duration = duration(plan)
3.0 s
```

You can use the schedule as a source for samples.

```jldoctest audio_schedule
julia> read(a_schedule, the_duration)
132300-frame, 1-channel SampleBuf{Float64, 2}
3.0s sampled at 44100.0Hz
▄▅▅▆▆▆▆▆▆▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▆▆▆▆▆▆▅▅▄
```
"""
function AudioSchedule(plan::Plan)
    triggers = plan.triggers
    orchestra = plan.orchestra
    sample_rate = plan.sample_rate
    channel = Channel{Tuple{Any, Int}}(length(triggers))
    time = 0.0s
    for (trigger_time, trigger_list) in pairs(triggers)
        samples = round(Int, (trigger_time - time) * sample_rate)
        time = trigger_time
        iterators = ((iterator for (iterator, is_on) in values(orchestra) if is_on)...,)
        for (label, is_on) in trigger_list
            iterator, _ = orchestra[label]
            orchestra[label] = iterator, is_on
        end
        if length(iterators) > 0 && samples > 0
            put!(channel, (map_iterator(+, iterators...), samples))
        end
    end
    close(channel)
    AudioSchedule(channel, sample_rate)
end

# TODO: figure out a way to avoid peaking !!!

"""
    function Scale(ratio)

A simple wrapper that will multiply inputs by the ratio.

```jldoctest
julia> using AudioSchedules


julia> Scale(3)(2)
6
```
"""
struct Scale{Ratio}
    ratio::Ratio
end

(scale::Scale)(thing) = thing * scale.ratio

export Scale

const DIGITS = of(:maybe, raw("-")), of(:some, short(:digit))
const QUOTIENT = pattern(
    CONSTANTS.start,
    of(:maybe, capture(DIGITS..., name = "numerator")),
    of(:maybe, raw("/"), capture(DIGITS..., name = "denominator")),
    of(:maybe, "o", capture(DIGITS..., name = "octave")),
    CONSTANTS.stop,
)

get_parse(something, _) = parse(Int, something)
get_parse(::Nothing, default) = default

function q_str(interval_string)
    a_match = match(QUOTIENT, interval_string)
    if a_match === nothing
        throw(Meta.ParseError("Can't parse interval $interval_string"))
    end
    get_parse(a_match["numerator"], 1) // get_parse(a_match["denominator"], 1) *
    (2 // 1)^get_parse(a_match["octave"], 0)
end

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
ERROR: LoadError: Base.Meta.ParseError("Can't parse interval 1 + 1")
[...]
```
"""
macro q_str(interval_string::AbstractString)
    esc(q_str(interval_string))
end
export @q_str

"""
    duration(synthesizer)

Get the duration of a synthesizer (with units of time, like `s`), for synthesizers with an
inherent length.

```jldoctest
julia> using AudioSchedules


julia> using FileIO: load


julia> using LibSndFile: LibSndFile


julia> using Unitful: Hz


julia> cd(joinpath(pkgdir(AudioSchedules), "test"))


julia> duration(load("clunk.wav"))
0.3518820861678005 s
```
"""
function duration(buffer::SampleBuf)
    (length(buffer) / samplerate(buffer))s
end
export duration

include("equal_loudness.jl")

end
