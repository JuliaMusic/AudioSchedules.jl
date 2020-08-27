module AudioSchedules

import Base:
    eltype,
    extrema,
    iterate,
    IteratorEltype,
    IteratorSize,
    length,
    map,
    push_widen,
    read!,
    setindex!,
    show
using Base: Generator, IsInfinite, HasEltype, @kwdef, tail
using Base.Iterators: Zip
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

"""
    get_duration(synthesizer)

Get the duration of a synthesizer (with units of time, like `s`), for synthesizers with an
inherent length.

```jldoctest
julia> using AudioSchedules


julia> using FileIO: load


julia> using LibSndFile: LibSndFile


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

function detach_state(a_map::Generator)
    stateful = a_map.iter
    iterator, item_state = detach_state(stateful)
    Generator(a_map.f, iterator), item_state
end

function attach_state!(a_map::Generator, item_state)
    attach_state!(a_map.iter, item_state)
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

function iterate(laggy::Laggy, (last_item, state) = iterate(laggy.iterator))
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

mutable struct InfiniteStateful{Iterator, Item, State}
    iterator::Iterator
    item::Item
    state::State
end

IteratorSize(::Type{<:InfiniteStateful}) = IsInfinite()

IteratorEltype(::Type{<:InfiniteStateful}) = HasEltype()

eltype(::Type{<:InfiniteStateful{<:Any, Item, <:Any}}) where {Item} = Item

function InfiniteStateful(iterator)
    InfiniteStateful(iterator, iterate(iterator)...)
end

function iterate(stateful::InfiniteStateful, state = nothing)
    last_item = stateful.item
    stateful.item, stateful.state = iterate(stateful.iterator, stateful.state)
    last_item, nothing
end

function detach_state(stateful::InfiniteStateful)
    Laggy(stateful.iterator), (stateful.item, stateful.state)
end

function attach_state!(stateful::InfiniteStateful, item_state)
    stateful.item, stateful.state = item_state
    nothing
end

struct SawTooth{overtones} end

"""
    SawTooth(overtones)

Build a saw-tooth wave from its partials, starting with the fundamental (1), up to
`overtones`.

To increase richness but also buziness, increase `overtones`.

```jldoctest
julia> using AudioSchedules


julia> SawTooth(3)(π / 4)
1.4428090415820634
```
"""
SawTooth(overtones) = SawTooth{overtones}()

export SawTooth

function (saw::SawTooth{overtones})(an_angle) where {overtones}
    sum(ntuple(let an_angle = an_angle
        function (overtone)
            sin(overtone * an_angle) / overtone
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

function map_iterator(a_function, iterators...)
    Generator(Splat(a_function), zip(iterators...))
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
    LineIterator(line.start_level, line.slope / sample_rate)
end

"""
    segments(shape, start_level, duration, end_level)

Called by [`envelope`](@ref). Return a tuple of pairs in the form `(segment, duration)`,
where duration has units of time (like `s`), with a segment of shape `shape`.

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


julia> plan = Plan(44100Hz);


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
    statefuls_samples::Vector{Tuple{Iterator, Int}}
    sample_rate::FREQUENCY
    state::Int
    stateful::Iterator
    has_left::Int
    first_item_state::Any
end

export AudioSchedule

function initialize(statefuls_samples)
    stateful_samples_state = iterate(statefuls_samples)
    if stateful_samples_state === nothing
        throw(ArgumentError("AudioSchedules require at least one synthesizer"))
    end
    (stateful, has_left), state = stateful_samples_state
    _, first_item_state = detach_state(stateful)
    state, stateful, has_left, first_item_state
end

function AudioSchedule(
    statefuls_samples::Vector{Tuple{Iterator, Int}},
    sample_rate,
) where {Iterator}
    AudioSchedule{Iterator}(
        statefuls_samples,
        sample_rate,
        initialize(statefuls_samples)...,
    )
end

eltype(::AudioSchedule) = Float64

nchannels(source::AudioSchedule) = 1

samplerate(source::AudioSchedule) = source.sample_rate / Hz

function length(source::AudioSchedule)
    sum((samples for (_, samples) in source.statefuls_samples))
end

@noinline function update_peak(stateful, samples, peak)
    iterator, state = detach_state(stateful)
    for index in 1:samples
        item, state = iterate(iterator, state)
        peak = max(peak, abs(item))
    end
    peak
end

"""
    seek_peek(a_schedule::AudioSchedule)

Find the maximum absolute amplitude in an AudioSchedule.

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> plan = Plan(44100Hz);


julia> add!(plan, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 1, Line => 1s, 0)


julia> seek_peak(AudioSchedule(plan))
0.9994267666261519
```
"""
function seek_peak(a_schedule::AudioSchedule)
    peak = 0
    for (stateful, samples) in a_schedule.statefuls_samples
        peak = update_peak(stateful, samples, peak)
    end
    peak
end
export seek_peak

@noinline function switch_iterator!(source, buf, frameoffset, framecount, ::Nothing, until)
    source.state, source.stateful, source.has_left, source.first_item_state =
        initialize(source.statefuls_samples)
    until
end
@noinline function switch_iterator!(
    source,
    buf,
    frameoffset,
    framecount,
    stateful_samples_state,
    until,
)
    (stateful, source.has_left), source.state = stateful_samples_state
    _, source.first_item_state = detach_state(stateful)
    source.stateful = stateful
    unsafe_read!(source, buf, frameoffset, framecount, until + 1)
end

@noinline function inner_fill!(stateful, buf, a_range)
    iterator, state = detach_state(stateful)
    offset = first(a_range) - 1
    @threads for index in a_range
        buf[index] = preview(iterator, state, index - offset)
    end
    attach_state!(stateful, skip(iterator, state, length(a_range)))
    nothing
end

function unsafe_read!(source::AudioSchedule, buf, frameoffset, framecount, from = 1)
    has_left = source.has_left
    stateful = source.stateful
    empties = framecount - from + 1
    if (has_left > empties)
        source.has_left = has_left - empties
        inner_fill!(stateful, buf, from:framecount)
        framecount
    else
        until = from + has_left - 1
        inner_fill!(stateful, buf, from:until)
        attach_state!(stateful, source.first_item_state)
        switch_iterator!(
            source,
            buf,
            frameoffset,
            framecount,
            iterate(source.statefuls_samples, source.state),
            until,
        )
    end
end

"""
    Plan(sample_rate)

Create an empty plan for an audio schedule. Use [`add!`](@ref) to add new synthesizers for plan. Specify a `sample_rate`
with units per time, like `1/s`.

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz


julia> Plan(44100Hz);
```
"""
struct Plan
    sample_rate::RATE
    orchestra::ORCHESTRA
    triggers::TRIGGERS
end

export Plan

Plan(sample_rate) = Plan(sample_rate, ORCHESTRA(), TRIGGERS())

function make_envelope(start_level, (shape, duration), end_level, more_segments...)
    segments(shape, start_level, duration, end_level)...,
    make_envelope(end_level, more_segments...)...
end
function make_envelope(end_level)
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

"""
    add!(plan::Plan, synthesizer, start_time)

Add a synthesizer to the plan, where `synthesizer` must support [`make_iterator`](@ref) and [`get_duration`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using FileIO: load


julia> using LibSndFile: LibSndFile

julia> using Unitful: Hz, s


julia> cd(joinpath(pkgdir(AudioSchedules), "test"))

julia> plan = Plan(44100Hz);

julia> add!(plan, load("clunk.wav"), 0s)

julia> schedule = AudioSchedule(plan);

julia> read(schedule, length(schedule));
```
"""
function add!(plan, synthesizer, start_time)
    add_iterator!(
        plan,
        InfiniteStateful(make_iterator(synthesizer, plan.sample_rate)),
        start_time,
        get_duration(synthesizer),
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


julia> plan = Plan(44100Hz);


julia> add!(plan, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 1, Line => 1s, 0)


julia> collect(keys(plan.triggers)) == [0.0s, 1.0s, 2.0s]
true

julia> AudioSchedule(Plan(44100Hz))
ERROR: ArgumentError: AudioSchedules require at least one synthesizer
[...]
```
"""
function add!(plan::Plan, synthesizer, start_time, piece_1, rest...)
    sample_rate = plan.sample_rate
    triggers = plan.triggers
    orchestra = plan.orchestra
    stateful_wave = InfiniteStateful(make_iterator(synthesizer, sample_rate))
    for (shape_synthesizer, duration) in make_envelope(piece_1, rest...)
        add_iterator!(
            plan,
            map_iterator(
                *,
                stateful_wave,
                InfiniteStateful(make_iterator(shape_synthesizer, sample_rate)),
            ),
            start_time,
            duration,
        )
        start_time = start_time + duration
    end
    nothing
end

export add!

# TODO: reset!

const SUM_OF = Generator{<:Zip{<:Tuple}, Splat{typeof(+)}}

"""
    AudioSchedule(plan::Plan)

Return a `SampledSource` from a [`Plan`](@ref).

```jldoctest audio_schedule
julia> using AudioSchedules


julia> using Unitful: s, Hz


julia> plan = Plan(44100Hz);


julia> add!(plan, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 1, Line => 1s, 0)


julia> a_schedule = AudioSchedule(plan);

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
"""
function AudioSchedule(plan::Plan)
    triggers = plan.triggers
    orchestra = plan.orchestra
    sample_rate = plan.sample_rate
    statefuls_samples = Tuple{SUM_OF, Int}[]
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
            push!(statefuls_samples, (map_iterator(+, iterators...), samples))
        end
    end
    AudioSchedule(statefuls_samples, sample_rate)
end

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

"""
    map(a_function, schedule::AudioSchedule)

Map a function over all of the synthesizers in the schedule.

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> plan = Plan(44100Hz);


julia> add!(plan, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 1, Line => 1s, 0)


julia> schedule = AudioSchedule(plan);


julia> seek_peak(map(Scale(2), schedule))
1.9988535332523039
```
"""
function map(
    a_function::AFunction,
    schedule::AudioSchedule{Iterator},
) where {AFunction, Iterator}
    mapped = Vector{Tuple{Generator{<:Iterator, AFunction}, Int}}(
        undef,
        length(schedule.statefuls_samples),
    )
    map!(
        let a_function = a_function
            function ((iterator, samples),)
                Generator(a_function, iterator), samples
            end
        end,
        mapped,
        schedule.statefuls_samples,
    )
    AudioSchedule(mapped, schedule.sample_rate)
end
export within

const DIGITS = of(:maybe, raw("-")), of(:some, short(:digit))
const QUOTIENT = pattern(
    CONSTANTS.start,
    of(:maybe, capture(DIGITS..., name = "numerator")),
    of(:maybe, raw("/"), capture(DIGITS..., name = "denominator")),
    of(:maybe, "o", capture(DIGITS..., name = "octave")),
    CONSTANTS.stop,
)

get_parse(something, default) = parse(Int, something)
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

include("equal_loudness.jl")

end
