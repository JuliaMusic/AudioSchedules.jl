const POSITIVES = 1:∞

struct Skip{Synthesizer}
    synthesizer::Synthesizer
    time::FLOAT_SECONDS
end

function make_series(skip::Skip, sample_rate)
    make_series(skip.synthesizer, sample_rate)[
        (round(Int, skip.time * sample_rate)+1):end
    ]
end

"""
    Map(a_function, synthesizers...)

Map `a_function` over `synthesizers`.
Supports [`make_series`](@ref).

```jldoctest
julia> using AudioSchedules


julia> first(make_series(Map(sin, Cycles(440Hz)), 44100Hz))
0.06264832417874369
```
"""
struct Map{AFunction,Synthesizers}
    a_function::AFunction
    synthesizers::Synthesizers
    function Map(a_function::AFunction, synthesizers...) where {AFunction}
        new{AFunction,typeof(synthesizers)}(a_function, synthesizers)
    end
end
export Map

"""
    make_series(synthesizer, sample_rate)

Return an iterator that will the play the `synthesizer` at `sample_rate` (with frequency units, like `Hz`).
The iterator should yield `Float64`s between -1 and 1.
Assumes that iterators will never end while they are scheduled.
"""
function make_series(a_map::Map, sample_rate)
    broadcast(
        a_map.a_function,
        map(let sample_rate = sample_rate
            function (synthesizer)
                make_series(synthesizer, sample_rate)
            end
        end, a_map.synthesizers)...,
    )
end
export make_series

"""
    Line(start_level, duration, end_level)

A line from `start_level` to `end_level` with a `duration` in time units like `s`.
Supports [`make_series`](@ref).

```jldoctest
julia> using AudioSchedules


julia> first(make_series(Line(0, 1s, 1), 44100Hz))
2.2675736961451248e-5
```
"""
struct Line
    start_level::Float64
    slope::FLOAT_PER_SECOND
end
export Line

function make_series(line::Line, sample_rate)
    line.start_level .+ (line.slope / sample_rate) .* POSITIVES
end

precompile(make_series, (Line, FLOAT_HERTZ))

function Line(start_level, duration, end_level)
    Line(start_level, (end_level - start_level) / duration)
end

precompile(Line, (Float64, FLOAT_SECONDS, Float64))

const τ = 2 * π

# TODO: No need to cycle here; sin is periodic?
"""
    Cycles(frequency)

Cycles from 0 to 2π to repeat at a `frequency` (with frequency units, like `Hz`).
Supports [`make_series`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz


julia> first(make_series(Cycles(440Hz), 44100Hz))
0.06268937721449021
```
"""
struct Cycles
    frequency::FLOAT_HERTZ
end
export Cycles

function make_series(cycles::Cycles, sample_rate)
    ((cycles.frequency / sample_rate * τ) .* POSITIVES) .% τ
end

precompile(make_series, (Cycles, FLOAT_HERTZ))

"""
    Grow(start_level, duration, end_level)

Exponentially grow or decay from `start_level` to `end_level` over a `duration` in time units like `s`.
Supports [`make_series`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> first(make_series(Grow(0.1, 1s, 1), 44100Hz))
0.10000522141770128
```
"""
struct Grow
    start_level::Float64
    rate::FLOAT_PER_SECOND
end
export Grow

function make_series(grow::Grow, sample_rate)
    grow.start_level .* exp(grow.rate / sample_rate) .^ POSITIVES
end

precompile(make_series, (Grow, FLOAT_HERTZ))

function Grow(start_level, duration, end_level)
    Grow(start_level, log(end_level / start_level) / duration)
end

precompile(Grow, (Float64, FLOAT_SECONDS, Float64))
