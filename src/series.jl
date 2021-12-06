const POSITIVES = 1:∞

"""
    Map(a_function, synthesizers...)

Map `a_function` over `synthesizers`. Supports [`make_series`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz


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
    Line(start_level, slope)

A line from `start_level` (unitless) with `slope` (with units per time like `1/s`). Supports
[`make_series`](@ref) and [`segments`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> first(make_series(Line(0, 1 / s), 44100Hz))
2.2675736961451248e-5
```
"""
struct Line
    start_level::Float64
    slope::RATE
end
export Line

function make_series(line::Line, sample_rate)
    line.start_level .+ (line.slope / sample_rate) .* POSITIVES
end

"""
    segments(shape, start_level, duration, end_level)

Called for each envelope segment passed to [`add!`](@ref). Return a tuple of pairs in the form `(segment, duration)`,
where duration has units of time (like `s`), for a segment of shape `shape`.

```jldoctest
julia> using AudioSchedules


julia> using Unitful: s


julia> segments(Grow, 1, 1s, ℯ)
((Grow(1.0, 1.0 s⁻¹), 1 s),)
```
"""
function segments(::Type{Line}, start_level, duration, end_level)
    ((Line(start_level, (end_level - start_level) / duration), duration),)
end
export segments

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
    frequency::FREQUENCY
end
export Cycles

function make_series(cycles::Cycles, sample_rate)
    ((cycles.frequency / sample_rate * τ) .* POSITIVES) .% τ
end

"""
    Grow(start_level, rate)

Exponentially grow or decay from `start_level` (unitless), at a continuous `rate` (with units per time like `1/s`).
Supports [`make_series`](@ref) and [`segments`](@ref).

```jldoctest
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> first(make_series(Grow(1, 1 / s), 44100Hz))
1.0000226759940578
```
"""
struct Grow
    start_level::Float64
    rate::RATE
end
export Grow

function make_series(grow::Grow, sample_rate)
    grow.start_level .* exp(grow.rate / sample_rate) .^ POSITIVES
end

function segments(::Type{Grow}, start_level, duration, end_level)
    ((Grow(start_level, log(end_level / start_level) / duration), duration),)
end

"""
    Hook(rate, slope)

Make a hook shape, with an exponential curve growing at a continuous `rate` (with units per time like `1/s`), followed by a line with `slope` (with units per time like `1/s`).
Use with [`add!`](@ref).
Supports [`segments`](@ref). Not all hooks are solvable.

```jldoctest hook
julia> using AudioSchedules


julia> using Unitful: Hz, s


julia> a_schedule = AudioSchedule();


julia> add!(a_schedule, Map(sin, Cycles(440Hz)), 0s, 1, Hook(1 / s, 1 / s) => 2s, ℯ + 1)


julia> add!(a_schedule, Map(sin, Cycles(440Hz)), 0s, 1, Hook(1 / s, 1 / s) => 2s, 0)
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
