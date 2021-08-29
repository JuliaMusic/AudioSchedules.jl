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

"""
    make_iterator(synthesizer, sample_rate)

Return an iterator that will the play the `synthesizer` at `sample_rate` (with frequency units, like `Hz`).
The iterator should yield `Float64`s between -1 and 1.
Assumes that iterators will never end while they are scheduled.
"""
function make_iterator(a_map::Map, sample_rate)
    Iterators.map(a_map.a_function, map(let sample_rate = sample_rate
        function (synthesizer)
            make_iterator(synthesizer, sample_rate)
        end
    end, a_map.synthesizers)...)
end
export make_iterator

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

struct CyclesIterator
    start_level::Float64
    increment::Float64
end

IteratorSize(::Type{CyclesIterator}) = IsInfinite

IteratorEltype(::Type{CyclesIterator}) = HasEltype

eltype(::Type{CyclesIterator}) = Float64

const τ = 2 * π

function iterate(ring::CyclesIterator, state = ring.start_level)
    next_state = state + ring.increment
    if next_state >= τ
        next_state = next_state - τ
    end
    state, next_state
end

"""
    Cycles(frequency)

Cycles from 0 to 2π to repeat at a `frequency` (with frequency units, like `Hz`).
Supports [`make_iterator`](@ref).

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
    CyclesIterator(0, cycles.frequency / sample_rate * τ)
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

"""
    Grow(start_level, rate)

Exponentially grow or decay from `start_level` (unitless), at a continuous `rate` (with units per time like `1/s`).
Supports [`make_iterator`](@ref) and [`segments`](@ref).

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