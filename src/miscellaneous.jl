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

Create a musical interval. You can specify a numerator (which defaults to 1), denominator (which defaults to 1), and an octave shift (which defaults to 0).

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

# TODO: use FFT here?
struct SawTooth{overtones} end

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

const ADJUST = 2 / pi

function (saw::SawTooth{overtones})(an_angle) where {overtones}
    ADJUST * sum(ntuple(let an_angle = an_angle
        function (overtone)
            sin_fast(overtone * an_angle) / overtone
        end
    end, Val{overtones}()))
end