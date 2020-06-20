const REFERENCE_LOG_10_FREQUENCIES = 1.3 : 0.1 : 4.1

const EXPONENT_INTERPOLATOR = CubicSplineInterpolation(REFERENCE_LOG_10_FREQUENCIES, [
    0.532,
    0.506,
    0.480,
    0.455,
    0.432,
    0.409,
    0.387,
    0.367,
    0.349,
    0.330,
    0.315,
    0.301,
    0.288,
    0.276,
    0.267,
    0.259,
    0.253,
    0.250,
    0.246,
    0.244,
    0.243,
    0.243,
    0.243,
    0.242,
    0.242,
    0.245,
    0.254,
    0.271,
    0.301
])

const LINEAR_INTERPOLATOR = CubicSplineInterpolation(REFERENCE_LOG_10_FREQUENCIES, [
    -31.6,
    -27.2,
    -23.0,
    -19.1,
    -15.9,
    -13.0,
    -10.3,
    -8.1,
    -6.2,
    -4.5,
    -3.1,
    -2.0,
    -1.1,
    -0.4,
    0.0,
    0.3,
    0.5,
    0.0,
    -2.7,
    -4.1,
    -1.0,
    1.7,
    2.5,
    1.2,
    -2.1,
    -7.1,
    -11.2,
    -10.7,
    -3.1
])

const THRESHOLD_INTERPOLATOR = CubicSplineInterpolation(REFERENCE_LOG_10_FREQUENCIES, [
    78.5,
    68.7,
    59.5,
    51.1,
    44.0,
    37.5,
    31.5,
    26.5,
    22.1,
    17.9,
    14.4,
    11.4,
    8.6,
    6.2,
    4.4,
    3.0,
    2.2,
    2.4,
    3.5,
    1.7,
    -1.3,
    -4.2,
    -6.0,
    -5.4,
    -1.5,
    6.0,
    12.6,
    13.9,
    12.3,
])

function equivalent_sound_pressure(phon, frequency)
    @assert 20 <= phon <= 80
    log_10_frequency = log10(frequency / Hz)
    exponent = EXPONENT_INTERPOLATOR(log_10_frequency)
    linear = LINEAR_INTERPOLATOR(log_10_frequency)
    threshold = THRESHOLD_INTERPOLATOR(log_10_frequency)
    A_f = 0.00447 * (10^(0.025 * phon) - 1.15) + (0.4 * (10^((threshold + linear) / 10 - 9)))^exponent
    ((10 / exponent * log10(A_f)) - linear + 94)dB * 20μPa
end

const MAXIMUM_PRESSURE = equivalent_sound_pressure(40, 20Hz)

"""
    equal_loudness(synthesizer::StrictMap{<:Any, Tuple{Cycles}})

Change the volume of a synthesizer so that sounds played at different frequencies will have
the same perceived volume. Assumes that the map function has a period of 2π.

```jldoctest
julia> using AudioSchedules

julia> using Unitful: Hz

julia> soft = equal_loudness(StrictMap(cos, Cycles(10000Hz)));

julia> first(make_iterator(soft, 44100Hz)) ≈ 0.0053035474
true
```

Technical details: uses the ISO 226:2003 curve for 40 phons. Scales output by a ratio
of the equivalent sound pressure at the current frequency to the equivalent sound pressure
at 20Hz (about as low as humans can hear).
"""
function equal_loudness(synthesizer::StrictMap{<:Any, Tuple{Cycles}})
    StrictMap(
        let multiplier = equivalent_sound_pressure(40, (synthesizer.synthesizers[1].frequency)) / MAXIMUM_PRESSURE
            function (amplitude)
                amplitude * multiplier
            end
        end,
        synthesizer
    )
end
export equal_loudness
