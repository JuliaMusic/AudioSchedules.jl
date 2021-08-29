var documenterSearchIndex = {"docs":
[{"location":"#Interface","page":"Interface","title":"Interface","text":"","category":"section"},{"location":"","page":"Interface","title":"Interface","text":"warning: Performance note\nPresumably due to the limits of inference, scheduling 16 or more synthesizers simultaneously will lead you off a performance cliff. Hopefully this limitation will go away in future versions of Julia.","category":"page"},{"location":"","page":"Interface","title":"Interface","text":"Modules = [AudioSchedules]","category":"page"},{"location":"","page":"Interface","title":"Interface","text":"Modules = [AudioSchedules]","category":"page"},{"location":"#AudioSchedules.AudioSchedule-Tuple{}","page":"Interface","title":"AudioSchedules.AudioSchedule","text":"AudioSchedule(; sample_rate = 44100Hz)\n\nCreate an empty AudioSchedule. You can add! new synthesizers to the schedule. Specify a sample_rate with units per time, like 1/s or Hz.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: s, Hz\n\n\njulia> a_schedule = AudioSchedule()\n0.0 s 44100.0 Hz AudioSchedule\n\njulia> add!(a_schedule, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 0.05, Line => 1s, 1, Line => 1s, 0)\n\njulia> a_schedule\n3.0 s 44100.0 Hz AudioSchedule\n\nYou can use write the schedule directly to a PortAudioStream. However, to do so, the PortAudioStream must have a Weaver writer. The PortAudioStream must have exactly 0 input channels, 1 output channel, and a matching sample rate.\n\njulia> using PortAudio: PortAudioStream\n\njulia> PortAudioStream(0, 1, writer = Weaver()) do stream\n            write(stream, a_schedule)\n        end\n\njulia> a_schedule\n0.0 s 44100.0 Hz AudioSchedule\n\nAfter you play an AudioSchedule, it will be empty again. You can save it as a SampledSignals.SampleBuf if you want to play it again.\n\njulia> using SampledSignals: SampleBuf\n\njulia> new_schedule = AudioSchedule();\n\njulia> add!(new_schedule, Map(sin, Cycles(440Hz)), 0s, 0, Line => 1s, 0.05, Line => 1s, 1, Line => 1s, 0)\n\njulia> saved = SampleBuf(new_schedule)\n132300-frame, 1-channel SampleBuf{Float64, 2}\n3.0s sampled at 44100.0Hz\n▁▂▂▃▃▃▃▃▃▃▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▅▅▅▆▆▆▆▆▆▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▆▆▆▆▆▆▅▅▄\n\njulia> PortAudioStream(0, 1, warn_xruns = false) do stream\n            write(stream, saved)\n        end\n132300\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.Cycles","page":"Interface","title":"AudioSchedules.Cycles","text":"Cycles(frequency)\n\nCycles from 0 to 2π to repeat at a frequency (with frequency units, like Hz). Supports make_iterator.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: Hz\n\n\njulia> first(make_iterator(Cycles(440Hz), 44100Hz))\n0.0\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.Grow","page":"Interface","title":"AudioSchedules.Grow","text":"Grow(start_level, rate)\n\nExponentially grow or decay from start_level (unitless), at a continuous rate (with units per time like 1/s). Supports make_iterator and segments.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: Hz, s\n\n\njulia> first(make_iterator(Grow(1, 1 / s), 44100Hz))\n1.0\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.Hook","page":"Interface","title":"AudioSchedules.Hook","text":"Hook(rate, slope)\n\nMake a hook shape, with an exponential curve growing at a continuous rate (with units per time like 1/s), followed by a line with slope (with units per time like 1/s). Use with add!.  Supports segments. Not all hooks are solvable.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: Hz, s\n\n\njulia> a_schedule = AudioSchedule();\n\n\njulia> add!(a_schedule, Map(sin, Cycles(440Hz)), 0s, 1, Hook(1 / s, 1 / s) => 2s, ℯ + 1)\n\n\njulia> add!(a_schedule, Map(sin, Cycles(440Hz)), 0s, 1, Hook(1 / s, 1 / s) => 2s, 0)\nERROR: Unsolvable hook\n[...]\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.Line","page":"Interface","title":"AudioSchedules.Line","text":"Line(start_level, slope)\n\nA line from start_level (unitless) with slope (with units per time like 1/s). Supports make_iterator and segments.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: Hz, s\n\n\njulia> first(make_iterator(Line(0, 1 / s), 44100Hz))\n0.0\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.Map","page":"Interface","title":"AudioSchedules.Map","text":"Map(a_function, synthesizers...)\n\nMap a_function over synthesizers. Supports make_iterator.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: Hz\n\n\njulia> first(make_iterator(Map(sin, Cycles(440Hz)), 44100Hz))\n0.0\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.SawTooth-Tuple{Any}","page":"Interface","title":"AudioSchedules.SawTooth","text":"SawTooth(overtones)\n\nBuild a saw-tooth wave from its partials, starting with the fundamental (1), up to overtones.\n\nTo increase richness but also buziness, increase overtones.\n\njulia> using AudioSchedules\n\n\njulia> SawTooth(3)(π / 4)\n0.9185207636218614\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.Scale","page":"Interface","title":"AudioSchedules.Scale","text":"function Scale(ratio)\n\nA simple wrapper that will multiply inputs by the ratio.\n\njulia> using AudioSchedules\n\n\njulia> Scale(3)(2)\n6\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.Weaver","page":"Interface","title":"AudioSchedules.Weaver","text":"struct Weaver <: Scribe end\n\nA special PortAudio Scribe. You can use a Weaver to write an AudioSchedule directly to your speakers.\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.add!-Tuple{AudioSchedule, Any, Any, Any, Vararg{Any, N} where N}","page":"Interface","title":"AudioSchedules.add!","text":"add!(schedule::AudioSchedule, synthesizer, start_time,\n    start_level, shape => duration, end_level, more_segments...\n)\n\nAdd a synthesizer to a AudioSchedule, where synthesizer is anything that supports make_iterator, start_time has units of time (like s), and the rest of the arguments specify the shape of the envelope.\n\nFor all envelope segments, call\n\nsegments(shape, start_level, duration, end_level)\n\nduration should have units of time (like s). For example,\n\nadd!(schedule, synthesizer, start_time, 0, Line => 1s, 1, Line => 1s, 0)\n\nwill call segments twice:\n\nsegments(Line, 0, 1s, 1)\nsegments(Line, 1, 1s, 0)\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.equal_loudness-Tuple{Map{var\"#s31\", Tuple{Cycles}} where var\"#s31\"}","page":"Interface","title":"AudioSchedules.equal_loudness","text":"equal_loudness(synthesizer::Map{<:Any, Tuple{Cycles}})\n\nChange the volume of a synthesizer so that sounds played at different frequencies will have the same perceived volume. Assumes that the map function has a period of 2π.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: Hz\n\n\njulia> soft = equal_loudness(Map(cos, Cycles(10000Hz)));\n\n\njulia> first(make_iterator(soft, 44100Hz)) ≈ 0.0053035474\ntrue\n\nTechnical details: uses the ISO 226:2003 curve for 40 phons. Scales output by a ratio of the equivalent sound pressure at the current frequency to the equivalent sound pressure at 20Hz (about as low as humans can hear).\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.make_iterator-Tuple{Map, Any}","page":"Interface","title":"AudioSchedules.make_iterator","text":"make_iterator(synthesizer, sample_rate)\n\nReturn an iterator that will the play the synthesizer at sample_rate (with frequency units, like Hz). The iterator should yield Float64s between -1 and 1. Assumes that iterators will never end while they are scheduled.\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.segments-Tuple{Type{Line}, Any, Any, Any}","page":"Interface","title":"AudioSchedules.segments","text":"segments(shape, start_level, duration, end_level)\n\nCalled for each envelope segment passed to add!. Return a tuple of pairs in the form (segment, duration), where duration has units of time (like s), for a segment of shape shape.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: s\n\n\njulia> segments(Grow, 1, 1s, ℯ)\n((Grow(1.0, 1.0 s⁻¹), 1 s),)\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.@q_str-Tuple{AbstractString}","page":"Interface","title":"AudioSchedules.@q_str","text":"q\"interval\"\n\nCreate a musical interval. You can specify a numerator (which defaults to 1), denominator (which defaults to 1), and an octave shift (which defaults to 0).\n\njulia> using AudioSchedules\n\n\njulia> q\"1\"\n1//1\n\njulia> q\"3/2\"\n3//2\n\njulia> q\"2/3o1\"\n4//3\n\njulia> q\"2/3o-1\"\n1//3\n\njulia> q\"o2\"\n4//1\n\njulia> q\"1 + 1\"\nERROR: LoadError: Base.Meta.ParseError(\"Can't parse interval 1 + 1\")\n[...]\n\n\n\n\n\n","category":"macro"}]
}
