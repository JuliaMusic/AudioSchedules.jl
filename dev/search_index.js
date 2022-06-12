var documenterSearchIndex = {"docs":
[{"location":"#Interface","page":"Interface","title":"Interface","text":"","category":"section"},{"location":"","page":"Interface","title":"Interface","text":"warning: Performance note\nPresumably due to the limits of inference, scheduling 16 or more synthesizers simultaneously will lead you off a performance cliff. Hopefully this limitation will go away in future versions of Julia.","category":"page"},{"location":"","page":"Interface","title":"Interface","text":"Modules = [AudioSchedules]","category":"page"},{"location":"","page":"Interface","title":"Interface","text":"Modules = [AudioSchedules]","category":"page"},{"location":"#AudioSchedules.AudioSchedule-Tuple{}","page":"Interface","title":"AudioSchedules.AudioSchedule","text":"AudioSchedule(; sample_rate = 44100Hz)\n\nCreate an empty AudioSchedule. You can push! new synthesizers to the audio_schedule. Provide 4 arguments to push!: the schedule, the synthesizer, the start time, and an envelope that you create with @envelope.\n\njulia> using AudioSchedules\n\njulia> audio_schedule = AudioSchedule()\n0.0 s 44100.0 Hz AudioSchedule\n\njulia> push!(audio_schedule, Map(sin, Cycles(440Hz)), 0s, @envelope(\n           0,\n           Line => 1s,\n           0.2,\n           Line => 1s,\n           0,\n       ))\n\njulia> push!(audio_schedule, Map(sin, Cycles(660Hz)), 2s, @envelope(\n            0,\n            Line => 1s,\n            0.2,\n            Line => 1s,\n            0,\n        ))\n\njulia> audio_schedule\n4.0 s 44100.0 Hz AudioSchedule\n\nYou can iterate over an AudioSchedule. Each element will just be a vector of amplitudes.\n\njulia> length(first(audio_schedule))\n44100\n\njulia> collect(AudioSchedule())\nAny[]\n\nYou can use write the audio_schedule directly to a PortAudio.PortAudioStream. The PortAudioStream must have exactly 1 output channel, and a matching sample rate.\n\njulia> using PortAudio: PortAudioStream\n\n\njulia> PortAudioStream(0, 1, warn_xruns = false) do stream\n           write(stream, audio_schedule)\n       end\n\njulia> PortAudioStream(0, 2) do stream\n            write(stream, audio_schedule)\n        end\nERROR: ArgumentError: PortAudioStream does not have 1 output channel\n[...]\n\njulia> PortAudioStream(0, 1, samplerate = 48000) do stream\n            write(stream, audio_schedule)\n        end\nERROR: ArgumentError: Sample rates of PortAudioStream (48000.0) and AudioSchedule (44100.0) do not match\n[...]\n\nYou can save an AudioSchedule as a SampledSignals.SampleBuf.\n\njulia> using SampledSignals: SampleBuf\n\n\njulia> saved = SampleBuf(audio_schedule)\n176400-frame, 1-channel SampleBuf{Float64, 1}\n4.0s sampled at 44100.0Hz\n▃▄▄▄▄▅▅▅▅▅▅▅▅▆▆▆▆▆▆▆▆▆▆▆▆▆▆▅▅▅▅▅▅▅▅▄▄▄▄▃▃▄▄▄▄▅▅▅▅▅▅▅▅▆▆▆▆▆▆▆▆▆▆▆▆▆▆▅▅▅▅▅▅▅▅▄▄▄▄▃\n\nYou can empty! an AudioSchedule and reuse it.\n\njulia> empty!(audio_schedule)\n\njulia> audio_schedule\n0.0 s 44100.0 Hz AudioSchedule\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.Cycles","page":"Interface","title":"AudioSchedules.Cycles","text":"Cycles(frequency)\n\nCycles from 0 to 2π to repeat at a frequency (with frequency units, like Hz). Supports make_series.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: Hz\n\n\njulia> first(make_series(Cycles(440Hz), 44100Hz))\n0.06268937721449021\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.Grow","page":"Interface","title":"AudioSchedules.Grow","text":"Grow(start_level, duration, end_level)\n\nExponentially grow or decay from start_level to end_level over a duration in time units like s. Supports make_series.\n\njulia> using AudioSchedules\n\n\njulia> using Unitful: Hz, s\n\n\njulia> first(make_series(Grow(0.1, 1s, 1), 44100Hz))\n0.10000522141770128\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.Line","page":"Interface","title":"AudioSchedules.Line","text":"Line(start_level, duration, end_level)\n\nA line from start_level to end_level with a duration in time units like s. Supports make_series.\n\njulia> using AudioSchedules\n\n\njulia> first(make_series(Line(0, 1s, 1), 44100Hz))\n2.2675736961451248e-5\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.Map","page":"Interface","title":"AudioSchedules.Map","text":"Map(a_function, synthesizers...)\n\nMap a_function over synthesizers. Supports make_series.\n\njulia> using AudioSchedules\n\n\njulia> first(make_series(Map(sin, Cycles(440Hz)), 44100Hz))\n0.06264832417874369\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.SawTooth-Tuple{Any}","page":"Interface","title":"AudioSchedules.SawTooth","text":"SawTooth(overtones)\n\nBuild a saw-tooth wave from its partials, starting with the fundamental (1), up to overtones.\n\nTo increase richness but also buziness, increase overtones.\n\njulia> using AudioSchedules\n\n\njulia> SawTooth(3)(π / 4)\n0.9185207636218614\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.Scale","page":"Interface","title":"AudioSchedules.Scale","text":"function Scale(ratio)\n\nA simple wrapper that will multiply inputs by the ratio.\n\njulia> using AudioSchedules\n\n\njulia> Scale(3)(2)\n6\n\n\n\n\n\n","category":"type"},{"location":"#AudioSchedules.duration-Tuple{AudioSchedule}","page":"Interface","title":"AudioSchedules.duration","text":"duration(audio_schedule::AudioSchedule)\n\nFind the duration of an AudioSchedule in seconds.\n\njulia> using AudioSchedules\n\njulia> audio_schedule = AudioSchedule();\n\njulia> push!(audio_schedule, Map(sin, Cycles(440Hz)), 0s, @envelope(\n            0,\n            Line => 1s,\n            1,\n            Line => 1s,\n            0,\n        ))\n\njulia> duration(audio_schedule)\n2.0 s\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.make_series-Tuple{Map, Any}","page":"Interface","title":"AudioSchedules.make_series","text":"make_series(synthesizer, sample_rate)\n\nReturn an iterator that will the play the synthesizer at sample_rate (with frequency units, like Hz). The iterator should yield Float64s between -1 and 1. Assumes that iterators will never end while they are scheduled.\n\n\n\n\n\n","category":"method"},{"location":"#Base.push!-Tuple{AudioSchedule, Any, Any, Any}","page":"Interface","title":"Base.push!","text":"push!(audio_schedule::AudioSchedule, synthesizer, start_time,\n    start_level, shape => duration, end_level, more_segments...\n)\n\nAdd a synthesizer to a AudioSchedule, where synthesizer is anything that supports make_series, start_time has units of time (like s), and the rest of the arguments specify the shape of the envelope.\n\nFor all envelope segment, call\n\nsegment(shape, start_level, duration, end_level)\n\nduration should have units of time (like s). For example,\n\npush!(audio_schedule, synthesizer, start_time, @envelope(0, Line => 1s, 1, Line => 1s, 0))\n\nwill call segment twice:\n\nsegment(Line, 0, 1s, 1)\nsegment(Line, 1, 1s, 0)\n\n\n\n\n\n","category":"method"},{"location":"#AudioSchedules.@envelope-Tuple","page":"Interface","title":"AudioSchedules.@envelope","text":"@envelope(arguments...)\n\nCreate an envelope. Start with the start amplitude (a number between 0 and 1). Then, specify a pair, segment_function => duration, where duration is the duration of the segment. Then, specify the end amplitude (again, a number between 0 and 1). So for example,\n\nFor example, @envelope(0, Line => 1s, 1) will create an envelope with 1 segment. The segment is created with segment_function(start_level, duration, end_level) => duration. So, for this example, the segment will be Line(0, 1s, 1) => 1s.\n\nAfter you finished your first segment, you can add as many more segments as you'd like. The end level of the previous segment will be the start level of the next segment.\n\nFor example, @envelope(0, Line => 1s, 1, Line => 1s, 0) will create an envelope with 2 segments:\n\nLine(0, 1s, 1) => 1s\nLine(1, 1s, 0) => 1s\n\njulia> using AudioSchedules\n\njulia> @envelope(0, Line => 1s, 1, Line => 1s, 0)\n(Line(0.0, 1.0 s⁻¹) => 1.0 s, Line(1.0, -1.0 s⁻¹) => 1.0 s)\n\njulia> @envelope(1, 2, 3)\nERROR: LoadError: ArgumentError: 2 is not a pair\n[...]\n\n\n\n\n\n","category":"macro"}]
}
