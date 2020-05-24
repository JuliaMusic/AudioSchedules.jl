# AudioSchedules

[![Latest](https://img.shields.io/badge/docs-dev-blue.svg)](https://bramtayl.github.io/AudioSchedules.jl/dev)
[![CodeCov](https://codecov.io/gh/bramtayl/AudioSchedules.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/bramtayl/AudioSchedules.jl)

AudioSchedules allows you to schedule a sequence of overlapping audio synthesizers.
Critically, it allows you to do this without lags or gaps. To use `AudioSchedules`, create
a schedule with

```
AudioSchedule()
```

Then add synthesizers to the schedule using `schedule!`. You can add synthesizers at any
time for any duration. In addition, you can schedule synthesizers to be played using an
`Envelope`.

When you are done, convert your schedule to a `SampleSource` using `Plan`. This is the
first and only time you will need to specify a sample rate. You can find the number of
samples in your plan using `length`. Finally, you can `write` your source to any
SampleSink. See [SampledSignals](https://github.com/JuliaAudio/SampledSignals.jl) for
more information.

See the [documentation](https://bramtayl.github.io/AudioSchedules.jl/dev) for more details
and an example.
