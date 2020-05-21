using Unitful: s, Hz
using PortAudio: PortAudioStream
using ProfileView: @profview
using InteractiveUtils: @code_warntype, @which

stream = PortAudioStream(samplerate = 44100)
sink = stream.sink
schedule = AudioSchedule(sink)
envelope = Envelope((0, 0.25, 0), (1s, 1s), (Line, Line))
schedule!(schedule, Map((@fastmath sin), Cycles(440Hz)), 0s, envelope)
schedule!(schedule, Map((@fastmath sin), Cycles(440Hz)), 2s, envelope)
schedule!(schedule, Map((@fastmath sin), Cycles(550Hz)), 2s, envelope)
start_time = 0.0
triggers = schedule.triggers
orchestra = schedule.orchestra
all_triggers = collect(pairs(triggers))
(end_time, trigger_list) = all_triggers[1]
for (label, is_on) in trigger_list
    orchestra[label].is_on = is_on
end
start_time = end_time
(end_time, trigger_list) = all_triggers[2]
instruments = ((instrument for instrument in values(orchestra) if instrument.is_on)...,)
states = map((@inline function (instrument)
    instrument.state
end), instruments)
source = IteratorSource(
    make_iterator(
        Map((@fastmath +), map((@inline function (instrument)
            instrument.iterator
        end), instruments)...),
        samplerate(sink),
    ),
    states,
    1,
    samplerate(sink),
)

g = source.iterator.iter.is[1]
@code_warntype iterate(g) # NOT INFERRED
@code_warntype iterate(g.iter) # INFERRED
@code_warntype g.f(iterate(g.iter)[1]) # INFERRED
