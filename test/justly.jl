using AudioSchedules: AudioSchedule, Cycles, Envelope, Line, Plan, plan_within, @q_str, repeat!, schedule!, StrictMap
using FileIO: load, save
import LibSndFile
using Unitful: Hz, s
using Waveforms: sawtoothwave
import SampledSignals

cd("/home/brandon/Music")

const BEAT = 0.7s
const BEATS = 100

function justly(chords, the_sample_rate;
    key = 440Hz,
    seconds_per_beat = 1s,
    ramp = 0.1s,
)
    audio_schedule = AudioSchedule()
    clock = 0.0s
    for notes in chords
        ratio, beats = notes[1]
        key = key * ratio
        for (ratio, beats) in notes[2:end]
            schedule!(
                audio_schedule,
                StrictMap(sawtoothwave, Cycles(key * ratio)),
                clock,
                Envelope(
                    (0, 1, 1, 0),
                    (ramp, beats * seconds_per_beat - ramp - ramp, ramp),
                    (Line, Line, Line),
                ),
            )
        end
        clock = clock + beats * seconds_per_beat
    end
    plan_within(audio_schedule, the_sample_rate)
end

SONG = [
    [q"1" => 4, q"1" => 4, q"3/2" => 16, q"5/4o1" => 4], # darling you can
    [q"3/4" => 2, q"5/4" => 2, q"3/2o1" => 2], # count on
    [q"4/3" => 6, q"1" => 6, q"5/4o1" => 6], # me till the sun dries
    [q"3/4" => 2, q"5/4" => 2, q"3/2o1" => 2], # up the
    [q"4/3" => 2, q"1" => 4, q"5/4o1" => 2], # sea
    [q"2/3" => 2, q"5/4o1" => 2, q"o2" => 2], # until
    [q"15/8" => 2, q"6/5" => 2, q"3/2" => 2, q"o1" => 2], # then I'll
    [q"8/9" => 2, q"3/2o-1" => 2, q"6/5" => 2, q"o1" => 2], # always
    [q"9/5o-1" => 2, q"1" => 4, q"3/2" => 2, q"5/4o1" => 2], # be de
    [q"2/3" => 2, q"5/4o1" => 2, q"o2" => 2], # vo
    [q"9/8" => 4, q"5/4" => 4, q"o1" => 6, q"3/2o1" => 4], # ted to
    [q"4/3" => 2, q"1" => 2, q"5/4o1" => 2], # you

    [q"1" => 4, q"1" => 4, q"3/2" => 16, q"5/4o1" => 4], # I'll be your through
    [q"3/4" => 2, q"5/4" => 2, q"3/2o1" => 2], # endless
    [q"4/3" => 6, q"1" => 6, q"5/4o1" => 6], # time. I'll adore your
    [q"3/4" => 2, q"5/4" => 2, q"3/2o1" => 2], # charms sub
    [q"4/3" => 2, q"1" => 4, q"5/4o1" => 2], # lime
    [q"2/3" => 2, q"5/4o1" => 2, q"o2" => 2], # Guess by
    [q"15/8" => 2, q"6/5" => 2, q"3/2" => 2, q"o1" => 2], # now you
    [q"8/9" => 2, q"3/2o-1" => 2, q"6/5" => 2, q"o1" => 2], # know that
    [q"9/5o-1" => 2, q"1" => 4, q"3/2" => 2, q"5/4o1" => 2], # I'm de
    [q"2/3" => 2, q"5/4o1" => 2, q"o2" => 2], # vo
    [q"9/8" => 4, q"5/4" => 4, q"o1" => 6, q"3/2o1" => 4], # ted to
    [q"4/3" => 2, q"1" => 2, q"5/4o1" => 2], # you

    [q"5/9" => 4, q"3/2" => 4, q"6/5o1" => 4, q"o2" => 4], # I'll never hurt you
    [q"9/8o1" => 2, q"1" => 4, q"6/5" => 2, q"3/2" => 2], # I'll never
    [q"2/3" => 2, q"o1" => 4, q"6/5o1" => 2], # lie
    [q"4/3" => 2, q"6/5" => 2, q"o1" => 4], # I'll never
    [q"2/3" => 2, q"7/4" => 2, q"o1" => 6, q"5/4o1" => 2], # be un
    [q"4/3" => 2, q"1" => 4, q"5/4o1" => 4], # true
    [q"1" => 2, q"7/4" => 2], #
    [q"5/9" => 4, q"3/2" => 4, q"6/5o1" => 4, q"o2" => 4], # I'll never give you
    [q"9/8o1" => 2, q"1" => 4, q"6/5" => 2, q"3/2" => 2], # reasons to
    [q"2/3" => 2, q"o1" => 6, q"6/5o1" => 2], # cry
    [q"4/3" => 4, q"1" => 8, q"5/4" => 4, q"7/4" => 4], # I'd be unhappy if
    [q"2/3" => 2, q"o1" => 4, q"5/4o1" => 4], # you were
    [q"1" => 2, q"7/4" => 2], # blue

    [q"4/3" => 4, q"1" => 4, q"3/2" => 16, q"5/4o1" => 4], # Through the years our
    [q"3/4" => 2, q"5/4" => 2, q"3/2o1" => 2], # love will
    [q"4/3" => 6, q"1" => 6, q"5/4o1" => 6], # grow. Like a river,
    [q"3/4" => 2, q"5/4" => 2, q"3/2o1" => 2], # it will
    [q"4/3" => 2, q"1" => 4, q"5/4o1" => 2], # flow.
    [q"2/3" => 2, q"5/4o1" => 2, q"o2" => 2], # It can't
    [q"15/8" => 2, q"6/5" => 2, q"3/2" => 2, q"o1" => 2], # die be
    [q"8/9" => 2, q"3/2o-1" => 2, q"6/5" => 2, q"o1" => 2], # cause I'm
    [q"9/5o-1" => 2, q"1" => 4, q"3/2" => 2, q"5/4o1" => 2], # so de
    [q"2/3" => 2, q"5/4o1" => 2, q"o2" => 2], # vo
    [q"9/8" => 4, q"5/4" => 4, q"o1" => 6, q"3/2o1" => 4], # ted to
    [q"4/3" => 2, q"1" => 2, q"5/4o1" => 2], # you

    [q"5/9" => 4, q"3/2" => 4, q"6/5o1" => 4, q"o2" => 4], # I'll never hurt you
    [q"9/8o1" => 2, q"1" => 4, q"6/5" => 2, q"3/2" => 2], # I'll never
    [q"2/3" => 2, q"o1" => 4, q"6/5o1" => 2], # lie
    [q"4/3" => 2, q"6/5" => 2, q"o1" => 4], # I'll never
    [q"2/3" => 2, q"7/4" => 2, q"o1" => 6, q"5/4o1" => 2], # be un
    [q"4/3" => 2, q"1" => 4, q"5/4o1" => 4], # true
    [q"1" => 2, q"7/4" => 2], #
    [q"5/9" => 4, q"3/2" => 4, q"6/5o1" => 4, q"o2" => 4], # I'll never give you
    [q"9/8o1" => 2, q"1" => 4, q"6/5" => 2, q"3/2" => 2], # reasons to
    [q"2/3" => 2, q"o1" => 6, q"6/5o1" => 2], # cry
    [q"4/3" => 4, q"1" => 8, q"5/4" => 4, q"7/4" => 4], # I'd be unhappy if
    [q"2/3" => 2, q"o1" => 4, q"5/4o1" => 4], # you were
    [q"1" => 2, q"7/4" => 2], # blue

    [q"4/3" => 4, q"1" => 4, q"3/2" => 16, q"5/4o1" => 4], # Through the years our
    [q"3/4" => 2, q"5/4" => 2, q"3/2o1" => 2], # love will
    [q"4/3" => 6, q"1" => 6, q"5/4o1" => 6], # grow. Like a river,
    [q"3/4" => 2, q"5/4" => 2, q"3/2o1" => 2], # it will
    [q"4/3" => 2, q"1" => 4, q"5/4o1" => 2], # flow.
    [q"2/3" => 2, q"5/4o1" => 2, q"o2" => 2], # It can't
    [q"15/8" => 2, q"6/5" => 2, q"3/2" => 2, q"o1" => 2], # die be
    [q"8/9" => 2, q"3/2o-1" => 2, q"6/5" => 2, q"o1" => 2], # cause I'm
    [q"9/5o-1" => 2, q"1" => 4, q"3/2" => 2, q"5/4o1" => 2], # so de
    [q"2/3" => 2, q"5/4o1" => 2, q"o2" => 2], # vo
    [q"9/8" => 4, q"5/4" => 4, q"o1" => 8, q"3/2o1" => 4], # ted to
    [q"4/3" => 4, q"1" => 4, q"5/4o1" => 4], # you
]

plan = justly(SONG, 44100Hz, key = 161.81Hz, seconds_per_beat = BEAT)
save("music.wav", read(plan, length(plan)))

audio_schedule = AudioSchedule()

clunk = load("clunk.wav")
tap = load("tap.wav")
brrp = load("brrp.wav")

repeat!(audio_schedule, brrp, 0s, BEAT * 2, BEATS)
repeat!(audio_schedule, clunk, BEAT, BEAT * 2, BEATS)
repeat!(audio_schedule, clunk, 3 * BEAT / 2, BEAT * 2, BEATS)

plan = plan_within(audio_schedule, 44100Hz)
save("percussion.wav", read(plan, length(plan)))
