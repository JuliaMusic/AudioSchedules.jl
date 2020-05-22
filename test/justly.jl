using AudioSchedules:
    AudioSchedule, Cycles, Envelope, InfiniteMap, Line, play, restart!, schedule!
using PortAudio: PortAudioStream
using Unitful: Hz, s
import JSON

function make_envelope(duration)
    Envelope((0, 0.1, 0.1, 0), (0.1s, duration - 0.2s, 0.1s), (Line, Line, Line))
end

function justly!(schedule, song, key, seconds_per_beat)
    clock = 0.0s
    for chord in JSON.parse(song)
        notes = chord["notes"]
        new_key = notes[1]
        key = key * new_key["numerator"] / new_key["denominator"] * 2^new_key["octave"]
        for note in notes[2:end]
            schedule!(
                schedule,
                InfiniteMap(
                    sin,
                    Cycles((
                        key * note["numerator"] / note["denominator"] * 2^note["octave"]
                    )),
                ),
                clock,
                make_envelope(note["beats"] * seconds_per_beat),
            )
        end
        clock = clock + new_key["beats"]s
    end
end

song = """
[
  {
    "notes": [
      {
        "numerator": 1,
        "denominator": 1,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 1,
        "denominator": 1,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 5,
        "denominator": 4,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 3,
        "denominator": 2,
        "octave": 0,
        "beats": 1
      }
    ]
  },
  {
    "notes": [
      {
        "numerator": 2,
        "denominator": 3,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 3,
        "denominator": 2,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 1,
        "denominator": 1,
        "octave": 1,
        "beats": 1
      },
      {
        "numerator": 5,
        "denominator": 4,
        "octave": 1,
        "beats": 1
      }
    ]
  },
  {
    "notes": [
      {
        "numerator": 3,
        "denominator": 2,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 1,
        "denominator": 1,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 5,
        "denominator": 4,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 3,
        "denominator": 2,
        "octave": 0,
        "beats": 1
      }
    ]
  }
]
"""

stream = PortAudioStream(samplerate = 44100)
a_schedule = AudioSchedule(stream.sink)
justly!(a_schedule, song, 440Hz, 1.0s)
play(a_schedule)
restart!(a_schedule)
play(schedule)
close(stream)
