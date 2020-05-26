using PortAudio: PortAudioStream
using Unitful: Hz, s
import JSON
using ProfileView: @profview
using InteractiveUtils: @code_warntype

function make_envelope(duration)
    Envelope((0, 0.1, 0.1, 0), (0.1s, duration - 0.2s, 0.1s), (Line, Line, Line))
end

function justly!(schedule, song, key, seconds_per_beat)
    clock = 0.0s
    for chord in JSON.parse(song)
        notes = chord["notes"]
        new_key = notes[1]
        key = key * new_key["numerator"] / new_key["denominator"] * 2.0^new_key["octave"]
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
    "lyrics" : "ever I",
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
    "lyrics" : "want you",
    "notes": [
      {
        "numerator": 5,
        "denominator": 6,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 6,
        "denominator": 5,
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
        "numerator": 2,
        "denominator": 1,
        "octave": 0,
        "beats": 1
      }
    ]
  },
  {
    "lyrics": "all I have to",
    "notes": [
      {
        "numerator": 4,
        "denominator": 5,
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
    "lyrics": "do is",
    "notes": [
      {
        "numerator": 9,
        "denominator": 8,
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
      },
      {
        "numerator": 1,
        "denominator": 1,
        "octave": 1,
        "beats": 1
      }
    ]
  },
  {
    "lyrics": "dree",
    "notes": [
      {
        "numerator": 2,
        "denominator": 3,
        "octave": 1,
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
    "lyrics": "ea",
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
    "lyrics": "ea",
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
  },
  {
    "lyrics": "eam",
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
      },
      {
        "numerator": 7,
        "denominator": 4,
        "octave": 0,
        "beats": 1
      }
    ]
  },
  {
    "lyrics": "I can make you mine",
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
    "lyrics": "taste your lips of wine",
    "notes": [
      {
        "numerator": 15,
        "denominator": 8,
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
        "numerator": 6,
        "denominator": 5,
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
    "lyrics": "anytime night or",
    "notes": [
      {
        "numerator": 8,
        "denominator": 9,
        "octave": 0,
        "beats": 1
      },
      {
        "numerator": 6,
        "denominator": 5,
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
        "numerator": 2,
        "denominator": 1,
        "octave": 0,
        "beats": 1
      }
    ]
  },
  {
    "lyrics": "day",
    "notes": [
      {
        "numerator": 8,
        "denominator": 9,
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
      },
      {
        "numerator": 7,
        "denominator": 4,
        "octave": 0,
        "beats": 1
      }
    ]
  },
  {
    "lyrics": "Only trouble is",
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
    "lyrics": "gee whiz",
    "notes": [
      {
        "numerator": 15,
        "denominator": 8,
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
        "numerator": 6,
        "denominator": 5,
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
    "lyrics": "I'm dreamin my life a-",
    "notes": [
      {
        "numerator": 8,
        "denominator": 9,
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
      },
      {
        "numerator": 7,
        "denominator": 4,
        "octave": 0,
        "beats": 1
      }
    ]
  },
  {
    "lyrics": "way",
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
        "numerator": 7,
        "denominator": 4,
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
        "numerator": 2,
        "denominator": 3,
        "octave": 1,
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
a_schedule = AudioSchedule()
justly!(a_schedule, song, 440Hz, 1.0s)

a_plan = Plan(a_schedule, 44100Hz)
write(stream.sink, a_plan, length(a_plan))

a_plan = Plan(a_schedule, 44100Hz)
the_length = length(a_plan)
buf = Vector{Float64}(undef, the_length)
@profview unsafe_read!(a_plan, buf, 0, the_length)

a_schedule = AudioSchedule()
schedule!(a_schedule, InfiniteMap(x -> 0.1 * sin(x), Cycles(A)), 0 * BEAT, 3 * BEAT)
schedule!(a_schedule, InfiniteMap(x -> 0.1 * sin(x), Cycles(A * 6/5)), 1 * BEAT, 2 * BEAT)
schedule!(a_schedule, InfiniteMap(x -> 0.1 * sin(x), Cycles(A * 3/2)), 2 * BEAT, 1 * BEAT)
a_plan = Plan(a_schedule, SAMPLE_RATE)

write(stream.sink, a_plan, length(a_plan))
stream = PortAudioStream(samplerate = 44100)
