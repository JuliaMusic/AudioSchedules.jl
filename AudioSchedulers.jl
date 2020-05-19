module AudioSchedulers

import Base: iterate, eltype, IteratorSize, read!, show, setindex!
using Base: Generator, IsInfinite, RefValue
using Base.Iterators: repeated
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: SampleSource, s, Hz
using DataStructures: OrderedDict
using PortAudio: PortAudioStream
using FunctionWrappers: FunctionWrapper

const TAU = 2 * pi

function broadcast_reduce(a_function)
    let a_function = a_function
        function (samples)
            broadcast(a_function, samples...)
        end
    end
end

struct IteratorSource{Iterator,State} <: SampleSource
    iterator::Iterator
    state_box::RefValue{State}
    nchannels::Int
    samplerate::Float64
end
eltype(::IteratorSource) = Float64
nchannels(source::IteratorSource) = source.nchannels
samplerate(source::IteratorSource) = source.samplerate
function unsafe_read!(source::IteratorSource, buf::Matrix, frameoffset, framecount)
    iterator = source.iterator
    state_box = source.state_box
    state = state_box[]
    rows = size(buf)[1]
    for row = 1:rows
        result = iterate(iterator, state)
        if result === nothing
            state_box[] = state
            return row - 1
        else
            item, state = iterate(iterator, state)
            buf[row, :] .= item
        end
    end
    state_box[] = state
    return rows
end

"""
    Envelope(levels, durations, shapes)

Shapes are all functions of the form:

    shape(samplerate, start_value, end_value, duration) -> iterator

`durations` and `levels` list the time and level of the boundaries of segments of the
envelope. For example,

    Envelope([0.0, 1.0, 1.0, 0.0], [.05, 0.9, 0.05], [line, line, line])

will create an envelope with three segments:

    line(samplerate, 0.0, 1.0, 0.05)
    line(samplerate, 1.0, 1.0, 0.9)
    line(samplerate, 1.0, 0.0, 0.05)

See the example for [`AudioScheduler`](@ref).
"""
struct Envelope{Levels,Durations,Shapes}
    levels::Levels
    durations::Durations
    shapes::Shapes
    function Envelope(
        levels::Levels,
        durations::Durations,
        shapes::Shapes,
    ) where {Levels,Durations,Shapes}
        @assert length(durations) == length(shapes) == length(levels) - 1
        new{Levels,Durations,Shapes}(levels, durations, shapes)
    end
end

"""
    IteratorSource(intended_sink, iterator)

Create a source of audio samples for an `indended_sink` using an `iterator`.

```jldoctest
julia> using AudioScheduler

julia> using Base: Generator

julia> const SAMPLE_RATE = 44100

julia> stream = PortAudioStream(samplerate = SAMPLE_RATE);

julia> sink = stream.sink;

julia> iterator = Generator(sin, cycles(SAMPLE_RATE, 440))

julia> write(sink, IteratorSource(sink, iterator), SAMPLE_RATE)

julia> close(stream)
```
"""
function IteratorSource(intended_sink, iterator)
    _, state = iterate(iterator)
    state_box = Ref(state)
    IteratorSource(iterator, state_box, nchannels(intended_sink), samplerate(intended_sink))
end

struct AudioScheduler{Sink}
    orchestra::Dict{Symbol,Any}
    triggers::OrderedDict{Float64,Vector{FunctionWrapper{Nothing,Tuple{}}}}
    sink::Sink
end

"""
    AudioScheduler(sink)

Create a `AudioScheduler` to schedule changes to sink. Add an iterator to the schedule with
[`schedule!`](@ref). Then, you can play it with [`play`](@ref).

```jldoctest
julia> using AudioScheduler

julia> using PortAudio: PortAudioStream

julia> using Base: Generator

julia> const SAMPLE_RATE = 44100

julia> stream = PortAudioStream(samplerate = SAMPLE_RATE)

julia> scheduler = AudioScheduler(stream.sink)
AudioScheduler with triggers at Float64[] seconds

julia> envelope = Envelope((0.0, 0.25, 0.25, 0.0), (0.5, 0.5, 0.5), (line, line, line));

julia> schedule!(scheduler, Generator(sin, cycles(SAMPLE_RATE, 440)), 0.0, envelope)

julia> schedule!(scheduler, Generator(sin, cycles(SAMPLE_RATE, 440)), 1.5, envelope)

julia> schedule!(scheduler, Generator(sin, cycles(SAMPLE_RATE, 550)), 1.5, envelope)

julia> scheduler
AudioScheduler with triggers at [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0] seconds

julia> play(scheduler) # laggy due to compilation

julia> play(scheduler) # well, still not great

julia> close(stream)
```
"""
AudioScheduler(sink) = AudioScheduler(
    Dict{Symbol,Any}(),
    OrderedDict{Float64,Vector{FunctionWrapper{Nothing,Tuple{}}}}(),
    sink,
)

"""
    schedule!(scheduler::AudioScheduler, iterator, start_time, duration)

schedule an `iterator` to be added to the `scheduler`, starting at `start_time` and lasting for
`duration`. You can also pass an [`Envelope`](@ref) as a duration. See the example for
[`AudioScheduler`](@ref). Note: the scheduler will discard the first sample in the iterator
during scheduling.
"""
function schedule!(
    scheduler::AudioScheduler,
    iterator,
    start_time,
    duration
)
    orchestra = scheduler.orchestra
    triggers = scheduler.triggers
    label = gensym("instrument")
    stop_time = start_time + duration
    _, state = iterate(iterator)
    on_trigger = let orchestra = orchestra, label = label, iterator = iterator, state_box = Ref(state)
        function ()
            # note: will discard first sample in the iterator
            orchestra[label] = (iterator, state_box)
            return nothing
        end
    end
    if haskey(triggers, start_time)
        push!(triggers[start_time], on_trigger)
    else
        triggers[start_time] = [on_trigger]
    end
    off_trigger = let orchestra = orchestra, label = label
        function ()
            delete!(orchestra, label)
            return nothing
        end
    end
    if haskey(triggers, stop_time)
        push!(triggers[stop_time], off_trigger)
    else
        triggers[stop_time] = [off_trigger]
    end
    nothing
end

function schedule!(scheduler::AudioScheduler, iterator, start_time, envelope::Envelope)
    the_samplerate = samplerate(scheduler.sink)
    durations = envelope.durations
    levels = envelope.levels
    shapes = envelope.shapes
    for index = 1:length(durations)
        duration = durations[index]
        schedule!(
            scheduler,
            Generator(
                broadcast_reduce(*),
                zip(
                    iterator,
                    shapes[index](the_samplerate, levels[index], levels[index+1], duration),
                ),
            ),
            start_time,
            duration,
        )
        start_time = start_time + duration
    end
end

show(io::IO, scheduler::AudioScheduler) =
    print(io, "AudioScheduler with triggers at $(keys(scheduler.triggers)) seconds")

@noinline function _play(sink, start_time, end_time, ::Tuple{})
    write(sink, IteratorSource(sink, repeated(0.0)), (end_time - start_time)s)
    nothing
end
@noinline function _play(sink, start_time, end_time, iterator_state_boxes)
    iterators = map(first, iterator_state_boxes)
    state_boxes = map(last, iterator_state_boxes)
    iterator = Generator(broadcast_reduce(+), zip(iterators...))
    state_box = Ref(map(getindex, state_boxes))
    write(
        sink,
        IteratorSource(iterator, state_box, length(first(iterator)), samplerate(sink)),
        (end_time - start_time)s,
    )
    map(setindex!, state_boxes, state_box[])
    nothing
end

"""
    play(scheduler::AudioScheduler)

Play a [`AudioScheduler`](@ref). Note the first time a scheduer is played will be laggy due to
compilation time; successive plays should sound better. See the example for
[`AudioScheduler`](@ref).
"""
function play(scheduler::AudioScheduler)
    start_time = 0
    orchestra = scheduler.orchestra
    triggers = scheduler.triggers
    for (end_time, triggers) in pairs(triggers)
        _play(scheduler.sink, start_time, end_time, (values(orchestra)...,))
        for trigger in triggers
            trigger()
        end
        start_time = end_time
    end
end

struct Ring
    start::Float64
    plus::Float64
end
IteratorSize(::Type{Ring}) = IsInfinite()
eltype(::Type{Ring}) = Float64
function iterate(ring::Ring, state = ring.start) where {Element}
    next_state = state + ring.plus
    if next_state >= TAU
        next_state = next_state - TAU
    end
    state, next_state
end

"""
    cycles(samplerate, frequency)

Rotate an angle starting with 0 at `frequency` rotations per second and `samplerate` samples
per second. Return an angle in radians.

```jldoctest
julia> using AudioScheduler

julia> using Base.Iterators: take

julia> collect(take(cycles(2, 1), 4))
4-element Array{Float64,1}:
 0.0
 3.141592653589793
 0.0
 3.141592653589793
```
"""
function cycles(samplerate, frequency)
    # radians_per_cycle = (cycles / second) / (samples / second) * (radian / cycle)
    Ring(0, frequency / samplerate * TAU)
end

struct Ramp
    start::Float64
    plus::Float64
end
IteratorSize(::Type{Ramp}) = IsInfinite()
eltype(::Type{Ramp}) = Float64
function iterate(line::Ramp, state = line.start)
    state, state + line.plus
end

"""
    line(samplerate, start_value, end_value, duration)

At a given `samplerate`, create a line that goes from `start_value` to `end_value` and lasts
for `duration` seconds. Note that the line will continue indefinitely in the same direction
once the `duration` is over. Useful to pass to [`Envelope`](@ref).

```jldoctest
julia> using AudioScheduler

julia> using Base.Iterators: take

julia> collect(take(line(4, 0, 1, 1), 5))
5-element Array{Float64,1}:
 0.0
 0.25
 0.5
 0.75
 1.0
```
"""
function line(samplerate, start_value, end_value, duration)
    Ramp(start_value, (end_value - start_value) / (samplerate * duration))
end

function schedule_note(
    scheduler,
    tone,
    samplerate,
    start_level,
    end_level,
    start_time,
    end_time,
)
    scheduler[start_time, end_time] = Generator(
        product,
        zip(tone, line(samplerate, start_level, end_level, end_time - start_time)),
    )
end

end
