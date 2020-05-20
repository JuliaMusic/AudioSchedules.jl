module AudioSchedulers

import Base: eltype, iterate, IteratorSize, read!, setindex!, show
using Base: Generator, IsInfinite, RefValue
using Base.Iterators: repeated
using DataStructures: OrderedDict
using PortAudio: PortAudioStream
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: Hz, s, SampleSource

const TAU = 2 * pi

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

struct Ramp
    start::Float64
    plus::Float64
end

function splat(a_function)
    let a_function = a_function
        function (samples)
            a_function(samples...)
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
            item, state = result
            buf[row, 1] = item
        end
    end
    state_box[] = state
    return rows
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

julia> iterator = Generator(sin, cycles(SAMPLE_RATE, 440));

julia> write(sink, IteratorSource(sink, iterator), SAMPLE_RATE)

julia> close(stream)
```
"""
function IteratorSource(intended_sink, iterator)
    _, state = iterate(iterator)
    state_box = Ref(state)
    IteratorSource(iterator, state_box, 1, samplerate(intended_sink))
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

struct Instrument{Iterator,State}
    iterator::Iterator
    state_box::RefValue{State}
    is_on_box::RefValue{Bool}
end

struct AudioScheduler{Sink}
    orchestra::Dict{Symbol,Instrument}
    triggers::OrderedDict{Float64,Vector{Tuple{Symbol,Bool}}}
    sink::Sink
    consumed_box::Ref{Bool}
end

"""
    Periodic(a_function, frequency)

A periodic function with period 2π to be played at a `frequency` in hertz.
"""
struct Periodic{AFunction}
    a_function::AFunction
    frequency::Float64
end

function make_iterator(generator::Periodic, samplerate)
    Generator(generator.a_function, Ring(0, generator.frequency / samplerate * TAU))
end

make_iterator(something, samplerate) = something

"""
    AudioScheduler(sink, start_time = 0.0)

Create a `AudioScheduler` to schedule changes to sink. Add an generator to the schedule with
[`schedule!`](@ref). Then, you can play it with [`play`](@ref). You can schedule based on
times in the same unit of `start_time`, or samples by setting `start_time` to an `Int`.

```jldoctest
julia> using AudioScheduler

julia> using PortAudio: PortAudioStream

julia> using Base: Generator

julia> const SAMPLE_RATE = 44100

julia> stream = PortAudioStream(samplerate = SAMPLE_RATE)

julia> scheduler = AudioScheduler(stream.sink)
AudioScheduler with triggers at ()

julia> envelope = Envelope((0.0, 0.25, 0.0), (2, 2), (line, line));

julia> schedule!(scheduler, Periodic(sin, 440.0), 0, envelope)

julia> schedule!(scheduler, Periodic(sin, 550.0), 4, envelope)

julia> schedule!(scheduler, Periodic(sin, 440.0), 4, envelope)

julia> play(scheduler) # laggy due to compilation

julia> close(stream)
```
"""
AudioScheduler(sink::Sink) where {Sink} =
    AudioScheduler{Sink}(
        Dict{Symbol,Instrument}(),
        OrderedDict{Float64,Vector{Tuple{Symbol,Bool}}}(),
        sink,
        Ref(false)
    )

"""
    schedule!(scheduler::AudioScheduler, generator, start_time, duration)

Schedule an audio generator to be added to the `scheduler`, starting at `start_time` and
lasting for `duration`. You can also pass an [`Envelope`](@ref) as a duration. See the
example for [`AudioScheduler`](@ref). Note: the scheduler will discard the first sample in
the iterator during scheduling.
"""
function schedule!(scheduler::AudioScheduler, generator, start_time, duration)
    iterator = make_iterator(generator, samplerate(scheduler.sink))
    orchestra = scheduler.orchestra
    triggers = scheduler.triggers
    label = gensym("instrument")
    stop_time = start_time + duration
    # note: will discard first sample in the iterator
    _, state = iterate(iterator)
    orchestra[label] = Instrument(iterator, Ref(state), Ref(false))
    on_trigger = (label, true)
    if haskey(triggers, start_time)
        push!(triggers[start_time], on_trigger)
    else
        triggers[start_time] = [on_trigger]
    end
    off_trigger = (label, false)
    if haskey(triggers, stop_time)
        push!(triggers[stop_time], off_trigger)
    else
        triggers[stop_time] = [off_trigger]
    end
    nothing
end

function schedule!(scheduler::AudioScheduler, generator, start_time, envelope::Envelope)
    the_samplerate = samplerate(scheduler.sink)
    iterator = make_iterator(generator, the_samplerate)
    durations = envelope.durations
    levels = envelope.levels
    shapes = envelope.shapes
    for index = 1:length(durations)
        duration = durations[index]
        schedule!(
            scheduler,
            Generator(
                splat(*),
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
    print(io, "AudioScheduler with triggers at $((keys(scheduler.triggers)...,))")

@noinline function _play(sink, start_time, end_time, ::Tuple{})
    write(sink, IteratorSource(sink, repeated(0.0)), (end_time - start_time)s)
    nothing
end

@noinline function _play(sink, start_time, end_time, instruments)
    state_boxes = map(instrument -> instrument.state_box, instruments)
    state_box = Ref(map(getindex, state_boxes))
    iterator = Generator(
        splat(+),
        zip(map(instrument -> instrument.iterator, instruments)...),
    )
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

Play a [`AudioScheduler`](@ref). Note the first time a scheduler is played will be laggy due to
compilation time; successive plays should sound better. See the example for
[`AudioScheduler`](@ref).
"""
function play(scheduler::AudioScheduler)
    consumed_box = scheduler.consumed_box
    if consumed_box[]
        error("This scheduler has already been played!")
    else
        start_time = 0.0
        triggers = scheduler.triggers
        orchestra = scheduler.orchestra
        for instrument in values(orchestra)
            # reset state
            _, state = iterate(instrument.iterator)
            instrument.state_box[] = state
        end
        for (end_time, trigger_list) in pairs(triggers)
            _play(
                scheduler.sink,
                start_time,
                end_time,
                ((instrument for instrument in values(orchestra) if instrument.is_on_box[])...,),
            )
            for (label, is_on) in trigger_list
                orchestra[label].is_on_box[] = is_on
            end
            start_time = end_time
        end
    end
    consumed_box[] = true
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

end
