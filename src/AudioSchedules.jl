module AudioSchedules

import Base: eltype, iterate, IteratorSize, read!, setindex!, show
using Base: Generator, IsInfinite, RefValue
using Base.Iterators: repeated
using DataStructures: OrderedDict
import SampledSignals: samplerate, nchannels, unsafe_read!
using SampledSignals: Hz, s, SampleSource
const TAU = 2 * pi

"""
    abstract type Synthesizer

Synthesizers need only support [`make_iterator`](@ref).
"""
abstract type Synthesizer end
export Synthesizer

struct LineIterator
    start::Float64
    plus::Float64
end

IteratorSize(::Type{LineIterator}) = IsInfinite()

eltype(::Type{LineIterator}) = Float64

@inline function iterate(line::LineIterator, state = line.start)
    state, state + line.plus
end

"""
    Line(start_value, end_value, duration)

A line from `start_value` to `end_value` that lasts for `duration`.
"""
struct Line <: Synthesizer
    start_value::Float64
    end_value::Float64
    duration::Float64
    Line(start_value, end_value, duration) = new(start_value, end_value, duration / s)
end
export Line

function make_iterator(line::Line, samplerate)
    start_value = line.start_value
    LineIterator(start_value, (line.end_value - start_value) / (samplerate * line.duration))
end

struct CyclesIterator
    start::Float64
    plus::Float64
end

IteratorSize(::Type{CyclesIterator}) = IsInfinite()

eltype(::Type{CyclesIterator}) = Float64

@inline function iterate(ring::CyclesIterator, state = ring.start) where {Element}
    next_state = state + ring.plus
    if next_state >= TAU
        next_state = next_state - TAU
    end
    state, next_state
end

"""
    Cycles(frequency)

Cycles from 0 2π to repeat at a `frequency`.
"""
struct Cycles <: Synthesizer
    frequency::Float64
    Cycles(frequency) = new(frequency / Hz)
end

export Cycles

function make_iterator(cycles::Cycles, samplerate)
    CyclesIterator(0, cycles.frequency / samplerate * TAU)
end

mutable struct IteratorSource{Iterator,State} <: SampleSource
    iterator::Iterator
    state::State
    nchannels::Int
    samplerate::Float64
end

eltype(::IteratorSource) = Float64

nchannels(source::IteratorSource) = source.nchannels

samplerate(source::IteratorSource) = source.samplerate

@inline function unsafe_read!(source::IteratorSource, buf::Matrix, frameoffset, framecount)
    _unsafe_read!(source, buf, frameoffset, framecount, IteratorSize(source))
end

@inline function _unsafe_read!(source, buf, frameoffset, framecount, _)
    iterator = source.iterator
    state = source.state
    for index in eachindex(buf)
        result = iterate(iterator, state)
        if result === nothing
            source.state = state
            return row - 1
        else
            item, state = result
            @inbounds buf[index] = item
        end
    end
    source.state = state
    return length(buf)
end

@inline function _unsafe_read!(source, buf, frameoffset, framecount, ::IsInfinite)
    iterator = source.iterator
    state = source.state
    for index in eachindex(buf)
        item, state = iterate(iterator, state)
        @inbounds buf[index] = item
    end
    source.state = state
    return length(buf)
end

function IteratorSource(intended_sink, iterator)
    _, state = iterate(iterator)
    IteratorSource(iterator, state, 1, samplerate(intended_sink))
end

"""
    Envelope(levels, durations, shapes)

Shapes are all functions which return [`Synthesizer`](@ref)s:

```
shape(start_value, end_value, duration) -> Synthesizer
```

`durations` and `levels` list the time and level of the boundaries of segments of the
envelope. For example,

```
Envelope([0.0, 1.0, 1.0, 0.0], [.05 s, 0.9 s, 0.05 s], [Line, Line, Line])
```

will create an envelope with three segments:

```
Line(0.0, 1.0, 0.05 s)
Line(1.0, 1.0, 0.9 s)
Line(s1.0, 0.0, 0.05 s)
```

See the example for [`AudioSchedule`](@ref).
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
export Envelope

mutable struct Instrument{Iterator,State}
    iterator::Iterator
    state::State
    is_on::Bool
end

mutable struct AudioSchedule{Sink}
    orchestra::Dict{Symbol,Instrument}
    triggers::OrderedDict{Float64,Vector{Tuple{Symbol,Bool}}}
    sink::Sink
    consumed::Bool
end

"""
    Map(a_function, synthesizers)

Map `a_function` over audio `synthesizers`.
"""
struct Map{AFunction,Synthesizers}
    a_function::AFunction
    synthesizers::Synthesizers
    Map(a_function::AFunction, synthesizers...) where {AFunction} =
        new{AFunction,typeof(synthesizers)}(a_function, synthesizers)
end
export Map

function make_iterator(a_map::Map, samplerate)
    Generator(
        let a_function = a_map.a_function
            @inline function (samples)
                a_function(samples...)
            end
        end,
        zip(map(
            (
                let samplerate = samplerate
                    synthesizer -> make_iterator(synthesizer, samplerate)
                end
            ),
            a_map.synthesizers,
        )...),
    )
end

function make_iterator(a_map::Map{<:Any,Tuple{<:Any}}, samplerate)
    Generator(a_map.a_function, make_iterator(a_map.synthesizers[1], samplerate))
end

"""
    make_iterator(synthesizer, samplerate)

Return an iterator that will the `synthesizer` at a given `samplerate`
"""
make_iterator(synthesizer, samplerate) = synthesizer
export make_iterator

"""
    AudioSchedule(sink)

Create a `AudioSchedule` to schedule changes to sink.

```jldoctest schedule
julia> using AudioSchedules

julia> using Unitful: s, Hz

julia> using PortAudio: PortAudioStream

julia> stream = PortAudioStream(samplerate = 44100);

julia> schedule = AudioSchedule(stream.sink)
AudioSchedule with triggers at () seconds
```

Add a synthesizer to the schedule with [`schedule!`](@ref). You can schedule for a duration
in seconds, or use an [`Envelope`](@ref).

```jldoctest schedule
julia> envelope = Envelope((0, 0.25, 0), (0.05s, 0.95s), (Line, Line));

julia> schedule!(schedule, Map(sin, Cycles(440Hz)), 0s, envelope)

julia> schedule!(schedule, Map(sin, Cycles(440Hz)), 1s, envelope)

julia> schedule!(schedule, Map(sin, Cycles(550Hz)), 1s, envelope)

julia> schedule
AudioSchedule with triggers at (0.0, 0.05, 1.0, 1.05, 2.0) seconds
```

Then, you can play it with [`play`](@ref).

```jldoctest schedule
julia> play(schedule)
```

You can only play a schedule once.

```jldoctest schedule
julia> play(schedule)
ERROR: EOFError: read end of file
[...]

julia> close(stream)
```
"""
AudioSchedule(sink::Sink) where {Sink} = AudioSchedule{Sink}(
    Dict{Symbol,Instrument}(),
    OrderedDict{Float64,Vector{Tuple{Symbol,Bool}}}(),
    sink,
    false,
)
export AudioSchedule

"""
    schedule!(schedule::AudioSchedule, synthesizer, start_time, duration)

Schedule an audio synthesizer to be added to the `schedule`, starting at `start_time` and
lasting for `duration`. You can also pass an [`Envelope`](@ref) as a duration. See the
example for [`AudioSchedule`](@ref). Note: the schedule will discard the first sample in
the iterator during scheduling.
"""
function schedule!(schedule::AudioSchedule, synthesizer, start_time, duration)
    start_time_unitless = start_time / s
    iterator = make_iterator(synthesizer, samplerate(schedule.sink))
    triggers = schedule.triggers
    label = gensym("instrument")
    stop_time = start_time_unitless + duration / s
    # TODO: don't discard first sample
    _, state = iterate(iterator)
    schedule.orchestra[label] = Instrument(iterator, state, false)
    on_trigger = (label, true)
    if haskey(triggers, start_time_unitless)
        push!(triggers[start_time_unitless], on_trigger)
    else
        triggers[start_time_unitless] = [on_trigger]
    end
    off_trigger = (label, false)
    if haskey(triggers, stop_time)
        push!(triggers[stop_time], off_trigger)
    else
        triggers[stop_time] = [off_trigger]
    end
    nothing
end

function schedule!(schedule::AudioSchedule, synthesizer, start_time, envelope::Envelope)
    the_samplerate = samplerate(schedule.sink)
    durations = envelope.durations
    levels = envelope.levels
    shapes = envelope.shapes
    for index = 1:length(durations)
        duration = durations[index]
        schedule!(
            schedule,
            Map(*, synthesizer, shapes[index](levels[index], levels[index+1], duration)),
            start_time,
            duration,
        )
        start_time = start_time + duration
    end
end
export schedule!

show(io::IO, schedule::AudioSchedule) =
    print(io, "AudioSchedule with triggers at $((keys(schedule.triggers)...,)) seconds")

@noinline function _play(sink, start_time, end_time, ::Tuple{})
    write(sink, IteratorSource(sink, repeated(0.0)), (end_time - start_time)s)
    nothing
end

@noinline function _play(sink, start_time, end_time, instruments)
    states = map(instrument -> instrument.state, instruments)
    source = IteratorSource(
        make_iterator(
            Map(+, map(instrument -> instrument.iterator, instruments)...),
            samplerate(sink),
        ),
        states,
        1,
        samplerate(sink),
    )
    write(sink, source, (end_time - start_time)s)
    map((instrument, state) -> instrument.state = state, instruments, source.state)
    nothing
end

"""
    play(schedule::AudioSchedule)

Play an [`AudioSchedule`](@ref). See the example for [`AudioSchedule`](@ref).
"""
function play(schedule::AudioSchedule)
    if schedule.consumed
        throw(EOFError())
    else
        start_time = 0.0
        triggers = schedule.triggers
        orchestra = schedule.orchestra
        for (end_time, trigger_list) in pairs(triggers)
            _play(
                schedule.sink,
                start_time,
                end_time,
                ((instrument for instrument in values(orchestra) if instrument.is_on)...,),
            )
            for (label, is_on) in trigger_list
                orchestra[label].is_on = is_on
            end
            start_time = end_time
        end
        schedule.consumed = true
    end
    return nothing
end
export play

end
