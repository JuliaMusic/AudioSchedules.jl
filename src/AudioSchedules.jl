module AudioSchedules

import Base:
    close,
    getindex,
    iterate,
    IteratorEltype,
    IteratorSize,
    push!,
    show,
    size,
    wait,
    write
using Base: EltypeUnknown, SizeUnknown
using Base.FastMath: sin_fast
using Base.Meta: ParseError
using Base.Threads: nthreads
using DataStructures: SortedSet
using InfiniteArrays: Fill, ∞
using Interpolations: CubicSplineInterpolation
using PortAudio: Messenger, PortAudioStream, Scribe, write_buffer
import PortAudio: get_input_type, get_output_type
using RegularExpressions: capture, CONSTANTS, of, pattern, raw, short
import SampledSignals: nchannels, SampleBuf, samplerate
using Unitful: dB, Hz, μPa, s

const FREQUENCY = typeof(1.0Hz)
const RATE = typeof(1.0 / s)
const TIME = typeof(0.0s)

include("miscellaneous.jl")
include("series.jl")

export AudioSchedule

struct AudioSchedule
    sample_rate::FREQUENCY
    # instruments and the time they are first scheduled
    orchestra::Vector{Tuple{Any, TIME}}
    # which instruments in the orchestra are currently active
    activated::Vector{Bool}
    # times to turn instruments on and off
    triggers::SortedSet{Tuple{TIME, Int}}
end

export AudioSchedule

"""
    AudioSchedule(; sample_rate = 44100Hz)

Create an empty `AudioSchedule`.
You can [`push!`](@ref) new synthesizers to the a_schedule.
Specify a `sample_rate` with units per time, like `1/s` or `Hz`.

```jldoctest audio_schedule
julia> using AudioSchedules


julia> using Unitful: s, Hz


julia> a_schedule = AudioSchedule()
0.0 s 44100.0 Hz AudioSchedule

julia> push!(
           a_schedule,
           Map(sin, Cycles(440Hz)),
           0s,
           0,
           Line => 1s,
           0.05,
           Line => 1s,
           0.05,
           Line => 1s,
           0,
       )

julia> push!(
            a_schedule,
            Map(sin, Cycles(660Hz)),
            2s,
            0,
            Line => 1s,
            0.05,
            Line => 1s,
            0.05,
            Line => 1s,
            0,
        )


julia> a_schedule
5.0 s 44100.0 Hz AudioSchedule
```

You can use write the a_schedule directly to a `PortAudioStream`.
However, to do so, the `PortAudioStream` must have a [`Weaver`](@ref) writer.
The `PortAudioStream` must have exactly 0 input channels, 1 output channel, and a matching
sample rate.

```jldoctest audio_schedule
julia> using PortAudio: PortAudioStream


julia> PortAudioStream(0, 1, writer = Weaver(), warn_xruns = false) do stream
           write(stream, a_schedule)
       end

```

You can save an `AudioSchedule` as a `SampledSignals.SampleBuf`.

```jldoctest audio_schedule
julia> using SampledSignals: SampleBuf


julia> saved = SampleBuf(a_schedule)
220500-frame, 1-channel SampleBuf{Float64, 2}
5.0s sampled at 44100.0Hz
▂▂▃▃▃▃▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▃▃▃▃▂▂

julia> PortAudioStream(0, 1, warn_xruns = false) do stream
           write(stream, saved)
       end
220500
```
"""
function AudioSchedule(; sample_rate = 44100Hz)
    AudioSchedule(
        sample_rate,
        Vector{Tuple{Any, TIME}}(),
        Vector{Bool}(),
        SortedSet{Tuple{TIME, Int}}()
    )
end

# could potentially calculate this ahead of time
function IteratorSize(::Type{AudioSchedule})
    SizeUnknown()
end
# instruments could be any type
function IteratorEltype(::Type{AudioSchedule})
    EltypeUnknown()
end

function samplerate(a_schedule::AudioSchedule)
    a_schedule.sample_rate / Hz
end
function nchannels(a_schedule::AudioSchedule)
    1
end

function duration(a_schedule::AudioSchedule)
    triggers = a_schedule.triggers
    if length(triggers) > 0
        # time from first instrument turned on to last turned off
        last(triggers)[1] - first(triggers)[1]
    else
        # zero for an empty schedule
        0.0s
    end
end

function show(io::IO, a_schedule::AudioSchedule)
    print(io, "$(duration(a_schedule)) $(a_schedule.sample_rate) AudioSchedule")
end

function make_envelope(start_level, (shape, duration), end_level, more_segments...)
    segments(shape, start_level, duration, end_level)...,
    make_envelope(end_level, more_segments...)...
end
function make_envelope(_)
    ()
end

function add_series!(a_schedule::AudioSchedule, series, start_time, duration)
    triggers = a_schedule.triggers
    orchestra = a_schedule.orchestra
    push!(orchestra, (series, start_time))
    push!(a_schedule.activated, false)
    instrument_index = length(orchestra)
    push!(triggers, (start_time, instrument_index))
    push!(triggers, (start_time + duration, instrument_index))
    nothing
end

function second(something)
    something[2]
end

# remove last thing from a tuple
function all_but_last(thing1, thing2, rest...)
    thing1, all_but_last(thing2, rest...)...
end

function all_but_last(_)
    ()
end

function triple(wave, shape, start_time, skip_time, duration)
    Map(
        *,
        Skip(wave, skip_time),
        shape,
    ),
    start_time + skip_time,
    duration
end

function triples(wave, start_time, envelope...)
    shapes_durations = make_envelope(envelope...)
    map(
        let start_time = start_time, wave = wave
            function ((shape, duration), skip_time)
                triple(wave, shape, start_time, skip_time, duration)
            end
        end,
        shapes_durations,
        # cumulative sum of durations, starting at 0
        # then ignore the last one
        all_but_last(cumsum((0.0s, map(second, shapes_durations)...))...),
    )
end

"""
    push!(a_schedule::AudioSchedule, synthesizer, start_time,
        start_level, shape => duration, end_level, more_segments...
    )

Add a synthesizer to a [`AudioSchedule`](@ref), where `synthesizer` is anything that supports [`make_series`](@ref),
`start_time` has units of time (like `s`), and the rest of the arguments specify the shape of the envelope.

For all envelope [`segments`](@ref), call

```
segments(shape, start_level, duration, end_level)
```

`duration` should have units of time (like `s`). For example,

```
push!(a_schedule, synthesizer, start_time, 0, Line => 1s, 1, Line => 1s, 0)
```

will call segments twice:

```
segments(Line, 0, 1s, 1)
segments(Line, 1, 1s, 0)
```
"""
function push!(a_schedule::AudioSchedule, synthesizer, start_time, piece_1, rest...)
    map(
        let a_schedule = a_schedule
            function ((shaped, time, duration),)
                add_series!(a_schedule, shaped, time, duration)
            end
        end,
        triples(synthesizer, start_time, piece_1, rest...),
    )
    nothing
end

export push!

function iterate(a_schedule::AudioSchedule)
    iteration = iterate(a_schedule.triggers)
    # Done if no more triggers
    if iteration === nothing
        nothing
    else
        # Start at 0s
        trigger_iterate(a_schedule, 0.0s, iteration...)
    end
end

function iterate(a_schedule::AudioSchedule, (schedule_time, trigger_state))
    iteration = iterate(a_schedule.triggers, trigger_state)
    if iteration === nothing
        nothing
    else
        trigger_iterate(a_schedule, schedule_time, iteration...)
    end
end

function trigger_iterate(
    a_schedule,
    schedule_time,
    (trigger_time, instrument_index),
    trigger_state,
)
    sample_rate = a_schedule.sample_rate
    activated = a_schedule.activated
    triggers = a_schedule.triggers
    # pull the rest of the triggers at the current time
    while schedule_time == trigger_time
        activated[instrument_index] = !activated[instrument_index]
        iteration = iterate(triggers, trigger_state)
        if iteration === nothing
            return nothing
        end
        (trigger_time, instrument_index), trigger_state = iteration
    end
    active = view(a_schedule.orchestra, activated)
    combined = (
        if isempty(active)
            Fill(0.0, ∞)
        else
            make_series(Map(
                +,
                Iterators.map(
                    let schedule_time = schedule_time
                        function ((instrument, instrument_start_time),)
                            Skip(instrument, schedule_time - instrument_start_time)
                        end
                    end,
                    active
                )...
            ), sample_rate)
        end
    )[1:round(Int, (trigger_time - schedule_time) * sample_rate)]
    # pull the next trigger
    activated[instrument_index] = !activated[instrument_index]
    (combined, (trigger_time, trigger_state))
end

function trigger_iterate(_, __, ::Nothing)
    nothing
end

# TODO: make a SampledSignal without sacrificing performance
# TODO: figure out a way to avoid peaking !!!

"""
    struct Weaver <: Scribe end

A special `PortAudio` `Scribe`.
You can use a `Weaver` to write an [`AudioSchedule`](@ref) directly to your speakers.
"""
struct Weaver <: Scribe end

get_input_type(::Weaver, _) = Nothing
get_output_type(::Weaver, _) = Nothing

export Weaver

struct DividedRange <: AbstractVector{UnitRange{Int}}
    previous::Int
    share::Float64
    divisions::Int
end

function size(divided_range::DividedRange)
    (divided_range.divisions,)
end

function getindex(divided_range::DividedRange, index)
    share = divided_range.share
    divided_range.previous .+
    ((round(Int, (index - 1) * share)+1):round(Int, index * share))
end

function add_to(divided_range::DividedRange, increment)
    DividedRange(
        divided_range.previous + increment,
        divided_range.share,
        divided_range.divisions,
    )
end

function DividedRange(a_range::UnitRange, divisions)
    DividedRange(first(a_range) - 1, length(a_range) / divisions, divisions)
end

struct DualSubRanges <: AbstractVector{Tuple{UnitRange{Int},UnitRange{Int}}}
    frames_per_buffer::Int
    the_length::Int
    left_in_buffer::Int
    first_item::Tuple{DividedRange,DividedRange,Bool}
    last_item::Tuple{DividedRange,DividedRange,Bool}
    full_buffer_ranges::DividedRange
end

function DualSubRanges(
    first_buffer_at,
    frames_per_buffer,
    series_total,
    number_of_subsections,
)
    left_in_buffer = frames_per_buffer - first_buffer_at
    middle_buffers, last_buffer_at =
        fldmod(series_total - left_in_buffer, frames_per_buffer)
    is_last_complete = last_buffer_at == 0
    if is_last_complete
        middle_buffers = middle_buffers - 1
        last_buffer_at = frames_per_buffer
    end
    if series_total <= left_in_buffer
        first_series_sub_ranges = DividedRange(1:series_total, number_of_subsections)
        is_first_complete = is_last_complete
    else
        first_series_sub_ranges = DividedRange(1:left_in_buffer, number_of_subsections)
        is_first_complete = true
    end
    last_buffer_sub_ranges = DividedRange(1:last_buffer_at, number_of_subsections)
    DualSubRanges(
        frames_per_buffer,
        middle_buffers + 2,
        left_in_buffer,
        (
            first_series_sub_ranges,
            add_to(first_series_sub_ranges, first_buffer_at),
            is_first_complete,
        ),
        (
            add_to(
                last_buffer_sub_ranges,
                left_in_buffer + middle_buffers * frames_per_buffer,
            ),
            last_buffer_sub_ranges,
            is_last_complete,
        ),
        DividedRange(1:frames_per_buffer, number_of_subsections),
    )
end

function size(dual_sub_ranges::DualSubRanges)
    (dual_sub_ranges.the_length,)
end

function getindex(dual_sub_ranges::DualSubRanges, index::Int)
    the_length = dual_sub_ranges.the_length
    @boundscheck if index < 0 || index > the_length
        throw(BoundsError(dual_sub_ranges, index))
    end
    if index == 1
        dual_sub_ranges.first_item
    elseif index == the_length
        dual_sub_ranges.last_item
    else
        full_buffer_ranges = dual_sub_ranges.full_buffer_ranges
        (
            add_to(
                full_buffer_ranges,
                dual_sub_ranges.left_in_buffer +
                (index - 2) * dual_sub_ranges.frames_per_buffer,
            ),
            full_buffer_ranges,
            true,
        )
    end
end

struct TaskIO
    task::Task
    series_buffers_channel::Channel{Tuple{Any,Int}}
    sub_ranges_channel::Channel{Tuple{UnitRange{Int},UnitRange{Int},Bool}}
    output_channel::Channel{Nothing}
end

function close(task_io::TaskIO)
    close(task_io.series_buffers_channel)
    close(task_io.sub_ranges_channel)
    wait(task_io.task)
    close(task_io.output_channel)
end

function fill_buffer_signal!(
    buffer,
    series,
    sub_ranges_channel,
    output_channel,
)
    (series_sub_range, buffer_sub_range, is_complete) = take!(sub_ranges_channel)
    buffer.data[buffer_sub_range] .= view(series, series_sub_range)
    if is_complete
        put!(output_channel, nothing)
    end
end

function fill_series_signal!(buffer, (series, buffers), sub_ranges_channel, output_channel)
    foreach(
        let buffer = buffer, series = series, sub_ranges_channel = sub_ranges_channel, output_channel = output_channel
            function (_)
                fill_buffer_signal!(
                    buffer,
                    series,
                    sub_ranges_channel,
                    output_channel,
                )
            end
        end,
        1:buffers,
    )
end

function fill_all_signal!(
    buffer,
    series_buffers_channel,
    sub_ranges_channel,
    output_channel,
)
    foreach(
        let buffer = buffer,
            sub_ranges_channel = sub_ranges_channel,
            output_channel = output_channel

            function (series_buffers)
                fill_series_signal!(
                    buffer,
                    series_buffers,
                    sub_ranges_channel,
                    output_channel,
                )
            end
        end,
        series_buffers_channel,
    )
end

function fill_all_task_io(buffer)
    series_buffers_channel = Channel{Tuple{Any,Int}}(Inf)
    sub_ranges_channel = Channel{Tuple{UnitRange{Int},UnitRange{Int},Bool}}(0)
    output_channel = Channel{Nothing}(0)
    task = Task(
        let buffer = buffer,
            series_buffers_channel = series_buffers_channel,
            sub_ranges_channel = sub_ranges_channel,
            output_channel = output_channel

            function ()
                fill_all_signal!(
                    buffer,
                    series_buffers_channel,
                    sub_ranges_channel,
                    output_channel,
                )
            end
        end,
    )
    task.sticky = false
    schedule(task)
    TaskIO(task, series_buffers_channel, sub_ranges_channel, output_channel)
end

function fill_all_task_ios(buffer; number_of_tasks = nthreads() - 1)
    map(let buffer = buffer
        function (_)
            fill_all_task_io(buffer)
        end
    end, 1:number_of_tasks)
end

function feed_write!(task_ios, buffer, (series_sub_ranges, buffer_sub_ranges, is_complete))
    foreach(
        let is_complete = is_complete
            function ((task_io, series_sub_range, output_sub_range),)
                put!(
                    task_io.sub_ranges_channel,
                    (series_sub_range, output_sub_range, is_complete),
                )
            end
        end,
        zip(task_ios, series_sub_ranges, buffer_sub_ranges),
    )
    if is_complete
        foreach(function (task_io)
            take!(task_io.output_channel)
        end, task_ios)
        write_buffer(buffer)
    end
end

function write_series!(task_ios, series, buffer, buffer_at)
    series_total = length(series)
    dual_sub_ranges =
        DualSubRanges(buffer_at, buffer.frames_per_buffer, series_total, length(task_ios))
    foreach(let series_buffers = (series, length(dual_sub_ranges))
        function (task_io)
            put!(task_io.series_buffers_channel, series_buffers)
        end
    end, task_ios)
    foreach(let task_ios = task_ios, buffer = buffer
        function (ranges)
            feed_write!(task_ios, buffer, ranges)
        end
    end, dual_sub_ranges)
    last(last(last(dual_sub_ranges)[2]))
end

function write(
    stream::PortAudioStream{<:Messenger{<:Any,Weaver},<:Any},
    a_schedule::AudioSchedule;
    number_of_tasks = nthreads() - 1,
)
    if number_of_tasks == 0
        throw(ArgumentError("Must write with at least 1 task"))
    end
    if nchannels(stream.source_messenger) != 0
        throw(ArgumentError("$stream does not have 0 input channels"))
    end
    sink_messenger = stream.sink_messenger
    if nchannels(sink_messenger) != 1
        throw(ArgumentError("$stream does not have 1 output channel"))
    end
    if samplerate(stream) != samplerate(a_schedule)
        throw(ArgumentError("Sample rates of $stream and $a_schedule do not match"))
    end
    buffer = sink_messenger.buffer
    buffer_at = 0
    # TODO: Move task_ios inside of the Weaver object and reuse them
    task_ios = fill_all_task_ios(buffer; number_of_tasks = number_of_tasks)
    try
        for series in collect(a_schedule)
            buffer_at = write_series!(task_ios, series, buffer, buffer_at)
        end
    finally
        foreach(close, task_ios)
    end
    # This might make the previous TODO difficult. Do we need to pass-back a shorter-than-expected result?
    if buffer_at > 0
        write_buffer(buffer, buffer_at)
    end
    nothing
end

struct TaskInput{Input}
    task::Task
    input_channel::Channel{Input}
end

function close(task_input::TaskInput)
    close(task_input.input_channel)
    wait(task_input.task)
end

function fill_output!(output_data, (series, series_sub_range, output_range))
    output_data[output_range] .= view(series, series_sub_range)
end

function fill_all!(input_channel, output_data)
    foreach(let output_data = output_data
        function (series_ranges)
            fill_output!(output_data, series_ranges)
        end
    end, input_channel)
end

function fill_all_task_input(output_data)
    input_channel = Channel{Tuple{Any,UnitRange{Int},UnitRange{Int}}}(Inf)
    task = Task(let input_channel = input_channel, output_data = output_data
        function ()
            fill_all!(input_channel, output_data)
        end
    end)
    task.sticky = false
    schedule(task)
    TaskInput(task, input_channel)
end

function feed!(task_inputs, series, output_at)
    series_total = length(series)
    number_of_tasks = length(task_inputs)
    series_ranges = DividedRange(1:series_total, number_of_tasks)
    foreach(
        let series = series
            function feed_task_input!((task_input, series_range, output_range))
                put!(task_input.input_channel, (series, series_range, output_range))
            end
        end,
        zip(task_inputs, series_ranges, add_to(series_ranges, output_at)),
    )
    output_at + series_total
end

function SampleBuf(a_schedule::AudioSchedule; number_of_tasks = nthreads() - 1)
    if number_of_tasks == 0
        throw(ArgumentError("Must write with at least 1 task"))
    end
    number = sum(Iterators.map(length, a_schedule))
    output = SampleBuf(Float64, samplerate(a_schedule), number, 1)
    task_inputs = map(let output_data = output.data
        function (_)
            fill_all_task_input(output_data)
        end
    end, 1:number_of_tasks)
    try
        output_at = 0
        for series in a_schedule
            output_at = feed!(task_inputs, series, output_at)
        end
    finally
        foreach(close, task_inputs)
    end
    output
end

include("equal_loudness.jl")

end
