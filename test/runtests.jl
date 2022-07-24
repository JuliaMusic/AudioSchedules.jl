using AudioSchedules
using Test: @test
using Documenter: doctest

doctest(AudioSchedules)

# test gaps in audio scheduels
a_schedule = AudioSchedule()
push!(a_schedule, Map(sin, Cycles(440Hz)), 0s, @envelope(
    0,
    Line => 1s,
    1,
    Line => 1s,
    0
))
push!(a_schedule, Map(sin, Cycles(440Hz)), 4s, @envelope(
    0,
    Line => 1s,
    1,
    Line => 1s,
    0
))
@test first(collect(a_schedule)[3]) ≈ 0