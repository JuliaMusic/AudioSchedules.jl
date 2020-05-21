using AudioSchedules
using Documenter: deploydocs, makedocs

makedocs(
    sitename = "AudioSchedules.jl",
    modules = [AudioSchedules],
    doctest = false,
)
deploydocs(repo = "github.com/bramtayl/AudioSchedules.jl.git")
