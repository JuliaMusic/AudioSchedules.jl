using AudioSchedulers
using Documenter: deploydocs, makedocs

makedocs(
    sitename = "AudioSchedulers.jl",
    modules = [AudioSchedulers],
    doctest = false,
)
deploydocs(repo = "github.com/bramtayl/AudioSchedulers.jl.git")
