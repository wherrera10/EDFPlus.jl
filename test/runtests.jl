using EDFPlus
using Base.Test
# Run tests

tic()
@time @test include("readtest.jl")
@time @test include("writetest.jl")
toc()
