using EDFPlus
using Test

# Run tests
@test include("readtest.jl")
@test include("writetest.jl")
@test include("cover.jl")

# Optional cross-validation against PyEDFlib. Uses PyCall.
#
# Run with:
#
#     EDFPLUS_PYEDFLIB_TEST=1 julia --project test/runtests.jl
#
# or from Julia, after changing to the tests' package directory:
#
#     ENV["EDFPLUS_PYEDFLIB_TEST"] = "1"
#     include("test/runtests.jl")
#
# You will need to have PyCall and pyedflib installed for these tests to run.
#
if get(ENV, "EDFPLUS_PYEDFLIB_TEST", "0") == "1"
    @info "Running optional PyEDFlib cross-validation tests"
    @test include("pyedflibtest.jl")
end


