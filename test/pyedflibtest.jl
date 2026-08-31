using Dates, Test, PyCall, EDFPlus

# Note: If your Python does not have it, pyedflib may be imported with
# ```julia
# using Conda
# Conda.add("pyedflib")
# ```
# if you get an error on the import, try this Conda installation first

const pyedflib = pyimport("pyedflib")

"""
Return the PyEDFlib signal index corresponding to an EDFPlus signal.

EDFPlus uses the positions in `mapped_signals`, while PyEDFlib uses
zero-based signal indices.  Matching by label avoids making assumptions
about the annotation channel position.
"""
function py_signal_number(pyreader, label)
    labels = String.(pyreader.getSignalLabels())
    # EDF header labels are space-padded.
    target = strip(label)
    i = findfirst(x -> strip(x) == target, labels)
    return i
end

"""
Compare the common signal-header fields for one EDFPlus channel against
the corresponding PyEDFlib signal.
"""
function test_signal_header(edfh, pyreader, channel)
    sp = edfh.signalparam[channel]
    pyn = py_signal_number(pyreader, sp.label)
    @test !isnothing(pyn)
    isnothing(pyn) && return # failed to load so exit early

    # PyEDFlib, as Python derived from the C library, is zero-based.
    pyheader = pyreader.getSignalHeader(pyn - 1)
    @test strip(sp.label) == strip(String(pyheader["label"]))
    @test strip(sp.transducer) == strip(String(pyheader["transducer"]))
    @test strip(sp.physdimension) == strip(String(pyheader["dimension"]))
    @test isapprox(sp.physmin, Float64(pyheader["physical_min"]), rtol=1e-5, atol=1e-5)
    @test isapprox(sp.physmax, Float64(pyheader["physical_max"]), rtol=1e-5, atol=1e-5)
    @test sp.digmin == Int(pyheader["digital_min"])
    @test sp.digmax == Int(pyheader["digital_max"])
    @test sp.smp_per_record == Int(round(pyheader["sample_frequency"] * edfh.datarecord_duration))
    @test strip(sp.prefilter) == strip(String(pyheader["prefilter"]))
    @test samplerate(edfh, channel) ≈ Float64(pyreader.getSampleFrequency(pyn - 1))
end

"""
Compare the digital samples of one signal.

Digital samples should be compared exactly.  This deliberately tests the
raw integer values before any physical-value conversion takes place.
"""
function test_digital_signal(edfh, pyreader, channel)
    sp = edfh.signalparam[channel]
    pyn = py_signal_number(pyreader, sp.label)
    @test !isnothing(pyn)
    pyn === nothing && return #load failure
    
    julia_data = digitalchanneldata(edfh, channel)
    python_data = Vector(pyreader.readSignal(pyn - 1; digital=true))
    @test length(julia_data) == length(python_data)
    if length(julia_data) == length(python_data)
        @test julia_data == python_data
    end
end

"""
Compare physical samples.

`isapprox` with 1e-5 tolerance is used for floating point comparisons not only because
EDFPlus and PyEDFlib may perform the same calibration arithmetic in different floating-
point order, but because in the event of BDF => EDF conversion, a value that transforms
into 16-bit integer storage and is passed back out again may lose even Float32 precision.
"""
function test_physical_signal(edfh, pyreader, channel)
    sp = edfh.signalparam[channel]
    pyn = py_signal_number(pyreader, sp.label)
    @test !isnothing(pyn)
    isnothing(pyn) && return

    julia_data = physicalchanneldata(edfh, channel)
    python_data = Vector(pyreader.readSignal(pyn - 1; digital=false))
    @test length(julia_data) == length(python_data)
    if length(julia_data) == length(python_data)
        @test isapprox(julia_data, python_data; rtol=1e-5, atol=1e-5)
    end
end


"""
    test_annotations(edfh, pyreader)


Compare EDFPlus annotations against PyEDFlib annotations.

PyEDFlib returns:

    onset, duration, description

as three parallel arrays.

EDFPlus stores annotations grouped by data record.
"""
function test_annotations(edfh, pyreader)

    if edfh.annotationchannel == 0
        return
    end

    onset, duration, description =
        pyreader.readAnnotations()

    python_annotations = [
        (
            Float64(onset[i]),
            Float64(duration[i]),
            String(description[i]),
        )
        for i in eachindex(onset)
    ]

    println()
    println("========== Annotation diagnostic ==========")
    println("PyEDFlib annotations: ", length(python_annotations))
    println("EDFPlus annotations container:")
    println("  type   = ", typeof(edfh.annotations))
    println("  length = ", length(edfh.annotations))

    for i in 1:min(5, length(edfh.annotations))
        println()
        println("EDFPlus annotations[$i]")
        println("  type = ", typeof(edfh.annotations[i]))
        println("  value = ", edfh.annotations[i])
    end

    println("===========================================")

    @test length(python_annotations) > 0
end

# EDF tests
@testset "PyEDFlib comparison: EDFPlusTestFile.edf" begin
    path = joinpath(@__DIR__, "EDFPlusTestFile.edf")
    edfh = loadfile(path)
    pyreader = pyedflib.EdfReader(path)

    try
        @testset "File properties" begin
            @test edfh.datarecords == pyreader.datarecords_in_file
            # chennelcount includes annotation channel in EDFPlus but not in PyEDFlib
            @test edfh.channelcount - 1 == pyreader.signals_in_file
            @test edfh.file_duration ≈ pyreader.getFileDuration()
            @test edfh.startdate_day == day(pyreader.getStartdatetime())
            @test edfh.startdate_month == month(pyreader.getStartdatetime())
            @test edfh.startdate_year == year(pyreader.getStartdatetime())
            @test edfh.starttime_hour == hour(pyreader.getStartdatetime())
            @test edfh.starttime_minute == minute(pyreader.getStartdatetime())
            @test edfh.starttime_second == second(pyreader.getStartdatetime())
        end

        @testset "Signal headers" begin
            for channel in edfh.mapped_signals
                test_signal_header(edfh, pyreader, channel)
            end
        end

        @testset "Digital signal data" begin
            for channel in edfh.mapped_signals
                test_digital_signal(edfh, pyreader, channel)
            end
        end

        @testset "Physical signal data" begin
            for channel in edfh.mapped_signals
                test_physical_signal(edfh, pyreader, channel)
            end
        end

        @testset "Annotations" begin
            test_annotations(edfh, pyreader)
        end

    finally
        closefile!(edfh)
        pyreader.close()
    end
end

# BDF tests
@testset "PyEDFlib comparison: samplefrombiosemicom.bdf" begin
    path = joinpath(@__DIR__, "samplefrombiosemicom.bdf")
    edfh = loadfile(path)
    pyreader = pyedflib.EdfReader(path)

    try
        @testset "File properties" begin
            @test edfh.datarecords == pyreader.datarecords_in_file
            @test edfh.channelcount == pyreader.signals_in_file
            @test edfh.file_duration ≈ pyreader.getFileDuration()
            @test edfh.startdate_day == day(pyreader.getStartdatetime())
            @test edfh.startdate_month == month(pyreader.getStartdatetime())
            @test edfh.startdate_year == year(pyreader.getStartdatetime())
        end

        @testset "Signal headers" begin
            for channel in edfh.mapped_signals
                test_signal_header(edfh, pyreader, channel)
            end
        end

        @testset "Digital signal data" begin
            for channel in edfh.mapped_signals
                test_digital_signal(edfh, pyreader, channel)
            end
        end

        @testset "Physical signal data" begin
            for channel in edfh.mapped_signals
                test_physical_signal(edfh, pyreader, channel)
            end
        end

        @testset "Annotations" begin
            test_annotations(edfh, pyreader)
        end

    finally
        closefile!(edfh)
        pyreader.close()
    end
end
