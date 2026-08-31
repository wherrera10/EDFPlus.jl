using EDFPlus
using Test
using Logging

# Most of the constants are from running Python pyedflib or the C code EDFBrowser libraries.
edfh = loadfile("EDFPlusTestFile.edf")
sz = size(edfh.EDFsignals)
@test sz == (20010, 601)
@test edfh.channelcount == 30
@test edfh.annotationchannel == 30
@test length(edfh.mapped_signals) == 29
origedfdigital = copy(digitalchanneldata(edfh, 2))
origedfphysical = copy(physicalchanneldata(edfh, 2))
origlabels = [trim(sp.label) for sp in edfh.signalparam]
origtransducers = [trim(sp.transducer) for sp in edfh.signalparam]
origphysdims = [trim(sp.physdimension) for sp in edfh.signalparam]
origprefilters = [trim(sp.prefilter) for sp in edfh.signalparam]
origphysmax = [sp.physmax for sp in edfh.signalparam]
origphysmin = [sp.physmin for sp in edfh.signalparam]
origdigmax = [sp.digmax for sp in edfh.signalparam]
origdigmin = [sp.digmin for sp in edfh.signalparam]
origsmp = [sp.smp_per_record for sp in edfh.signalparam]
origrate2 = samplerate(edfh, 2)
origdatarecords = edfh.datarecords
origduration = edfh.file_duration
copyedfh = deepcopy(edfh)

@test sum(Int64.(origedfdigital)) == -15361892 # from pyedflib digital checksum, channel "Fp2"
@test origedfdigital[100] == 0 # from pyedflib readSignal(1, digital=True)[99]

# Physical-value self-consistency: physicalchanneldata should always equal
# (digital + offset) * bitvalue for that channel's own calibration params.
# We use isapprox() due to a likelihood of cumulative floating-point errors.
sp2 = edfh.signalparam[2]
@test all(isapprox.(origedfphysical, (origedfdigital .+ sp2.offset) .* sp2.bitvalue, atol=1e-9))

# Annotation checking
eann = Annotation()
@test eann.onset == 0.0
@test eann.duration == ""
@test eann.annotation == []
ann = Annotation(61.04, "5.25", "They said schöner")
@test ann.onset == 61.04
@test ann.duration == "5.25"
@test ann.annotation == ["They said schöner"]     # single String gets wrapped in an array
annmulti = Annotation(1.0, "0", ["first", "second"])
@test length(annmulti.annotation) == 2

# latintoascii transliteration
@test EDFPlus.latintoascii("") == ""
@test EDFPlus.latintoascii("schöner") == "schoner"
@test EDFPlus.latintoascii("plain ascii") == "plain ascii"
@test EDFPlus.latintoascii("café") == "cafe"

addannotation!(copyedfh, ann.onset, ann.duration, ann.annotation)
copyedfh.gender = "Male"
copyedfh.patientcode = "TESTCODE1"
copyedfh.technician = "TEster"
copyedfh.signalparam[1].label = "Signal"
@test EDFPlus.readBiosemiStatus(copyedfh) isa Dict
statusdict = EDFPlus.readBiosemiStatus(copyedfh)
@test all(k -> haskey(statusdict, k), ["Code", "Index", "Onset", "Duration"])
@test length(statusdict["Code"]) == length(statusdict["Index"]) ==
      length(statusdict["Onset"]) == length(statusdict["Duration"])

# physicalchanneldata should refuse to operate on the annotation channel
@test_throws String physicalchanneldata(copyedfh, copyedfh.annotationchannel)

# plain EDF round-trip
newedfh = writefile!(copyedfh, "NEWedfplustestfile.edf")
@test size(newedfh.EDFsignals) == sz
@test EDFPlus.bytesperdatapoint(newedfh) == 2
@test newedfh.datarecords == origdatarecords
@test newedfh.file_duration == origduration
@test digitalchanneldata(newedfh, 2) == origedfdigital
@test all(isapprox.(physicalchanneldata(newedfh, 2), origedfphysical, atol=1e-6))
@test newedfh.gender == "Male"
@test trim(newedfh.patientcode) == "TESTCODE1"
@test trim(newedfh.technician) == "TEster"
@test trim(newedfh.signalparam[1].label) == "Signal"
@test samplerate(newedfh, 2) == origrate2
# every other (untouched) channel label/param should survive the round trip too
@test [trim(sp.label) for sp in newedfh.signalparam[2:end]] == origlabels[2:end]
@test [trim(sp.physdimension) for sp in newedfh.signalparam] == origphysdims
@test [trim(sp.transducer) for sp in newedfh.signalparam] == origtransducers
@test [trim(sp.prefilter) for sp in newedfh.signalparam] == origprefilters
@test all(isapprox.([sp.physmax for sp in newedfh.signalparam], origphysmax, rtol=1e-3))
@test all(isapprox.([sp.physmin for sp in newedfh.signalparam], origphysmin, rtol=1e-3))
@test [sp.digmax for sp in newedfh.signalparam] == origdigmax
@test [sp.digmin for sp in newedfh.signalparam] == origdigmin
@test [sp.smp_per_record for sp in newedfh.signalparam] == origsmp
# annotation text round-trips through latintoascii, at the onset/duration we specified
newanns = vcat(newedfh.annotations...)
@test any(a -> "They said schoner" in a.annotation, newanns)
matched = newanns[findfirst(a -> "They said schoner" in a.annotation, newanns)]
@test matched.onset == 61.04
@test matched.duration == "5.25"
@test count(a -> "They said schoner" in a.annotation, newanns) == 1

# plain BDF round-trip
bdfh = loadfile("samplefrombiosemicom.bdf")
bsz = size(bdfh.BDFsignals)
@test bsz == (60, 34816)
@test bdfh.annotationchannel == 0     # plain BDF (not BDF+) has no annotation channel
origbdfdigital = copy(digitalchanneldata(bdfh, 1))
origbdfphysical = copy(physicalchanneldata(bdfh, 1))
origbdfrate = samplerate(bdfh, 1)

# pyedflib cross-checks on the BDF fixture
@test sum(Int64.(origbdfdigital)) == -2080489952     # pyedflib digital checksum, channel "A1"
@test origbdfdigital[100] == -15520                   # pyedflib readSignal(0, digital=True)[99]
@test round(origbdfphysical[100], digits=3) == -484.984 # pyedflib readSignal(0)[99]

# a plain BDF file has no annotation channel, so adding one should throw
@test_throws String addannotation!(bdfh, 1.0, "0", "should fail, no annotation channel")

bdfh.signalparam[1].label = "TestLabel"
copyedbdfh = deepcopy(bdfh)

newbdfh = writefile!(copyedbdfh, "NEWsamplefrombiosemicom.bdf")
@test size(newbdfh.BDFsignals) == bsz
@test digitalchanneldata(newbdfh, 1) == origbdfdigital
@test all(isapprox.(physicalchanneldata(newbdfh, 1), origbdfphysical, atol=1e-6))
@test trim(newbdfh.signalparam[1].label) == "TestLabel"
@test samplerate(newbdfh, 1) == origbdfrate

# EDF → BDF+ → EDF+ digital-value round-trip
copyedfh2 = deepcopy(edfh)
newbdfh2 = writefile!(copyedfh2, "NEWbdfplusfromedfplus.bdf", sigformat=EDFPlus.bdfplus)
@test size(newbdfh2.BDFsignals) == sz
@test EDFPlus.bytesperdatapoint(newbdfh2) == 3
@test newbdfh2.datarecords == origdatarecords

# 16->24 bit widening should scale values by 2^8, see translate16to24bits! << 8
@test digitalchanneldata(newbdfh2, 2) == Int32.(origedfdigital) .* 256
# physical values survive the digital-resolution upgrade (16-bit -> 24-bit) if all is scaled correctly
@test all(isapprox.(physicalchanneldata(newbdfh2, 2), origedfphysical, atol=1e-3))
copynewbdfh = deepcopy(newbdfh2)

copynewedfh2 = writefile!(copynewbdfh, "NEWedfplusfrombdfplus.edf", sigformat=EDFPlus.edfplus)
@test size(copynewedfh2.EDFsignals) == sz
@test EDFPlus.bytesperdatapoint(copynewedfh2) == 2
@test digitalchanneldata(copynewedfh2, 2) == origedfdigital

@test all(isapprox.(physicalchanneldata(copynewedfh2, 2), origedfphysical, rtol=1e-1))
@test copynewedfh2.datarecords == origdatarecords

# Error handling bad filenames and files
@test_throws SystemError loadfile("this_file_does_not_exist_xyz.edf")

badpath = tempname() * ".edf"
open(badpath, "w") do io
    write(io, "not a real edf or bdf header at all, just garbage bytes"^5)
end
with_logger(NullLogger()) do # this keeps the warning the checker prints from showing here
    @test_throws String loadfile(badpath)
end
rm(badpath, force=true)

# closefile! releases in-memory data and marks the handle closed
# Note our deepcopy() copies the IO handle so we do not closefile! deep copies
closefile!(newedfh)
@test newedfh.filetype == EDFPlus.CLOSED
@test size(newedfh.EDFsignals) == (0, 0)

closefile!(newbdfh)
@test newbdfh.filetype == EDFPlus.CLOSED
@test size(newbdfh.BDFsignals) == (0, 0)

closefile!(bdfh)
closefile!(newbdfh2)
closefile!(copynewedfh2)
closefile!(edfh)

true
