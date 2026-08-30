
using EDFPlus
using Test


edfh = loadfile("EDFPlusTestFile.edf")
sz = size(edfh.EDFsignals)
@test sz == (20010, 601)
origedfdigital = copy(digitalchanneldata(edfh, 2))
# copy at baseline, before any in-place format conversion may occur below
copyedfh = deepcopy(edfh)

eann = Annotation()
@test eann.onset == 0.0
ann = Annotation(61.04, "5.25", "They said schöner")
addannotation!(copyedfh, ann.onset, ann.duration, ann.annotation)
EDFPlus.latintoascii("")
copyedfh.gender = "Male"
copyedfh.signalparam[1].label = "Signal"
@test EDFPlus.readBiosemiStatus(copyedfh) isa Dict

# read -> write -> change -> read back: verify metadata/annotation edits and signal values all survive a plain EDF round trip
newedfh = writefile!(copyedfh, "NEWedfplustestfile.edf")
@test size(newedfh.EDFsignals) == sz
@test EDFPlus.bytesperdatapoint(newedfh) == 2
@test digitalchanneldata(newedfh, 2) == origedfdigital
@test newedfh.gender == "Male"
@test trim(newedfh.signalparam[1].label) == "Signal"
@test any(a -> "They said schoner" in a.annotation, vcat(newedfh.annotations...))
closefile!(newedfh)

bdfh = loadfile("samplefrombiosemicom.bdf")
bsz = size(bdfh.BDFsignals)
@test bsz == (60, 34816)
origbdfdigital = copy(digitalchanneldata(bdfh, 1))
bdfh.signalparam[1].label = "TestLabel"  # plain BDF files have no patient/technician metadata fields
copyedbdfh = deepcopy(bdfh)

# read -> write -> change -> read back: same check for a plain BDF round trip
newbdfh = writefile!(copyedbdfh, "NEWsamplefrombiosemicom.bdf")
@test size(newbdfh.BDFsignals) == bsz
@test digitalchanneldata(newbdfh, 1) == origbdfdigital
@test trim(newbdfh.signalparam[1].label) == "TestLabel"
@test digitalchanneldata(newbdfh, 1) == origbdfdigital
#closefile!(newbdfh)
#closefile!(copyedbdfh)

# confirm digital sample values survive a full EDF -> BDF -> EDF format conversion round trip
copyedfh2 = deepcopy(edfh)
newbdfh = writefile!(copyedfh2, "NEWbdfplusfromedfplus.bdf", sigformat=EDFPlus.bdfplus)
@test size(newbdfh.BDFsignals) == sz
@test EDFPlus.bytesperdatapoint(newbdfh) == 3
@test digitalchanneldata(newbdfh, 2) == Int32.(origedfdigital)
copynewbdfh = deepcopy(newbdfh)

copynewedfh2 = writefile!(copynewbdfh, "NEWedfplusfrombdfplus.edf", sigformat=EDFPlus.edfplus)
@test size(copynewedfh2.EDFsignals) == sz
@test EDFPlus.bytesperdatapoint(copynewedfh2) == 2
@test digitalchanneldata(copynewedfh2, 2) == origedfdigital

true
