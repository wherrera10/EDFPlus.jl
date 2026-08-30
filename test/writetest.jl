using EDFPlus
using Test

cd("C:/Users/wherr/.julia/packages/EDFPlus/on1Zg/test")

edfh = loadfile("EDFPlusTestFile.edf")
sz = size(edfh.EDFsignals)
@test sz == (20010, 601)
origedfdigital = copy(digitalchanneldata(edfh, 2))
copyedfh = deepcopy(edfh)

eann = Annotation()
@test eann.onset == 0.0
ann = Annotation(61.04, "5.25", "They said schöner")
addannotation!(copyedfh, ann.onset, ann.duration, ann.annotation)
EDFPlus.latintoascii("")
copyedfh.gender = "Male"
copyedfh.signalparam[1].label = "Signal"
@test EDFPlus.readBiosemiStatus(copyedfh) isa Dict

# plain EDF round-trip
newedfh = writefile!(copyedfh, "NEWedfplustestfile.edf")
@test size(newedfh.EDFsignals) == sz
@test EDFPlus.bytesperdatapoint(newedfh) == 2
@test digitalchanneldata(newedfh, 2) == origedfdigital
@test newedfh.gender == "Male"
@test trim(newedfh.signalparam[1].label) == "Signal"
@test any(a -> "They said schoner" in a.annotation, vcat(newedfh.annotations...))
closefile!(newedfh)
closefile!(copyedfh)        

# plain BDF round-trip
bdfh = loadfile("samplefrombiosemicom.bdf")
bsz = size(bdfh.BDFsignals)
@test bsz == (60, 34816)
origbdfdigital = copy(digitalchanneldata(bdfh, 1))
bdfh.signalparam[1].label = "TestLabel"
copyedbdfh = deepcopy(bdfh)

newbdfh = writefile!(copyedbdfh, "NEWsamplefrombiosemicom.bdf")
@test size(newbdfh.BDFsignals) == bsz
@test digitalchanneldata(newbdfh, 1) == origbdfdigital
@test trim(newbdfh.signalparam[1].label) == "TestLabel"
closefile!(newbdfh)
closefile!(copyedbdfh)
closefile!(bdfh)

# EDF → BDF+ → EDF+ digital-value round-trip
copyedfh2 = deepcopy(edfh)
newbdfh = writefile!(copyedfh2, "NEWbdfplusfromedfplus.bdf", sigformat=EDFPlus.bdfplus)
@test size(newbdfh.BDFsignals) == sz
@test EDFPlus.bytesperdatapoint(newbdfh) == 3
@test digitalchanneldata(newbdfh, 2) == Int32.(origedfdigital)
copynewbdfh = deepcopy(newbdfh)
closefile!(newbdfh)         
closefile!(copyedfh2)

copynewedfh2 = writefile!(copynewbdfh, "NEWedfplusfrombdfplus.edf", sigformat=EDFPlus.edfplus)
@test size(copynewedfh2.EDFsignals) == sz
@test EDFPlus.bytesperdatapoint(copynewedfh2) == 2
@test digitalchanneldata(copynewedfh2, 2) == origedfdigital
closefile!(copynewedfh2)
closefile!(copynewbdfh)

closefile!(edfh)

true
