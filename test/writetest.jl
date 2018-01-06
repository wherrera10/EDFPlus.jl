using EDFPlus
using Base.Test


edfh = loadfile("EDFPlusTestFile.edf")
sz = size(edfh.EDFsignals)
@test sz == (20010,601)

ann = Annotation(61.04, "", "NotedThis")
addannotation(edfh, ann.onset, ann.duration, ann.annotation)


newedfh = writefile(edfh, "NEWedfplustestfile.edf")
@test size(newedfh.EDFsignals) == sz


bdfh = loadfile("samplefrombiosemicom.bdf")
bsz = size(bdfh.BDFsignals)
@test bsz == (60, 34816)

newbdfh = writefile(bdfh, "NEWsamplefrombiosemicom.edf")
@test size(newbdfh.BDFsignals) == bsz


true
