using EDFPlus
using Base.Test


edfh = loadfile("EDFPlusTestFile.edf")
sz = size(edfh.EDFsignals)
@test sz == (20010,601)

newedfh = writefile(edfh, "NEWedfplustestfile.edf")
@test size(newedfh.EDFsignals) == sz

true
