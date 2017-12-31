using EDFPlus
using Base.Test


edfh = loadfile("EDFPlusTestFile.edf")

gen = edfh.gender
achan = edfh.annotationchannel
typ = edfh.filetype

closefile(edfh)

@test gen == "Female"
@test achan == 30
@test typ == FileStatus(1)

true
