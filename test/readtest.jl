using EDFPlus
using Base.Test


edfh = loadfile("EDFPlusTestFile.edf")

@test edfh.gender == "Female"
@test edfh.annotationchannel == 30
@ test edfh.filetype == FileStatus(1)

fs = samplerate(edfh,1)
f1 = EDFPlus.recordslice(edfh, 21, 22)[1,:]
f2 = highpassfilter(reshape(f1, length(f1)), fs)
f3 = lowpassfilter(f2, fs)
f4 = notchfilter(f3, fs)
@test round(f4[end-3],2) == 0.01

eegpages = epoch_iterator(edfh, 12, channels=[7,8,9])
annots = annotation_epoch_iterator(edfh, 12)

for (pgnum, page) in enumerate(eegpages)
    if pgnum == 1
        @test round(page[3][end], 3) == -24.121
    elseif pgnum == 30
        @test round(page[1][100], 3) ==  -10.84
    end
end

for (i, ann) in enumerate(annots)
    if i == 9
        @test ann[1][1].onset == 96.0
    end
end

closefile(edfh)
@test edfh.filetype == FileStatus(6)

true
