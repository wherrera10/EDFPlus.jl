using EDFPlus
using Test


edfh = loadfile("EDFPlusTestFile.edf")

EDFPlus.version()
epoch_iterator(edfh, 1)
annotation_epoch_iterator(edfh, 1)
channeltimesegment(edfh, 1, 0.5, 1.5, true)
multichanneltimesegment(edfh, [1], 0.25, 1.25, false)
sg = physicalchanneldata(edfh, 2)
fs = samplerate(edfh, 2)
notchfilter(sg, fs)
highpassfilter(sg, fs)
highpassfilter(sg, fs)
lowpassfilter(sg, fs)
