using EDFPlus
using Base.Test


edfh = loadfile("EDFPlusTestFile.edf")
p4 = findfirst(chan -> match(r"^P4", chan.label) != nothing, edfh.signalparam)
fp2 = findfirst(chan -> match(r"^Fp2", chan.label) != nothing, edfh.signalparam)
dpsp4  = Int(round(edfh.signalparam[p4].smp_per_record / edfh.datarecord_duration))
dpsfp2 = Int(round(edfh.signalparam[fp2].smp_per_record / edfh.datarecord_duration))
sig = physicalchanneldata(edfh, p4) .- digitalchanneldata(edfh, fp2)
t1 = 60.0
t2 = 75.0
sample = sig[Int(t1*dpsp4):Int(t2*dpsp4)]
timepoints = linspace(t1, t2, length(sample))

@test p4 == 8
@test fp2 == 2
@test dpsp4 == 200
@test dpsfp2 == 200
@test length(sample) == 3001
@test round(maximum(sample), 3) == 920.746
@test round(minimum(sample), 3) == -605.910

sample = notchfilter(sample, samplerate(edfh, p4))
@test length(sample) == 3001
@test round(maximum(sample), 3) == 921.389

return true

