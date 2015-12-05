"""
Information about the EDF or EDF+ file, taken from the file
"""
type EDF_Info
   cedf
   file_name
   signal_labels
   signal_nsamples
   sample_freqs
   signals_in_file
   datarecords_in_file
   
   function EDF_Info(filename)
        cedf = _edflib.CyEdfReader(file_name)
        file_name = filename
        signal_labels = []
        signal_nsamples = []
        samplefreqs = []
        signals_in_file = cedf.signals_in_file
        datarecords_in_file = cedf.datarecords_in_file
        for idx in 1:signals_in_file
            signal_labels.append!(cedf.signal_label(idx))
            signal_nsamples.append!(cedf.samples_in_file(idx))
            samplefreqs.append!(cedf.samplefrequency(idx))
        end
   end
end

function print_file_info(EDF_Info edfi)
    println("file name:", edfi.file_name)
    println("signals in file:", edfi.signals_in_file)
end

function print_long_file_info(EDF_Info edfi)
    file_info(edfi)
    for idx in 1:edfi.signals_in_file
        println("label:", edfi.signal_labels[idx], "fs:", edfi.samplefreqs[idx], "nsamples", edfi.signal_nsamples[idx])
    end
end

function getSamples(EDF_Info edfi)
    [ edfi.samples_in_file(channel) for channel in 1:edfi.signals_in_file ]
end

function getAnnotations(EDF_Info edfi)
    annot = edfi.read_annotation()
    deepcopy(annot)
end
	
# C library is 0 based, may need to fixup	
function getSignalFreqs(EDF_Info edfi)
    [ edfi.samplefrequency(channel) for channel in 1:edfi.signals_in_file ]
end

function getSignalTextLabels(EDF_Info edfi)
    [ edfi.signal_label(channel).strip() for channel in 1:edfi.signals_in_file ]
end
    
function readSignal(EDFInfo edfi, channel::Int)
    nsamples = self.getNSamples()
    if channel < length(nsamples)
        x = zeros(Float64, nsamples[channel])
        v = x[channel*nsamples[channel]:(channel+1)*nsamples[channel]]
        edfi.readsignal(channel, 0, nsamples[channel],v)
        return x
    else
        return Null
    end
end
