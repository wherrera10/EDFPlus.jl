

# From the C library structs

type edf_param_struct            # this structure contains all the relevant EDF-signal parameters of one signal
  label::Ptr{Cchar}[17]          # label (name) of the signal, null-terminated string
  smp_in_file::Clonglong         # number of samples of this signal in the file
  phys_max::Cdouble              # physical maximum, usually the maximum input of the ADC
  double phys_min;               # physical minimum, usually the minimum input of the ADC
  dig_max::Cint                  # digital maximum, usually the maximum output of the ADC, can not not be higher than 32767 for EDF or 8388607 for BDF
  dig_min::Cint                  # digital minimum, usually the minimum output of the ADC, can not not be lower than -32768 for EDF or -8388608 for BDF
  smp_in_datarecord::Cint        # number of samples of this signal in a datarecord
  physdimension::Ptr{Cchar}[9]   # physical dimension (uV, bpm, mA, etc.), null-terminated string
  prefilter::Ptr{Cchar}[81]      # null-terminated string
  transducer::Ptr{Cchar}[81]     # null-terminated string
end

type edf_annotation_struct       # this structure is used for annotations 
    onset::Clonglong             # onset time of the event, expressed in units of 100 nanoSeconds and relative to the starttime in the header 
    duration::Ptr{Cchar}[16]     # duration time, this is a null-terminated ASCII text-string 
    annotation::Ptr{Cchar}[EDFLIB_MAX_ANNOTATION_LEN + 1] 
                                 # description of the event in UTF-8, this is a null terminated string
end

type edf_hdr_struct                # this structure contains all the relevant EDF header info and will be filled when calling the function edf_open_file_readonly()
    handle::Cint                   # a handle (identifier) used to distinguish the different files 
    filetype::Cint                 # 0: EDF, 1: EDFplus, 2: BDF, 3: BDFplus, a negative number means an error 
    edfsignals::Cint               # number of EDF signals in the file, annotation channels are NOT included 
    file_duration::Clonglong       # duration of the file expressed in units of 100 nanoSeconds 
    startdate_day::Cint
    startdate_month::Cint
    startdate_year::Cint
    starttime_subsecond::Clonglong # starttime offset expressed in units of 100 nanoSeconds. Is always less than 10000000 (one second). Only used by EDFplus and BDFplus
    starttime_second::Cint
    starttime_minute::Cint
    starttime_hour::Cint
    patient::Ptr{Cchar}[81]                  # null-terminated string, contains patientfield of header, is always empty when filetype is EDFPLUS or BDFPLUS 
    recording::Ptr{Cchar}[81]                # null-terminated string, contains recordingfield of header, is always empty when filetype is EDFPLUS or BDFPLUS
    patientcode::Ptr{Cchar}[81]              # null-terminated string, is always empty when filetype is EDF or BDF 
    gender::Ptr{Cchar}[16]                   # null-terminated string, is always empty when filetype is EDF or BDF 
    birthdate::Ptr{Cchar}[16]                # null-terminated string, is always empty when filetype is EDF or BDF 
    patient_name::Ptr{Cchar}[81]             # null-terminated string, is always empty when filetype is EDF or BDF 
    patient_additional::Ptr{Cchar}[81]       # null-terminated string, is always empty when filetype is EDF or BDF 
    admincode::Ptr{Cchar}[81]                # null-terminated string, is always empty when filetype is EDF or BDF 
    technician::Ptr{Cchar}[81]               # null-terminated string, is always empty when filetype is EDF or BDF 
    equipment::Ptr{Cchar}[81]                # null-terminated string, is always empty when filetype is EDF or BDF 
    recording_additional::Ptr{Cchar}[81]     # null-terminated string, is always empty when filetype is EDF or BDF 
    datarecord_duration::Clonglong           # duration of a datarecord expressed in units of 100 nanoSeconds 
    datarecords_in_file::Clonglong           # number of datarecords in the file 
    annotations_in_file::Clonglong           # number of annotations in the file 
    struct edf_param_struct signalparam[EDFLIB_MAXSIGNALS]
                                             # array of structs which contain the relevant signal parameters 
end

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
