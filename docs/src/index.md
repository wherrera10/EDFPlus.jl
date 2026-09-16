# EDFPlus.jl

<img src="https://github.com/wherrera10/EDFPlus.jl/blob/master/docs/src/eeg.png">

Julia for handling BDF+ and EDF+ EEG and similar signal data files.

Heavily influenced by the C EEG library edflib.

## Data Structs and Functions

### Structs:	
    
    mutable struct ChannelParam      # this structure contains all the relevant EDF-signal parameters of one signal
        label::String                  # label (name) of the signal, eg "C4" if in 10-20 labeling terms
        transducer::String             # signal transducer type
        physdimension::String          # physical dimension (uV, bpm, mA, etc.)
        physmax::Float64               # physical maximum, usually the maximum input of the ADC
        physmin::Float64               # physical minimum, usually the minimum input of the ADC
        digmax::Int                    # digital maximum, usually the maximum output of the ADC, cannot be higher than 32767 for EDF or 8388607 for BDF
        digmin::Int                    # digital minimum, usually the minimum output of the ADC, cannot be lower than -32768 for EDF or -8388608 for BDF
        smp_per_record::Int            # number of samples of this signal in a datarecord
        prefilter::String              # channel prefiltering settings if any
        reserved::String               # header reserved ascii text, 32 bytes
        offset::Float64                # offset of center of physical data value from center of digital values
        bufoffset::Int                 # bytes from start of record to start of this channel (zero for first channel)
        bitvalue::Float64              # physical data value of one unit change in digital value
        annotation::Bool               # true if is an annotation not a binary mapped signal data channel
        ChannelParam() = new("","","",0.0,0.0,0,0,0,"","",0.0,0,0.0,false)
    end

Parameters for each channel in the EEG record.
<br /><br /><br />

    mutable struct Annotation
        onset::Float64
        duration::String
        annotation::Array{String,1}
        Annotation() = new(0.0,"",[])
        Annotation(o,d,arr) = new(o, d, typeof(arr) == String ? [arr] : arr)
    end

These are text strings within the file denoting a time, optionally duration, and a list of notes
about the signal at that particular time in the recording. The first onset time
of the annotation channel gives a fractional second offset adjustment of the
start time of that record, which is specified in whole seconds in the header.
<br /><br /><br />

    mutable struct BEDFPlus                   # signal file data for EDF, BDF, EDF+, and BDF+ files
        ios::IOStream                         # file handle for the file containing the data
        path::String                          # file pathname
        writemode::Bool                       # true if is intended for writing to file
        version::String                       # version of the file format
        edf::Bool                             # EDF?
        edfplus::Bool                         # EDF+?
        bdf::Bool                             # BDF?
        bdfplus::Bool                         # BDF+?
        discontinuous::Bool                   # discontinuous (EDF+D?)
        filetype::FileStatus                  # @enum FileStatus as above
        channelcount::Int                     # total number of EDF signal bands in the file INCLUDING annotation channels
        file_duration::Float64                # duration of the file in seconds expressed as 64-bit floating point
        startdate_day::Int                    # startdate of study, day of month of startdate of study
        startdate_month::Int                  # startdate month
        startdate_year::Int                   # startdate year
        starttime_subsecond::Float64          # starttime offset in seconds, should be < 1 sec in size. Only used by EDFplus and BDFplus
        starttime_second::Int                 # this is in integer seconds, the field above makes it more precise
        starttime_minute::Int                 # startdate and time, minutes
        starttime_hour::Int                   # 0 to 23, midnight is 00:00:00
        # next 11 fields are for EDF+ and BDF+ files only
        patient::String                       # contains patientfield of header, is always empty when filetype is EDFPLUS or BDFPLUS
        recording::String                     # contains recordingfield of header, is always empty when filetype is EDFPLUS or BDFPLUS
        patientcode::String                   # empty when filetype is EDF or BDF
        gender::String                        # empty when filetype is EDF or BDF
        birthdate::String                     # empty when filetype is EDF or BDF
        patientname::String                   # empty when filetype is EDF or BDF
        patient_additional::String            # empty when filetype is EDF or BDF
        admincode::String                     # empty when filetype is EDF or BDF
        technician::String                    # empty when filetype is EDF or BDF
        equipment::String                     # empty when filetype is EDF or BDF
        recording_additional::String          # empty when filetype is EDF or BDF
        datarecord_duration::Float64          # duration of one datarecord in units of seconds
        datarecords::Int64                    # number of datarecords in the file
        startdatestring::String               # date recording started in dd-uuu-yyyy format
        reserved::String                      # reserved, 32 byte string
        headersize::Int                       # size of header in bytes
        recordsize::Int                       # size of one data record in bytes, these follow header
        annotationchannel::Int                # position in record of annotation channel
        mapped_signals::Array{Int,1}          # positions in record of channels carrying data
        signalparam::Array{ChannelParam,1}    # Array of structs which contain the per-signal parameters
        annotations::Array{Array{Annotation,1},1} # Array of lists of annotations
        EDFsignals::Array{Int16,2}    # 2D array, each row a record, columns are channels including annotations
        BDFsignals::Array{Int32,2}    # Note that either EDFsignals or BDFsignals is used
        BEDFPlus() = new(IOStream("nothing"),"",false,"",false,false,false,false,false,READ_ERROR,0,0.0,0,0,0,0.0,0,0,0,
                            "","","","","","","","","","","",0.0,0,"","",0,0,0,
                            Array{Int,1}(undef,0),Array{ChannelParam,1}(undef,0),
                            Array{Array{Annotation,1},1}(undef,0),Array{Int16,2}(undef,0,0),Array{Int32,2}(undef,0,0))
    end

Data struct for EDF, EDF+, BDF, and BDF+ EEG type signal files.
<br /><br /><br />

    DataFormat

enum for types this package handles. Current format for a potential translation is also /same/.
<br /><br /><br />

    FileStatus

enum for type or state of file: type of data detected, whether any errors
<br /><br /><br />

## Installation
```julia

# can be installed as usual with ]add EDFPlus at REPL command line, or with Pkg:

using Pkg
Pkg.add("EDFPlus")
```

## Functions Reference
This block automatically extracts docstrings from your source code:

```@index
```

```@autodocs
Modules = [EDFPlus]
```
