#=
@Version: 0.04
@Author: William Herrera, derived from C code by Teunis van Beelen, see below
@Copyright: (Julia code) 2015, 2016, 2017, 2018 William Herrera
@Created: Dec 6 2015
@Purpose: EEG file routines for EDF, BDF, EDF+, and BDF+ files
=#
#
# Note that except for some of the data structures and constants, most of this code is 
# a complete rewrite of the C version. Application ports will need different API calls from C.
#
#==============================================================================
*
* Copyright (c) 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017 Teunis van Beelen
* All rights reserved.
*
* email: teuniz@gmail.com
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY Teunis van Beelen ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL Teunis van Beelen BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES
* LOSS OF USE, DATA, OR PROFITS OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*
* For more info about the EDF and EDF+ format, visit: http://edfplus.info/specs/
* For more info about the BDF and BDF+ format, visit: http://www.teuniz.net/edfbrowser/bdfplus%20format%20description.html
=================================================================================#

# See also: https://www.edfplus.info/specs/edffaq.html

const VERSION = 0.02
const EDFLIB_MAXSIGNALS =                 512
const EDFLIB_MAX_ANNOTATION_LEN =         512

# the following defines are used in the member "filetype" of the edf_hdr_struct
const EDFLIB_FILETYPE_EDF                 = 0
const EDFLIB_FILETYPE_EDFPLUS             = 1
const EDFLIB_FILETYPE_BDF                 = 2
const EDFLIB_FILETYPE_BDFPLUS             = 3
const EDFLIB_MALLOC_ERROR                = -1
const EDFLIB_NO_SUCH_FILE_OR_DIRECTORY   = -2
const EDFLIB_FILE_CONTAINS_FORMAT_ERRORS = -3
const EDFLIB_MAXFILES_REACHED            = -4
const EDFLIB_FILE_READ_ERROR             = -5
const EDFLIB_FILE_ALREADY_OPENED         = -6
const EDFLIB_FILETYPE_ERROR              = -7
const EDFLIB_FILE_WRITE_ERROR            = -8
const EDFLIB_NUMBER_OF_SIGNALS_INVALID   = -9
const EDFLIB_FILE_IS_DISCONTINUOUS      = -10
const EDFLIB_INVALID_READ_ANNOTS_VALUE  = -11

# the following defines are possible errors returned by edfopen_file_writeonly()
const EDFLIB_NO_SIGNALS                 = -20
const EDFLIB_TOO_MANY_SIGNALS           = -21
const EDFLIB_NO_SAMPLES_IN_RECORD       = -22
const EDFLIB_DIGMIN_IS_DIGMAX           = -23
const EDFLIB_DIGMAX_LOWER_THAN_DIGMIN   = -24
const EDFLIB_PHYSMIN_IS_PHYSMAX         = -25


mutable struct ChannelParam      # this structure contains all the relevant EDF-signal parameters of one signal
  label::String                  # label (name) of the signal, null-terminated string
  transducer::String             # signal transducer type
  physdimension::String          # physical dimension (uV, bpm, mA, etc.), null-terminated string
  physmax::Float64               # physical maximum, usually the maximum input of the ADC
  physmin::Float64               # physical minimum, usually the minimum input of the ADC
  digmax::Int                    # digital maximum, usually the maximum output of the ADC, can not not be higher than 32767 for EDF or 8388607 for BDF
  digmin::Int                    # digital minimum, usually the minimum output of the ADC, can not not be lower than -32768 for EDF or -8388608 for BDF
  smp_in_file::Int               # number of samples of this signal in the file
  smp_per_record::Int            # number of samples of this signal in a datarecord
  prefilter::String              # null-terminated string
  reserved::String               # header reserved ascii text, 32 bytes
  offset::Float64
  bufoffset::Int
  bitvalue::Float64
  annotation::Bool
  ChannelParam() = new("","","",0.0,0.0,0,0,0,0,"","",0.0,0,0.0,false)
end


mutable struct Annotation
    onset::Float64
    duration::String
    annotation::String
    Annotation() = new(0.0,"","")
    Annotation(o,d,txt) = new(o,d,txt)
end
# max size of annotationtext
const EDFLIB_WRITE_MAX_ANNOTATION_LEN = 40
# bytes in datarecord for EDF annotations, must be a multiple of three and two
const EDFLIB_ANNOTATION_BYTES = 114


mutable struct EDFPlus                    # signal file data for EDF, BDF, EDF+, and BDF+ files
    iobuf::IOBuffer                       # IOBuffer for read and write of file
    path::String
    writemode::Bool
    version::String
    edf::Bool
    edfplus::Bool
    bdf::Bool
    bdfplus::Bool
    discontinuous::Bool
    filetype::Int                         # 0: EDF, 1: EDFplus, 2: BDF, 3: BDFplus, a negative number means an error
    channelcount::Int                     # total number of EDF signals in the file INCLUDING annotation channels
    file_duration::Float64                # duration of the file in seconds expressed as 64-bit floating point
    startdate_day::Int
    startdate_month::Int
    startdate_year::Int
    starttime_subsecond::Float64          # starttime offset in seconds, should be < 1 sec in size. Only used by EDFplus and BDFplus
    starttime_second::Int                 # this is in integer seconds, the field above makes it more precise
    starttime_minute::Int
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
    datarecord_duration::Float64          # duration of a datarecord expressed in units of seconds
    datarecords::Int64                    # number of datarecords in the file
    startdatestring::String               # date recording started in dd-uuu-yyyy format
    reserved::String                      # reserved, 32 byte string
    hdrsize::Int                          # size of header in bytes
    recordsize::Int                       # size of one data record in bytes, these follow header
    data_record_duration::Float64         # time duration of one data record in seconds
    mapped_signals::Array{Int,1}          # positions in record of channels carrying data
    annotation_channels::Array{Int,1}     # positions in record of annotation channels
    signalparam::Array{ChannelParam,1}        # 1D array of structs which contain the relevant per-signal parameters
    annotations::Array{Array{Annotation,1},2} # 2D array of lists of annotations, rows are records, columns channels
    EDFsignals::Array{Int16,2}    # 2D array, each row a record, columns are channels EXCLUDING annotations
    BDFsignals::Array{Int32,2}    # Note that either EDFsignals or BDFsignals is used but never both
    EDFPlus() = new(IOBuffer(),"",false,"",false,false,false,false,false,0,0,0.0,0,0,0,0.0,0.0,0,
                        "","","","","","","","","","","",0.0,0,"","",0,0,0.0,
                        Array{Int,1}(),Array{Int,1}(),Array{ChannelParam,1}(),
                        Array{Array{EDFAnnotation,1},2}(),Array{Int16,2}(),Array{Int32,2}())
end


function loadfile(path::String, read_annotations::Bool)
    edfh = EDFPlus()
    file = fopeno(path, "rb")
    check_edffile(file, edfh)
    if edfh.filetype == edf_error
        throw("Bad EDF/BDF file format at file $path")
    end
    if edfh.discontinuous
        edfh.filetype = EDFLIB_FILE_IS_DISCONTINUOUS
        throw("discontinuous file type is not yet supported")
    end
    edfh.writemode = false
    if edfh.edf
        edfh.filetype = EDFLIB_FILETYPE_EDF
    end
    if edfh.edfplus
        edfh.filetype = EDFLIB_FILETYPE_EDFPLUS
    end
    if edfh.bdf
        edfh.filetype = EDFLIB_FILETYPE_BDF
    end
    if edfh.bdfplus
        edfh.filetype = EDFLIB_FILETYPE_BDFPLUS
    end

    edfh.file_duration = edfh.data_record_duration * edfh.datarecords

    if edfh.edfplus == false && edfh.bdfplus == false
        edfh.patientcode = ""
        edfh.gender = ""
        edfh.birthdate = ""
        edfh.patientname = ""
        edfh.patient_additional = ""
        edfh.admincode = ""
        edfh.technician = ""
        edfh.equipment = ""
        edfh.recording_additional = ""
    else
        # EDF+ and BDF+ use different ID information so blank other fielsds
        edfh.patient = ""
        edfh.recording = ""
        if read_annotations
            if readannotations(edfh) == 0
                throw("Errors in annotations in file at $path")
            end
        end
    end
    edfh.path = path
    for i in 1:edfh.channelcount
        if !edfh.signalparam[i].annotation
            push!(edfh.mapped_signals, i)
        end
    end
    edfh
end


function readdata(edfh)
    channellength = Int(edfh.recordsize / edfh.channelcount)
    if edfh.bdf || edfh.bdfplus
        bbigbuf = zeros(Int32, (edfh.datarecords*channellength/3, length(edfh.mapped_signals)))
    else
        ebigbuf = zeros(Int16, (edfh.datarecords*channellength/2, length(edfh.mapped_signals)))
    end
    try
        cbuf = zeros(UInt8, channellength)
        seek(edfh.iobuf, edfh.hdrsize + 1)
        for i in 1:edfh.datarecords
            chan = 0
            for j in 1:edfh.channelcount
                read(edfh.iobuf, cbuf)
                if j in edfh.mapped_signals
                    chan += 1
                    if edfh.bdf || edfh.bdfplus
                        readpos = 1
                        startrow = (i-1)*channellength+1
                        for k in startrow:startrow+channellength-1
                            bbigbuf[k,chan] = Int(reinterpret(Int24, cbuf[readpos:readpos+2])[1])
                            readpos +=3
                        end
                    end
                else
                    startrow = (i-1)*channellength+1
                    ebigbuf[startrow:startrow+channellength/2-1, chan] .= reinterpret(Int16, cbuf)
                end
            end
        end
    catch y
        warn("$y\n")
        return -1
    end
    if edfh.bdf || edfh.bdfplus
        edfh.BDFsignals = bbigbuf
    else
        edfh.EDFsignals = ebigbuf
    end
    return 0
end


signaldata(edfh) = (edfh.bdf || edfh.bdfplus) ? BDFsignals : EDFsignals
recordslice(edfh, startpos, endpos) = signaldata(edfh)[startpos:endpos, 1:end]
bytesperdatapoint(edfh) = (edfh.bdfplus || edfh.bdf ) ? 3 : 2
datapointinterval(edfh) = edfh.data_record_duration * bytesperdatapoint(edfh) / edfh.recordsize
signalchannelnumbers(edfh) = edfh.mapped_signals
annotationchannelnumbers(edfh) = edfh.annotation_channels


function recordindexat(edfh, secondsafterstart)
    if edfh.edfplus || edfh.bdfplus
        for i in 1:edfh.datarecords
            firstannot = edfh.annotations[i, 1][1]
            if secondsafterstart > firstannot.onset
                return i
            end
        end
        return edfh.datarecords  # last one is at end
    end
    Int(floor(secs / edfh.record_duration)) + 1    # no annotation times
end


function signalat(edfh, secondsafter)
    ridx = recordindexat(edfh, secondsafter)
    seconddiff = secondsafter - edfh.annotations[ridx, 1][1].onset
    additional = Int(floor(seconddiff / datapointinterval(edfh)))
    Int(floor(recordsize * (ridx - 1) / bytesperdatapoint(edfh))) + additional
end


function epochmarkers(edfh, secs) = map(t->signalat(edfh,t), 0:secs:edfh.file_duration)


epoch_iterator(edfh, epochsecs; channels=edfh.mapped_signals,
                                startsec=0, endsec=edfh.file_duration, physical=true)
    epochs = collect(startsec:epochsecs:endsec)
    if length(epochs) < 2
        epochmarkers = [signalat(edfh,startsec), signalat(edfh,endsec)]
    else
        epochmarkers = map(t->signalat(edfh,t), startsec:epochsecs:endsec)
    end
    multiplier = edfh.signalparam[channelnumber].bitvalue
    epochwidth = epochmarkers[2] - epochmarkers[1]
    data = signaldata(edfh)
    if physical
        data .*= multiplier
    end
    imap(x -> data[x:x+epochwidth, channels], epochmarkers)
end


annotation_epoch_iterator(edfh, epochsecs; channels=edfh.annotation_signals,
                                startsec=0, endsec=edfh.file_duration)
    epochs = collect(startsec:epochsecs:endsec)
    if length(epochs) < 2
        epochmarkers = [signalat(edfh,startsec), signalat(edfh,endsec)]
    else
        epochmarkers = map(t->signalat(edfh,t), startsec:epochsecs:endsec)
    end
    epochwidth = epochmarkers[2] - epochmarkers[1]
    imap(x -> edfh.annotations[x:x+epochwidth, channels], epochmarkers)
end


function digitalchanneldata(edfh, channelnumber)
    if !(channelnumber in edfh.mapped_signals)
        return []
    end
    signaldata(edfh)[1:end,channelnumber]
end


# translate to physical measure with bit value etc.
function physicalchanneldata(edfh, channelnumber)
    digdata = getdigitaldata(edfh, channelnumber)
    if length(digdata) < 1
        return digdata
    end
    multiplier = edfh.signalparam[channelnumber].bitvalue
    return digdata .* multiplier
end


function closefile(edfh)
    close(edfh.iobuf)
    return 0
end


function check_edffile(inputfile, edfh)
    function throwifhasforbiddenchars(bytes)
        if findfirst(c -> (c < 32) || (c > 126), bytes) > 0
            throw("Control type forbidden chars in header")
        end
    end
    rewind(inputfile)
    try
        hdrbuf = read(inputfile, UInt8, 256)                    # check header
        if hdrbuf[1:8] == b"\xffBIOSEMI"                        # version bdf
            edfh.bdf = true
            edfh.edf = false
            edfh.version = "BIOSEMI"
        elseif hdrbuf[1:8] == b"\0       "                      # version edf
            edfh.bdf = false
            edfh.edf = true
            edfh.version = ""
        else
            throw("identification code error")
        end
        throwifhasforbiddenchars(hdrbuf[9:88])
        edfh.patient = convert(String, hdrbuf[9:88])            # patient
        throwifhasforbiddenchars(hdrbuf[89:168])
        edfh.recording = convert(String, hdrbuf[89:168])        # recording
        throwifhasforbiddenchars(hdrbuf[169:176])
        datestring = convert(String, hdrbuf[169:176])           # start date
        date = Date(datestring, "dd.mm.yy")
        if Dates.year(date) < 84
            date += Dates.Year(2000)
        else
            date += Dates.Year(1900)
        end
        edfh.startdate_day = Dates.Day(date)
        edfh.startdate_month = Dates.Month(date)
        edfh.startdate_year = Dates.Year(date)
        throwifhasforbiddenchars(hdrbuf[177:184])
        timestring = convert(String, hdrbuf[177:184])           # start time
        mat = match(timestring, r"(\d\d).(\d\d).(\d\d)")
        starttime_hour, starttime_minute, starttime_second = mat.captures
        edfh.starttime_hour = starttime_hour
        edfh.starttime_minute = starttime_minute
        edfh.starttime_second = starttime_second
        throwifhasforbiddenchars(hdrbuf[185:192])
        headersize = parse(Int32, hdrbuf[185:192])              # edf header size, changes with channels
        if edfh.channelcount * 256 + 256 != headersize
            throw("Bad header size")
        end
        throwifhasforbiddenchars(hdrbuf[193:236])
        subtype = convert(String, hdrbuf[193:197])              # subtype or version of data format
        if edfh.edf
            if subtype == "EDF+C"
                edfh.edfplus = true
            elseif subtype == "EDF+D"
                edfh.edfplus = true
                edfh.discontinuous = true
            end
        elseif edfh.bdf
            if subtype == "BDF+C"
                edfh.bdfplus = true
            elseif subtype == "BDF+D"
                edfh.bdfplus = true
                edfh.discontinuous = true
            end
        end
        throwifhasforbiddenchars(hdrbuf[237:244])
        edfh.datarecords = parse(Int32, hdrbuf[237:244])  # number of data records
        if edfh.datarecords < 1
            throw("Bad data record count: unknown or < 1")
        end
        throwifhasforbiddenchars(hdrbuf[245:252])
        edfh.data_record_duration = parse(Float32, hdrbuf[245:252]) # datarecord duration in seconds
        throwifhasforbiddenchars(hdrbuf[253:256])
        edfh.channelcount = parse(Int16, hdrbuf[253:256])         # number of data signals or records (channels) in file
        if edfh.channelcount < 0 || edf.channelcount > EDF_MAXSIGNALS
            throw("bad channel count")
        end

    catch y
        warn("$y\n")
        edfh.filetype = edf_error
        return edfh
    end

    # process the signal characteristics in the header after reading header into hdrbuf
    rewind(inputfile)
    try
        hdrbuf = read(inputfile, UInt8, (edfh.channelcount + 1) * 256)
        for i in 1:edfh.channelcount  # loop over channel signal parameters
            pblock = ChannelParam()
            # channel label gets special handling since it might indicate an annotations channel
            pos = 257 + (i-1) * 16
            channellabel = convert(String, hdrbuf[pos:pos+15])
            throwifhasforbiddenchars(channellabel)
            pblock.label = channellabel                         # channel label in ASCII, eg "Fp1"
            if (edfh.edfplus && channellabel == "EDF Annotations ") ||
               (edfh.bdfplus && channellabel == "BDF Annotations ")
                push!(edfh.annotation_channels, i)
                pblock.annotation = true
            else
                push!(edfh.mapped_signals, i)
            end
            pos = 257 + edfh.channelcount * 16 + (i-1) * 80
            transducertype = convert(String, hdrbuf[pos:pos+79])
            throwifhasforbiddenchars(transducertype)
            pblock.transducer = transducertype                    # transducer type eg "active electrode"
            if pfblock.annotation && findfirst(c->!isspace(c), transducertype) > 0
                throw("Transducer field should be blank in annotation channels")
            end
            pos = 257 + edfh.channelcount * 96 + (i-1) * 8
            pblock.physdimension = convert(String, hdrbuf[pos:pos+7])# physical dimensions eg. "uV"
            pos = 257 + edfh.channelcount * 104 + (i-1) * 8
            pblock.physmin = parse(Float32, hdrbuf[pos, pos+7])  # physical minimum in above dimensions
            pos = 257 + edfh.channelcount * 112 + (i-1) * 8
            pblock.physmax = parse(Float32, hdrbuf[pos, pos+7])  # physical maximum in above dimensions
            pos = 257 + edfh.channelcount * 120 + (i-1) * 8
            pblock.digmin = parse(Float32, hdrbuf[pos, pos+7])   # digital minimum in above dimensions
            pos = 257 + edfh.channelcount * 128 + (i-1) * 8
            pblock.digmax = parse(Float32, hdrbuf[pos, pos+7])   # digital maximum in above dimensions
            if edfh.edfplus && pblock.annotation && (pblock.digmin != -32768 || pblock.digmax != 32767)
                throw("edfplus annotation data entry should have the digital min parameters set to extremes")
            elseif edfh.bdfplus && pblock.annotation && (pblock.digmin != -8388608 || pblock.digmax != 8388607)
                throw("bdf annotation data entry should have the digital max parameters set to extremes")
            elseif edfh.edf && (pblock.digmin < -32768 || pblock.digital_min > 32767 ||
                   pblock.digmax < -32768 || pblock.digital_max > 32767)
                throw("edf digital parameter out of range")
            elseif edfh.bdf && (pblock.digmin < -8388608 || pblock.digital_min > 8388607 ||
                   pblock.digmax < -8388608 || pblock.digital_max > 8388607)
                throw("bdf digital parameter out of range")
            end
            pos = 257 + edfh.channelcount * 136 + (i-1) * 80
            pfchars = convert(String, hdrbuf[pos:pos+79])
            throwifhasforbiddenchars(pfchars)
            pblock.prefilter = pfchars                            # prefilter field eg "HP:DC"
            if pfblock.annotation && findfirst(c->!isspace(c), pfchars) > 0
                throw("Prefilter field should be blank in annotation channels")
            end
            pos = 257 + edfh.channelcount * 216 + (i-1) * 8
            pblock.smp_per_record = parse(Int, hdrbuf[pos,pos+7]) # number of samples of this channel per data record
            edfh.recordsize += pblock.smp_per_record
            pos = 257 + edfh.channelcount * 224 + (i-1) * 32
            reserved = convert(String, hdrbuf[pos:pos+31])        # reserved text field
            throwifhasforbiddenchars(reserved)
            pblock.reserved = reserved
            if pblock.annotation == false && !edfh.edfplus && !edfh.bdfplus &&
                                             edfh.data_record_duration < 0.0000001
                    throw("signal data may be mislabeled")
            end
            push!(edfh.signalparam, pblock)
        end
        if edfh.bdf
            edfh.recordsize *= 3
            if edfh.recordsize > 1578640
                throw("record size too large for a bdf file")
            end
        else
            edfh.recordsize *= 2
            if edfh.recordsize > 10485760
                    throw("Record size too large for an edf file")
            end
        end
    catch y
        warn("$y\n")
        edfh.filetype = EDFLIB_FILE_CONTAINS_FORMAT_ERRORS
        return edfh
    end

    #=
    from https://www.edfplus.info/specs/edfplus.html#header, December 2017:

    The 'local patient identification' field must start with the subfields
         (subfields do not contain, but are separated by, spaces):

    - the code by which the patient is known in the hospital administration.
    - sex (English, so F or M).
    - birthdate in dd-MMM-yyyy format using the English 3-character abbreviations
      of the month in capitals. 02-AUG-1951 is OK, while 2-AUG-1951 is not.
    - the patients name.

     Any space inside the hospital code or the name of the patient must be replaced
     by a different character, for instance an underscore. For instance, the
     'local patient identification' field could start with:
     MCH-0234567 F 02-MAY-1951 Haagse_Harry.

    Subfields whose contents are unknown, not applicable or must be made
    anonymous are replaced by a single character 'X'.

    Additional subfields may follow the ones described here.
    =#
    try
        if edfh.edfplus || edfh.bdfplus
            subfield = split(edfh.patient)
            if length(subfield) < 4
                throw("Plus patient identification lacking enough fields")
            end
            edfh.patientcode = subfield[1][1] == 'X' ? "" : subfield[1]
            edfh.patientcode = replace(edfh.plus_patientcode, "_", " ")
            if subfield[2] != "M" && subfield[2] != "F" && subfield[2] != "X"
                throw("patient identification second field must be X, F or M")
            elseif subfield[2] == "M"
                edfh.gender = "Male"
            elseif subfield[2] == "F"
                edfh.gender = "Female"
            else
                edfhdf.gender = ""
            end
            if subfield[3] == "X"
                edfh.birthdate = ""
            elseif !contains("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC", subfield[3][4:6]) ||
                   !(Date(subfield[3], "dd-uuu-yyyy") isa Date)
                throw("Bad birthdate field in patient identification")
            else
                edfh.birthdate = subfield[3]
            end
            edfh.patientname = replace(subfield[4], "_", " ")
            if length(subfield) > 4
                edfh.patient_additional = join(subfield[5:end], " ")
            end
    #=
     The 'local recording identification' field must start with the subfields
     (subfields do not contain, but are separated by, spaces):
     - The text 'Startdate'.
     - The startdate itself in dd-MMM-yyyy format using the English 3-character
       abbreviations of the month in capitals.
     - The hospital administration code of the investigation, i.e. EEG number or PSG number.
     - A code specifying the responsible investigator or technician.
     - A code specifying the used equipment.
     Any space inside any of these codes must be replaced by a different character,
     for instance an underscore. The 'local recording identification' field could
     contain: Startdate 02-MAR-2002 PSG-1234/2002 NN Telemetry03.
     Subfields whose contents are unknown, not applicable or must be made anonymous
     are replaced by a single character 'X'. So, if everything is unknown then the
     'local recording identification' field would start with:
     Startdate X X X X. Additional subfields may follow the ones described here.
    =#
            subfield = split(edfh.recording)
            if length(subfield) < 5
                throw("Not enough fields in plus recording data")
            elseif subfield[1] != "Startdate" || (subfield[2] != "X" &&
                 (!(dor = Date(subfield[2], "dd-uuu-yyyy") isa Date) ||
                  !contains("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC", subfield[2][4:6])))
                throw("Bad recording field start")
            elseif subfield[2] == "X"
                edfh.startdatestring = ""
            else
                edfh.startdatestring = subfield[2]
                if Dates.year(dor) < 1970
                    throw("bad startdate year in recording data")
                else
                    edfh.startdate_year = Dates.year(dor)
                end
            end
            if subfield[3] == ""
                edfh.admincode = ""
            else
                edfh.admincode = replace(subfield[3], "_", " ")
            end
            if subfield[4] == ""
                edfh.technician = ""
            else
                edfh.technician = replace(subfield[4], "_", " ")
            end
            if subfield[5] == ""
                edfh.equipment = ""
            else
                edfh.equipment = replace(subfield[5], "_", " ")
            end
            if length(subfield) > 5
                edfh.additional = replace(join(subfield[6:end], " "), "_", " ")
            end
        end

        edfh.hdrsize = (edfh.channelcount + 1) * 256
        filesize = filesize(edfh.path)   # get actual file size
        if filesize != edfh.recordsize * edfh.datarecords + edfh.hdrsize
            throw("file size is not compatible with header information")
        end
    catch y
        warn("$y\n")
        edf_error[1] = EDFLIB_FILE_CONTAINS_FORMAT_ERRORS
        return 0
    end

    n = 0
    for i in i: edfh.channelcount
        edfh.edfparam[i].bufoffset = n
        n += edfh.edfparam[i].smp_per_record * (edfh.bdf ? 3 : 2)
        edfh.edfparam[i].bitvalue = (edfh.edfparam[i].physmax - edfh.edfparam[i].physmin) /
                                      (edfh.edfparam[i].digmax - edfh.edfparam[i].digmin)
        edfh.edfparam[i].offset = edfh.edfparam[i].physmax / edfh.edfparam[i].bitvalue -
                                    edfh.edfparam[i].digmax
    end
    edfh.iobuf = inputfile
    return edfh
end


function readannotations(edfh)
    inputfile = edfh.iobuf
    channelcount = edfh.channelcount
    recordsize = edfh.recordsize
    edfparam = edfh.edfparam
    datarecords = edfh.datarecords
    data_record_duration = edfh.data_record_duration
    discontinuous = edfh.discontinuous
    annot_ch = edfh.annotation_channels
    samplesize = edfh.edfplus ? 2 : (edfh.bdfplus ? 3: 1)

    max_tal_ln = 0
    for chan in annot_ch
        if max_tal_ln < edfparam[chan].smp_per_record * samplesize
            max_tal_ln = edfparam[chan].smp_per_record * samplesize
        end
    end
    if max_tal_ln < 128
        max_tal_len = 128
    end
    seek(inputfile, (channelcount + 1) * 256)
    elapsedtime = 0
    annotations = Array{Array{Annotation,1},2}(datarecords, length(annot_ch))()
    fill!(annotations,[])
    for i in 1:datarecords
        try
            cnvbuf = read(inputfile, UInt8, recordsize)
            for chan in annot_ch
                startpos = edfparam[chan].bufoffset + 1
                endpos = edfparam[chan].bufoffset + edfparam[chan].smp_per_record * samplesize
                annotbuf = convert(String, cvnbuf[startpos:endpos])
                for (j, tal) in enumerate(split(annotbuf, "\x00"))
                    if tal = ""
                        break # padding zeroes at end
                    end
                    onset = 0.0  # time in seconds + or - from startdate/starttime of file
                    duration = ""  # duration in seconds in text form, may be empty
                    alist = []
                    for (k, annot) in enumerate(split(tal, "\x14"))
                        if j == 1 && k == 1 # first annot channel of first record should be timekeeping signal
                                offsettimestr = match(annot, r"^(\d+)").captures[1]
                                edfh.starttime_subsecond = parse(Float64, offsettimestr)
                        elseif k == 1  # first record of others is time and optional duration of event annotated
                            if length(tsignal = split(annot, "\x15")) > 1
                                onset = convert(Float64, tsignal[1])
                                duration = convert(String, tsignal[2])
                            else
                                onset = convert(Float64, tsignal[1])
                                duration = ""
                            end
                        else
                            annottxt = convert(String, annot)
                            if length(annottxt) > EDFLIB_MAX_ANNOTATION_LEN
                                printf("Annotation at $i $j $k too long, truncated")
                                annottxt = annottxt[1:EDFLIB_MAX_ANNOTATION_LEN]
                            end
                            newannot = Annotation(onset, duration, annottxt)
                            annotations[i,j] = vcat(annotations[i,j], newannot)
                        end
                    end
                end
            end
        catch y
            warn("$y\n")
            return 9
        end
    end
    return 0
end


function write_EDFplus(edfh, newpath)

end


function write_BDFplus(edfh,newpath)
end


function writeheader(edfh::EDFPlus)
    file = edfh.iobuf
    channelcount = edfh.channelcount
    if channelcount < 0
        return -20
    elseif channelcount > EDFLIB_MAXSIGNALS
        return -21
    end
    for i in 1:channelcount
        if edfh.edfparam[i].smp_per_record < 1
            return -22
        elseif edfh.edfparam[i].digmax == edfh.edfparam[i].digmin
            return -23
        elseif edfh.edfparam[i].digmax < edfh.edfparam[i].digmin
            return -24
        elseif edfh.edfparam[i].physmax == edfh.edfparam[i].physmin
            return(-25)
        end
    end
    for i in 1:channelcount
        edfh.edfparam[i].bitvalue = (edfh.edfparam[i].physmax - edfh.edfparam[i].physmin) /
                                   (edfh.edfparam[i].digmax - edfh.edfparam[i].digmin)
        edfh.edfparam[i].offset = edfh.edfparam[i].physmax /
                                 edfh.edfparam[i].bitvalue - edfh.edfparam[i].digmax
    end
    rewind(file)
    if(edfh.edf)
        write(file, "0       ")
    else
        write(file, b"\xffBIOSEMI")
    end
    pidbytes = edfh.patientcode = "" ? "X " : replace(edfh.patientcode, " ", "_") * " "
    if edfh.gender[1] == 'M'
        pidbytes *= "M "
    elseif edfh.gender[1] == 'F'
        pidbytes *= "F "
    else
        pidbytes *= "X "
    end
    if edfh.plus_birthdate != ""
        pidbytes += write(file, "X ")
    else
        pidbytes *= edfh.birthdate * " "
    end
    if edfh.plus_patientname == ""
        pidbytes *= "X "
    else
        pidbytes *= replace(edfh.patientname, " ", "_") * " "
    end
    if edfh.plus_patient_additional != ""
        pidbytes *= replace(edfh.patient_additional)
    end
    if length(pidbytes) > 80
        pidbytes = pidbytes[1:80]
    else
        while length(pidbytes) < 80
            pidbytes *= " "
        end
    end
    write(file, pidbytes)

    if edfh.startdate_year != 0
        date = DateTime(edfh.startdate_year, edfh.startdate_month, edfh.startdate_day,
                        edfh.starttime_hour, edfh.starttime_minute, edfh.starttime_second)
    else
        date = now()
    end

    ridbytes = "Startdate " * uppercase(Dates.format(date, "dd-uuu-yyyy")) * " "
    if edfh.plus_admincode == ""
        ridbytes *= "X "
    else
        ridbytes *= replace(edfh.admincode, " ", "_")
    end
    if edfh.plus_technician == ""
        ridbytes *= "X "
    else
        ridbytes *= replace(edfh.technician, " ", "_")
    end
    if edfh.plus_equipment == ""
        ridbytes *= "X "
    else
        ridbytes *= replace(edfh.equipment, " ", "_")
    end
    if edfh.plus_recording_additional != ""
        ridbytes *= replace(edfh.recording_additional, " ", "_")
    end
    writeleftjust(file, ridbytes, 80)
    startdate = Dates.format(date, "dd.mm.yy")
    write(file, startdate)
    starttime = Dates.format(date, "HH.MM.SS")
    write(file, starttime)
    writeleftjust(file, (channelcount + 1) * 256, 8)

    if edfh.edf
        writeleftjust(file, "EDF+C", 44)
    else
        writeleftjust(file, "BDF+C", 44)
    end
    # header initialized to -1 in duration in case data is not finalized yet
    # This must be updated when final channel data is written.
    writeleftjust(file, "-1      ", 8)
    if floor(edfh.data_record_duration) == edfh.data_record_duration
        writeleftjust(file, Int(edfh.data_record_duration), 8)
    else
        writeleftjust(file, "$(edfh.data_record_duration)", 8)
    end
    writeleftjust(file, channelcount, 4)
    for i in 1:channelcount
        if edfh.edfparam[i].annotation
            if edfh.edf
                write(file, "EDF Annotations ")
            else
                write(file, "BDF Annotations ")
            end
        else
            writeleftjust(file, edfh.edfparam[i].label, 16)
        end
    end
    for i in 1:channelcount
        writeleftjust(file, edfh.edfparam[i].transducer, 80)
    end
    for i in 1:channelcount
        writeleftjust(file, edfh.edfparam[i].physdimension, 8)
    end
    for i in 1:channelcount
        writejustleft(file, hdr->edfparam[i].physmin, 8)
    end
    for i in 1:channelcount
        writejustleft(file, hdr->edfparam[i].physmax, 8)
    end
    for i in 1:channelcount
        writejustleft(file, hdr->edfparam[i].digmin, 8)
    end
    for i in 1:channelcount
        writejustleft(file, hdr->edfparam[i].digmax, 8)
    end
    for i in 1:channelcount
        writejustleft(file, edfh.edfparam[i].prefilter, 80)
    end
    for i in 1:channelcount
        writejustleft(edfh.edfparam[i].smp_per_record, 8)
    end
    for i in 1:channel_count
        writejustleft(file, edfh.reserved, 32)
    end
    return 0
end

function getannotationbyonset(edfh, onset)
end

function addannotation(edfh, onset, duration, description)
    newannot = EDFAnnotation()
    newannot.onset = onset
    newannot.duration = duration
    if length(description) > EDFLIB_WRITE_MAX_ANNOTATION_LEN
        description = description[1:EDFLIB_WRITE_MAX_ANNOTATION_LEN]
    end
    newannot.annotation = latintoacsii(replace(description, r"[\0-\x1f]", "."))
    # determine which place record is in around where we should add
    nearest = getannotationbyonset(edfh, onset)
    push!(annotations[handle], newannot)
    edfh.annots_in_file += 1
    return 0
end


edflib_version() = EDFLIB_VERSION


function writeleftjust(file, str::String, len, fillchar=" ")
    strlen = length(str)
    if strlen > len
        str = str[1:len]
    end
    if strlen > 0
        bytelen = write(file, str)
    else
        bytelen = 0
    end
    while bytelen < len
        write(file, UInt8(fillchar))
        len += 1
    end
end
writeleftjust(file, n::Int, len, fillchar=" ") = writeleftjust(file, "$n", len, fillchar=" ")
writeleftjust(file, x::Float64, len, fillchar=" ") = writeleftjust(file, "$x", len, fillchar=" ")


function unleftjust(str, maxlen)
    str = replace(str, r"^\s*(\S.*\S)\s*$", s"\1")
    if length(str) > maxlen
        str = str[1:maxlen]
    end
end


function float2fixedwidth(x, width=8, leftjustified=true)
    if x == 0.0
        if leftjustified
            return "0.0" * (width > 3 ?  " "^(width-3) : "")
        else
            (width > 3 ?  " "^(width-3) : "") * "0.0"
        end
    end
    neg = x < 0.0
    if neg
        x = -x
    end
    str = @sprintf("%016.7f", x)
    str = replace(str, r"^[0]*([^0].+)", s"\1")
    str = replace(str, r"(.+[^0])[0]*$", s"\1")
    if length(str) < width && str[1] == '.'
        str = "0" * str
    end
    if neg
        str = "-" * str
    end
    len = length(str)
    if len > width
        decimalplaces = len - search(str, '.')
        if len - decimalplaces - 1 > width
            throw("too many significant decimal places on left")
        end
        str = str[1:8]
    else
        while length(str) < width
            if leftjustified
                str = str * " "
            else
                str = " " * str
            end
        end
    end
    str
end


const latin_dict = Dict(
'¡'=> '!', '¢'=> 'c', '£'=> 'L', '¤'=> 'o', '¥'=> 'Y',
'¦'=> '|', '§'=> 'S', '¨'=> '`', '©'=> 'c', 'ª'=> 'a',
'«'=> '<', '¬'=> '-', '®'=> 'R', '¯'=> '-',
'°'=> 'o', '±'=> '+', '²'=> '2', '³'=> '3', '´'=> '`',
'µ'=> 'u', '¶'=> 'P', '·'=> '.', '¸'=> ',', '¹'=> '1',
'º'=> 'o', '»'=> '>', '¼'=> '/', '½'=> '/', '¾'=> '/',
'¿'=> '?', 'À'=> 'A', 'Á'=> 'A', 'Â'=> 'A', 'Ã'=> 'A',
'Ä'=> 'A', 'Å'=> 'A', 'Æ'=> 'A', 'Ç'=> 'C', 'È'=> 'E',
'É'=> 'E', 'Ê'=> 'E', 'Ë'=> 'E', 'Ì'=> 'I', 'Í'=> 'I',
'Î'=> 'I', 'Ï'=> 'I', 'Ð'=> 'D', 'Ñ'=> 'N', 'Ò'=> 'O',
'Ó'=> 'O', 'Ô'=> 'O', 'Õ'=> 'O', 'Ö'=> 'O', '×'=> '*',
'Ø'=> 'O', 'Ù'=> 'U', 'Ú'=> 'U', 'Û'=> 'U', 'Ü'=> 'U',
'Ý'=> 'Y', 'Þ'=> 'p', 'ß'=> 'b', 'à'=> 'a', 'á'=> 'a',
'â'=> 'a', 'ã'=> 'a', 'ä'=> 'a', 'å'=> 'a', 'æ'=> 'a',
'ç'=> 'c', 'è'=> 'e', 'é'=> 'e', 'ê'=> 'e', 'ë'=> 'e',
'ì'=> 'i', 'í'=> 'i', 'î'=> 'i', 'ï'=> 'i', 'ð'=> 'd',
'ñ'=> 'n', 'ò'=> 'o', 'ó'=> 'o', 'ô'=> 'o', 'õ'=> 'o',
'ö'=> 'o', '÷'=> '/', 'ø'=> 'o', 'ù'=> 'u', 'ú'=> 'u',
'û'=> 'u', 'ü'=> 'u', 'ý'=> 'y', 'þ'=> 'p', 'ÿ'=> 'y')

function latintoascii(str)
    len = length(str)
    if len < 1
        return str
    else
        for i in 1:len
            if haskey(latin_dict, str[i])
                str[i] = latin_dict[str[i]]
            end
        end
    end
    str
end

