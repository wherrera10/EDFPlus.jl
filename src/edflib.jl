#=
@Version: 0.021
@Author: William Herrera, derived from C code by Teunis van Beelen, see below
@Copyright: (Julia code) 2015, 2016, 2017, 2018 William Herrera
@Created: Dec 6 2015
@Purpose: EEG file routines for EDF, BDF, EDF+, and BDF+ files
=#

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

const EDFSEEK_SET =                         0
const EDFSEEK_CUR =                         1
const EDFSEEK_END =                         2

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


mutable struct EDFParam          # this structure contains all the relevant EDF-signal parameters of one signal
  label::String                  # label (name) of the signal, null-terminated string
  transducer::String             # signal transducer type
  physdimension::String          # physical dimension (uV, bpm, mA, etc.), null-terminated string
  phys_max::Float64              # physical maximum, usually the maximum input of the ADC
  phys_min::Float64              # physical minimum, usually the minimum input of the ADC
  dig_max::Int                   # digital maximum, usually the maximum output of the ADC, can not not be higher than 32767 for EDF or 8388607 for BDF
  dig_min::Int                   # digital minimum, usually the minimum output of the ADC, can not not be lower than -32768 for EDF or -8388608 for BDF
  smp_in_file::Int               # number of samples of this signal in the file
  smp_per_record::Int            # number of samples of this signal in a datarecord
  prefilter::String              # null-terminated string
  reserved::String               # header reserved ascii text, 32 bytes
  offset::Float64
  buf_offset::Int
  bitvalue::Float64
  annotation::Bool
  sample_pntr::Int64
  EDFParam() = new("","","",0.0,0.0,0,0,0,0,"","",0.0,0,0.0,false,0)
end

mutable struct EDFAnnotation
    onset::Float64
    duration::String
    annotation::String
    EDFAnnotation() = new(0.0,"","")
end
# max size of annotationtext
const EDFLIB_WRITE_MAX_ANNOTATION_LEN = 40
# bytes in datarecord for EDF annotations, must be a multiple of three and two
const EDFLIB_ANNOTATION_BYTES = 114
const EDFLIB_ANNOT_MEMBLOCKSZ = 1000


mutable struct EDFPlus                    # signal file data for EDF, BDF, EDF+, and BDF+ files
    handle::Int                           # a handle (identifier) used to distinguish the different files
    file_hdl::IOBuffer                    # IOBuffer for read and write of file
    path::String
    writemode::Bool
    version::String
    edf::Bool
    edfplus::Bool
    bdf::Bool
    bdfplus::Bool
    discontinuous::Bool
    filetype::Int                         # 0: EDF, 1: EDFplus, 2: BDF, 3: BDFplus, a negative number means an error
    edfsignals::Int                       # number of EDF signals in the file, annotation channels are NOT included
    file_duration::Float64                # duration of the file in seconds expressed as 64-bit floating point
    startdate_day::Int
    startdate_month::Int
    startdate_year::Int
    starttime_subsecond::Float64          # starttime offset in seconds, should be < 1 sec in size. Only used by EDFplus and BDFplus
    starttime_second::Int                 # this is in integer seconds, the field above makes it more precise
    starttime_minute::Int
    starttime_hour::Int                   # 0 to 23, midnight is 00:00:00
    # next 12 fields are for EDF+ and BDF+ files only
    patient::String                       # contains patientfield of header, is always empty when filetype is EDFPLUS or BDFPLUS
    recording::String                     # contains recordingfield of header, is always empty when filetype is EDFPLUS or BDFPLUS
    patientcode::String                   # empty when filetype is EDF or BDF
    gender::String                        # empty when filetype is EDF or BDF
    birthdate::String                     # empty when filetype is EDF or BDF
    patient_name::String                  # empty when filetype is EDF or BDF
    patient_additional::String            # empty when filetype is EDF or BDF
    admincode::String                     # empty when filetype is EDF or BDF
    technician::String                    # empty when filetype is EDF or BDF
    equipment::String                     # empty when filetype is EDF or BDF
    recording_additional::String          # empty when filetype is EDF or BDF
    datarecord_duration::Float64          # duration of a datarecord expressed in units of seconds
    datarecords_in_file::Int64            # number of datarecords in the file
    annotations_in_file::Int64            # number of annotation channels in the file
    plus_startdate::String                # date recording started in dd-uuu-yyyy format
    l_starttime::Int64                    # time recording started in seconds from midnight
    reserved::String                      # reserved, 32 byte string
    hdrsize::Int
    datarecords::Int64
    recordsize::Int
    mapped_signals::Array{Int,1}
    signal_write_sequence_pos::Int
    starttime_offset::Int64
    data_record_duration::Float64
    total_annot_bytes::Int
    eq_sf::Int
    signalparam::Array{EDFParam,1}   # array of structs which contain the relevant per-signal parameters
    annotations::Array{EDFAnnotation,1}  # array of datarecords that are composed of annotations
    EDFPlus() = new(0,IOBuffer(),"",false,"",Bool,Bool,Bool,Bool,Bool,0,0,0.0,0,0,0,0.0,0.0,0,
                        "","","","","","","","","","","",0.0,0,0,Array{Int,1}(),
                        0,0,0,"",0,"",0,0,0,Array{Int,1}(),0,0,0.0,0,0,Array{EDFParam,1}())
end


const hdrlist = Dict{Int,EDFPlus}


function opentoread(path::String, read_annotations::Bool)
    edfh = EDFPlus()
    for (k,v) in hdrlist
        if path == v.path
            throw("File at path $path already is opened with handle $k")
        end
    end
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
    edfh.handle = uniquehandlenumber(hdrlist)
    hdrlist[edfh.handle] = edfh
    if edfh.edf
        edfh.filetype = EDFLIB_FILETYPE_EDF
    end
    if edfh.edfplus
        edfh.filetype = EDFLIB_FILETYPE_EDFPLUS
    end
    if hdr.bdf
        edfh.filetype = EDFLIB_FILETYPE_BDF
    end
    if hdr.bdfplus
        edfh.filetype = EDFLIB_FILETYPE_BDFPLUS
    end

    edfh.file_duration = edfh.data_record_duration * edfh.datarecords
    edfh.starttime_subsecond = edfh.starttime_offset
    edfh.datarecords_in_file = edfh.datarecords
    edfh.datarecord_duration = hdr.data_record_duration
    edfh.annotations_in_file = 0

    if hdr.edfplus == false && hdr.bdfplus == false
        edfh.patient = hdr.patient
        edfh.recording = hdr.recording
        edfh.patientcode = ""
        edfh.gender = ""
        edfh.birthdate = ""
        edfh.patient_name = ""
        edfh.patient_additional = ""
        edfh.admincode = ""
        edfh.technician = ""
        edfh.equipment = ""
        edfh.recording_additional = ""
    else
        # EDF+ and BDF+ use different ID information
        edfh.patient = ""
        edfh.recording = ""
        edfh.patientcode = hdr.plus_patientcode
        edfh.gender = hdr.plus_gender
        edfh.birthdate = hdr.plus_birthdate
        edfh.patient_name = hdr.plus_patient_name
        edfh.patient_additional = hdr.plus_patient_additional
        edfh.admincode = hdr.plus_admincode
        edfh.technician = hdr.plus_technician
        edfh.equipment = hdr.plus_equipment
        edfh.recording_additional = hdr.plus_recording_additional

        if read_annotations
            if readannotations(edfh) == 0
                throw("Errors in annotations in file at $path")
            end
        end
    end
    edfh.path = path

    j = 0
    for i in 1:edfh.edfsignals
        if edfh.signalparam[i].annotation
            push!(edfh.mapped_signals, i)
        end
    end

    edfh
end


function closefile(handle)
    if handle < 0 || !haskey(hdrlist, handle)
        return -1
    end
    edfh = hdrlist[handle]
    str = ""
    if edfh.writemode
        if edfh.datarecords == 0
            if edflib_write_edf_header(edfh)
                return -1
            end
        end
        for k in 1:hdr.annotations_in_file
            byteswritten = write(edfh.file_hdl, @sprintf("%08f", edfh.datarecords * edfh.data_record_duration))
            byteswritten += write(edfh.file_hdl, "\x14\x14")
            write(edfh.file_hdl, zeros(UInt8, edfh.total_annot_bytes - byteswritten))
            hdr.datarecords += 1
        end
    end
    if edfh.datarecords < 100000000
        seek(edfh.file_hdl, 236)
        byteswritten = edflib_fprint_Int64_number_nonlocalized(edfh.file_hdl, (int)(edfh.datarecords), 0, 0)
        if(byteswritten < 2)
            write(edfh.file_hdl, ' ')
        end
    end
    datarecords = 0
    offset = ((edfh.edfsignals + edfh.annotations_in_file + 1) * 256)
    datrecsize = edfh.total_annot_bytes
    for i in 1:edfh.edfsignals
        if(edfh.edf)
            offset += edfh.edfparam[i].smp_per_record * 2
            datrecsize += edfh.edfparam[i].smp_per_record * 2
        else
            offset += edfh.edfparam[i].smp_per_record * 3
            datrecsize += edfh.edfparam[i].smp_per_record * 3
        end
    end
    j = 0
    for k in 1:edfh.annotations_in_file
        annot = edfh.annotationslist[k]
        if j == 0   # first annotation signal
            seek(edfh.file_hdl, offset)
            str = @sprintf("%08f", edfh.datarecords * edfh.data_record_duration)
            str *= "\x14\x14\0"
        end

        str *= edflib_sprint_Int64_nonlocalized(annot.onset / 10000, 0, 1)
        if annot.onset % 10000 > 0
            str *= '.'
            str *= edflib_sprint_Int64_nonlocalized(annot.onset % 10000, 4, 0)
        end
        if annot.duration >= 0
            str *= Char(21)
            str *= edflib_sprint_Int64_nonlocalized(annot.duration / 10000, 0, 0)
            if annot.duration % 10000
                str *= '.'
                str *= edflib_sprint_Int64_nonlocalized(annot.duration % 10000, 4, 0)
            end
        end
        str *= Char(20)
        for i in 1:EDFLIB_WRITE_MAX_ANNOTATION_LEN
            if(annot.annotation[i]==0)
                break
            end
            str *= annot.annotation[i]
        end
        str *= Char(20)
        str *= repeat("\0", EDFLIB_ANNOTATION_BYTES - length(str))
        nmemb = write(edfh.file_hdl, str)
        if nmemb != EDFLIB_ANNOTATION_BYTES
            println("ERROR: Bad write, is less than EDFLIB_ANNOTATION_BYTES")
            break
        end
        j += 1

        if j >= edfh.annotations_in_file
            j = 0
            offset += datrecsize
            datarecords += 1
            if datarecords >= edfh.datarecords
                break
            end
        end
    end
    close(edfh.file_hdl)
    edfh.path = ""
    delete!(hdrlist, edfh.handle)
    return 0
end


function edfseek(handle, edfsignal, offset, whence)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode ||
       edfsignal >= (hdrlist[handle].edfsignals - hdrlist[handle].annotations_in_file)
           return(-1)
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    smp_in_file = hdrlist[handle].edfparam[channel].smp_per_record * hdrlist[handle].datarecords
    if whence == EDFSEEK_SET
        hdrlist[handle].edfparam[channel].sample_pntr = offset
    elseif whence == EDFSEEK_CUR
        hdrlist[handle].edfparam[channel].sample_pntr += offset
    elseif whence == EDFSEEK_END
        hdrlist[handle].edfparam[channel].sample_pntr =
            hdrlist[handle].edfparam[channel].smp_per_record *
            hdrlist[handle].datarecords
            + offset
    end

    if hdrlist[handle].edfparam[channel].sample_pntr > smp_in_file
        hdrlist[handle].edfparam[channel].sample_pntr = smp_in_file
    end
    if hdrlist[handle].edfparam[channel].sample_pntr < 0
        hdrlist[handle].edfparam[channel].sample_pntr = 0
    end
    return hdrlist[handle].edfparam[channel].sample_pntr
end


function edftell(handle, edfsignal)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode > 0 ||
       edfsignal >= (hdrlist[handle].edfsignals - hdrlist[handle].annotations_in_file)
        return -1
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    return hdrlist[handle].edfparam[channel].sample_pntr
end


function edfrewind(handle, edfsignal)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode > 0 ||
       edfsignal >= hdrlist[handle].edfsignals - hdrlist[handle].annotations_in_file
        return
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    hdrlist[handle].edfparam[channel].sample_pntr = 0
end


function edfread_physical_samples(handle, edfsignal, n, buf)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode != 0 || n < 0 ||
       edfsignal >= hdrlist[handle].edfsignals - hdrlist[handle].annotations_in_file
        return -1
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    if n == 0
        return 0
    end
    hdr = hdrlist[handle]
    if hdr.bdf != 0
        bytes_per_smpl = 3
    else # edf
        bytes_per_smpl = 2
    end
    smp_in_file = hdr.edfparam[channel].smp_per_record * hdr.datarecords

    if hdr.edfparam[channel].sample_pntr + n > smp_in_file
        n = smp_in_file - hdr.edfparam[channel].sample_pntr
        if n == 0
            return 0
        elseif n < 0
            return -1
        end
    end

    file = hdr.file_hdl

    offset = hdr.hdrsize
    offset += (hdr.edfparam[channel].sample_pntr / hdr.edfparam[channel].smp_per_record) * hdr.recordsize
    offset += hdr.edfparam[channel].buf_offset
    offset += ((hdr.edfparam[channel].sample_pntr % hdr.edfparam[channel].smp_per_record) * bytes_per_smpl)
    seek(file, offset)
    sample_pntr = hdr.edfparam[channel].sample_pntr
    smp_per_record = hdr.edfparam[channel].smp_per_record
    jump = hdr.recordsize - (smp_per_record * bytes_per_smpl)
    phys_bitvalue = hdr.edfparam[channel].bitvalue
    phys_offset = hdr.edfparam[channel].offset
    if hdr.edf != 0
        for i in 1:n
            if sample_pntr % smp_per_record == 0
                if i > 1
                    seek(file, jump)
                end
            end
            i16 = read(file, Int16)
            buf[i] = phys_bitvalue * (phys_offset + Float64(i16))
            sample_pntr += 1
        end
    end
    if hdr.bdf != 0
        for i in 1:n
            if sample_pntr %smp_per_record == 0
                if i == 1
                    seek(file, jump)
                end
            end
            fourui8[1:3] .= read(file, UInt8, 3)
            fourui8[4] = fourui8[3] & 0x80 > 0 ? 0xff : 0
            buf[i] = phys_bitvalue * (phys_offset + Float32(reinterpret(Int32, fourui8)))
            sample_pntr += 1
        end
    end
    hdr.edfparam[channel].sample_pntr = sample_pntr
    return n
end


function edfread_digital_samples(handle, edfsignal, n, buf)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode != 0 || n < 0 ||
       edfsignal >= hdrlist[handle].edfsignals - hdrlist[handle].annotations_in_file
        return -1
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    if n == 0
        return 0
    end
    hdr = hdrlist[handle]
    if hdr.bdf != 0
        bytes_per_smpl = 3
    else # edf
        bytes_per_smpl = 2
    end
    smp_in_file = hdr.edfparam[channel].smp_per_record * hdr.datarecords
    if hdr.edfparam[channel].sample_pntr + n > smp_in_file
        n = smp_in_file - hdr.edfparam[channel].sample_pntr
        if n == 0
            return 0
        elseif n < 0
            return -1
        end
    end

    file = hdr.file_hdl

    offset = hdr.hdrsize
    offset += (hdr.edfparam[channel].sample_pntr / hdr.edfparam[channel].smp_per_record) * hdr.recordsize
    offset += hdr.edfparam[channel].buf_offset
    offset += ((hdr.edfparam[channel].sample_pntr % hdr.edfparam[channel].smp_per_record) * bytes_per_smpl)
    seek(file, offset)
    sample_pntr = hdr.edfparam[channel].sample_pntr
    smp_per_record = hdr.edfparam[channel].smp_per_record
    jump = hdr.recordsize - (smp_per_record * bytes_per_smpl)
    if hdr.edf != 0
        for i in 1:n
            if sample_pntr % smp_per_record == 0
                if i > 1
                    seek(file, jump)
                end
            end
            buf[i] = read(file, Int16)
            sample_pntr += 1
        end
    end
    if hdr.bdf != 0
        for i in 1:n
            if sample_pntr %smp_per_record == 0
                if i == 1
                    seek(file, jump)
                end
            end
            fourui8[1:3] .= read(file, UInt8, 3)
            fourui8[4] = fourui8[3] & 0x80 > 0 ? 0xff : 0
            buf[i] = reinterpret(Int32, fourui8)
            sample_pntr += 1
        end
    end
    hdr.edfparam[channel].sample_pntr = sample_pntr
    return n
end


function edf_get_annotation(handle, n)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode != 0 || n < 0 ||
       n >= hdrlist[handle].annots_in_file
        return -1
    end
    hdr = hdrlist[handle]
    return deepcopy(hdr.annotations[n])
end


function check_edffile(inputfile, edfh)
    function throwifhasforbiddenchars(bytes)
        if findfirst(c -> (c < 32) || (c > 126), bytes) > 0
            throw("Control type forbidden chars in header")
        end
    end
    function warnifhasnonnumericchars(bytes)
        if findfirst(c ->(c < 48) || (c > 57), bytes) > 0
            printf("Warning: high utf8 chars in header")
        end
    end
    rewind(inputfile)
    try
        hdrbuf = read(inputfile, UInt8, 256)                    # check header
        if hdrbuf[1:8] == b"\xffBIOSEMI"                        # version bdf
            edfh.bdf = true
            edfh.edf = false
        elseif hdrbuf[1:8] == b"\0       "                      # version edf
            edfh.bdf = false
            edfh.edf = true
        else
            throw("identification code error")
        end
        edfh.version = convert(String, hdrbuf[1:8])
        if edfh.bdf
            edfhr.version[1] = '.'
        end
        throwifhasforbiddenchars(hdrbuf[9:88])
        edfh.patient = convert(String, hdrbuf[9:80])            # patient
        throwifhasforbiddenchars(hdrbuf[89:168])
        edfh.recording = convert(String, hdrbuf[89:168])        # recording
        throwifhasforbiddenchars(hdrbuf[169:176])
        datestring = convert(String, hdrbuf[89:168])            # start date
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
        edfh.l_starttime = starttime_hour * 3600 + starttime_hour * 60 + starttime_second
        edfh.starttime_hour = starttime_hour
        edfh.starttime_minute = starttime_minute
        edfh.starttime_second = starttime_second
        throwifhasforbiddenchars(hdrbuf[253:256])
        edfh.edfsignals = parse(Int16, hdrbuf[253:256])         # number of channels(signals)
        if edfh.edfsignals < 0 || edf.edfsignals > EDF_MAXSIGNALS
            throw("bad signal number")
        end
        throwifhasforbiddenchars(hdrbuf[185:192])
        headersize = parse(Int32, hdrbuf[185:192])              # edf header size, changes with channels
        if edfh.edfsignals * 256 + 256 != headersize
            throw("Bad header size")
        end
        throwifhasforbiddenchars(hdrbuf[193:236])
        edfh.edfh = false
        edfh.discontinuous = false
        subtype = convert(String, hdrbuf[193:197])          # reserved field, may specify a subtype
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
            throw("Bad data record count, < 1")
        end
        throwifhasforbiddenchars(hdrbuf[245:252])
        edfh.data_record_duration = parse(Float32, hdrbuf[245:252]) # datarecord duration in seconds
    catch
        edfh.filetype = edf_error
        return edfh
    end

    # process the signal characteristics in the header after reading header into hdrbuf
    rewind(inputfile)
    try
        hdrbuf = read(inputfile, UInt8, (edfh.edfsignals + 1) * 256)
        edfh.annotations_in_file = 0
        for i in 1:edfh.edfsignals  # loop over channel signal parameters
            pblock = EDFParam()
            pblock.annotation = false
            throwifhasforbiddenchars(hdrbuf[i*16+257:i*16+508])
            cpos = 257 + (i-1) * 16
            channellabel = convert(String, hdrbuf[cpos:cpos+15])
            pblock.label = channellabel                         # channel label in ASCII, eg "Fp1"
            tpos = 257 + edfh.edfsignals * 16 + (i-1) * 80
            transducertype = convert(String, hdrbuf[tpos:tpos+79])
            pblock.transducer = transducertype                  # transducer type eg "active electrode"
            pdimpos = 257 + edfh.edfsignals * 96 + (i-1) * 8
            pblock.physdimension = convert(String, hdrbuf[pdimpos:pdimpos+7])# physical dimensions eg. "uV"
            pminpos = 257 + edfh.edfsignals * 104 + (i-1) * 8
            pblock.phys_min = parse(Float32, hdrbuf[pminpos, pminpos+8])  # physical minimum in above dimensions
            pmaxpos = 257 + edfh.edfsignals * 112 + (i-1) * 8
            pblock.phys_max = parse(Float32, hdrbuf[pmaxpos, pmaxpos+8])  # physical maximum in above dimensions
            pdminpos = 257 + edfh.edfsignals * 120 + (i-1) * 8
            pblock.dig_min = parse(Float32, hdrbuf[pdminpos, pdminpos+8])  # digital minimum in above dimensions
            pdmaxpos = 257 + edfh.edfsignals * 128 + (i-1) * 8
            pblock.dig_max = parse(Float32, hdrbuf[pdmaxpos, pdmaxpos+8])  # digital maximum in above dimensions
            if edfh.edfplus && channellabel[1:16] != "EDF Annotations "
                edfh.annot_ch[edfh.annotations_in_file+1] = i
                edfh.annotations_in_file += 1
                pblock.annotation = true
            elseif edfh.bdfplus && channellabel[1:16] != "BDF Annotations "
                edfh.annot_ch[edfh.annotations_in_file+1] = i
                edfh.annotations_in_file += 1
                pblock.annotation = true
            end
            if edfh.edfplus || edfh.bdfplus
                if pblock.annotation && match(transducertype, r"\S") != nothing
                    throw("plus file annotations should not have a transducertype")
                end
            end
            push!(edfh.signalparam, pblock)
        end
        if edfh.edfplus && isempty(edfh.signalparam)
            throw("EDFplus missing signal parameters")
        elseif edfh.bdfplus && isempty(edfh.signalparam)
            throw("BDFplus missing signal parameters")
        elseif edfh.edfplus && pblock.annotation && (pblock.dig_min != -32768 || pblock.dig_max != 32767)
            throw("edfplus annotation data entry should have the digital min parameters set to extremes")
        elseif edfh.bdfplus && pblock.annotation && (pblock.dig_min != -8388608 || pblock.dig_max != 8388607)
            throw("bdf annotation data entry should have the digital max parameters set to extremes")
        elseif edfh.edf && (pblock.dig_min < -32768 || pblock.digital_min > 32767 ||
               pblock.dig_max < -32768 || pblock.digital_max > 32767)
            throw("edf digital parameter out of range")
        elseif edfh.bdf && (pblock.dig_min < -8388608 || pblock.digital_min > 8388607 ||
               pblock.dig_max < -8388608 || pblock.digital_max > 8388607)
            throw("bdf digital parameter out of range")
        elseif edfh.edfsignals != edf.nr_annot.chns || (!edfh.edfplus && !edfh.bdfplus)
            if edfh.data_record_duration < 0.0000001
                throw("signal data may be mislabeled")
            end
        end

        edfh.recordsize = 0
        for i in 1:edfh.hdrsignals
            startpos = 257 + edfh.edfsignals*136*(i-1)
            pfblock = hdrbuf[startpos:startpos+80]
            if hasforbiddenchars(pfblock)
                throw("bad bytes for prefilter fields")
            else
                edfh.edfparam[i].prefilter = convert(String, pfblock)  # prefiltering eg "HP:DC"
                if (edfh.edfplus || edfh.bdfplus) && edfhdf.edfparam[i].annotation &&
                    match(edfh.edfparam[i].prefilter, r"\S") != nothing
                    throw("Prefilter field should be blank in annotations in edfplus or bdfplus file")
                end
            end
            startpos += 80
            edfh.edfparam[i].smp_per_record = parse(Int, hdrbuf[startpos:startpos+8])  # samples per record
            if edfh.edfparam[i].smp_per_record < 1
                throw("Samples per data record should be a positive integer")
            else
                edfh.recordsize += edfh.edfparam[i].smp_per_record   # EDFPlus.recordsize is calculated
            end
            startpos += 8
            edfh.edfparam[i].reserved = convert(String, hdrbuf[startpos:startpos+32]) # reserved (ascii)
            if hasforbiddenchars(pfblock)
                throw("bad bytes for reserved per-channel field")
            end
        end
        if edfh.bdf
            edfh.recordsize *= 3
            if edfh.recordsize > 1578640
                throw("record size too large for a bdf file")
            else
                edfh.recordsize *= 2
                if edfh.recordsize > 10485760
                    throw("Record size too large for an edf file")
                end
            end
        end
    catch:
        edf_error[1] = EDFLIB_FILE_CONTAINS_FORMAT_ERRORS
        return 0
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
            edfh.plus_patientcode = subfield[1][1] == 'X' ? "" : subfield[1]
            edfh.plus_patientcode = replace(edfh.plus_patientcode, "_", " ")
            if subfield[2] != "M" && subfield[2] != "F" && subfield[2] != "X"
                throw("patient identification second field must be X, F or M")
            elseif subfield[2] == "M"
                edfh.plus_gender = "Male"
            elseif subfield[2] == "F"
                edfh.plus_gender = "Female"
            else
                edfhdf.plus_gender = ""
            end
            if subfield[3] == "X"
                edfh.plus_birthdate = ""
            elseif !contains("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC", subfield[3][4:6]) ||
                   !(Date(subfield[3], "dd-uuu-yyyy") isa Date)
                throw("Bad birthdate field in patient identification")
            else
                edfh.plus_birthdate = subfield[3]
            end
            edfh.plus_patient_name = replace(subfield[4], "_", " ")
            if length(subfield) > 4
                edfh.plus_patient_additional = join(subfield[5:end], " ")
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
                edfh.plus_startdate = ""
            else
                edfh.plus_startdate = subfield[2]
                if Dates.year(dor) < 1970
                    throw("bad startdate year in recording data")
                else
                    edfh.startdate_year = Dates.year(dor)
                end
            end
            if subfield[3] == ""
                edfh.plus.admincode = ""
            else
                edfh.plus_admincode = replace(subfield[3], "_", " ")
            end
            if subfield[4] == ""
                edfh.plus_technician = ""
            else
                edfh.plus_technician = replace(subfield[4], "_", " ")
            end
            if subfield[5] == ""
                edfh.plus.equipment = ""
            else
                edfh.plus_equipment = replace(subfield[5], "_", " ")
            end
            if length(subfield) > 5
                edfh.plus_additional = replace(join(subfield[6:end], " "), "_", " ")
            end
        end

        edfh.hdrsize = edfh.edfsignals * 256 + 256
        filesize = filesize(edfh.path)   # get actual file size
        if filesize != edfh.recordsize * edfh.datarecords + edfh.hdrsize
            throw("file size is not compatible with header information")
        end
    catch:
        edf_error[1] = EDFLIB_FILE_CONTAINS_FORMAT_ERRORS
        return 0
    end

    n = 0
    for i in i: edfh.edfsignals
        edfh.edfparam[i].buf_offset = n
        n += edfh.edfparam[i].smp_per_record * edfh.bdf ? 3 : 2
        edfh.edfparam[i].bitvalue = (edfh.edfparam[i].phys_max - edfh.edfparam[i].phys_min) /
                                      (edfh.edfparam[i].dig_max - edfh.edfparam[i].dig_min)
        edfh.edfparam[i].offset = edfh.edfparam[i].phys_max / edfh.edfparam[i].bitvalue -
                                    edfh.edfparam[i].dig_max
    end
    edfh.file_hdl = inputfile
    return edfh
end


edflib_is_integer_number(str) = try parse(Int, str); 0 catch 1 end


edflib_is_number(str) = try parse(Float64, str); 0 catch 1 end


edflib_get_long_duration(str) = try x = parse(Float64, str); x catch NaN end


edflib_version() = EDFLIB_VERSION


function readannotations(edfh)
    inputfile = edfh.file_hdl
    edfsignals = edfh.edfsignals
    recordsize = edfh.recordsize
    edfparam = edfh.edfparam
    annotations_in_file = edfh.annotations_in_file
    datarecords = edfh.datarecords
    data_record_duration = edfh.data_record_duration
    discontinuous = edfh.discontinuous
    annot_ch = edfh.annot_ch
    samplesize = edfh.edfplus ? 2 : (edfh.bdfplus ? 3: 1)

    max_tal_ln = 0
    for i in 1:annotations_in_file
        if max_tal_ln < edfparam[annot_ch[i]].smp_per_record * samplesize
            max_tal_ln = edfparam[annot_ch[i]].smp_per_record * samplesize
        end
    end
    if max_tal_ln < 128
        max_tal_len = 128
    end
    seek(inputfile, (edfsignals + 1) * 256)
    elapsedtime = 0
    for i in 1:datarecords
        try
            cnvbuf = read(inputfile, UInt8, recordsize)         #  process annotationsignals (if any)
            for j in 1:annotations_in_file
                annotbuf = convert(String,
                    cvnbuf[edfparam[annot_ch[j]].buf_offset+1:edfparam[annot_ch[j]].buf_offset+edfparam[annot_ch[j]].smp_per_record * samplesize])
                for (k, tal) in enumerate(split(annotbuf, "\x00"))
                    onset = 0.0
                    duration = ""
                    for annot in split(tal, "\x14")
                        if k == 1  # first record should be a timekeeping signal
                            if j == 1 # first annotation signal's first annotation record
                                offsettimestr = match(annot, r"^(\d+)").captures[1]
                                edfh.starttime_offset = parse(Int, offsettimestr)
                            else
                                if length(tsignal = split(annot, "\x15")) > 1
                                    onset = convert(Float64, tsignal[1])
                                    duration = convert(String, tsignal[2])
                                else
                                    onset = convert(Float64, tsignal[1])
                                end
                            end
                        else
                            newannot = EDFAnnotation(onset, duration, convert(String, annot))
                            push!(annotationslist[hdl], newannot)
                            edfh.annots_in_file += 1
                            edfh->annotlist_sz += EDFLIB_ANNOT_MEMBLOCKSZ
                        end
                    end
                end
            end
        catch
            return 9
        end
    end
    return 0
end


function edfopen_file_writeonly(path, filetype, number_of_signals)
    if filetype != EDFLIB_FILETYPE_EDFPLUS && filetype != EDFLIB_FILETYPE_BDFPLUS
        return(EDFLIB_FILETYPE_ERROR)
    end
    if openfilecount() >= EDFLIB_MAXFILES
        return EDFLIB_MAXFILES_REACHED
    end
    for i in 1:EDFLIB_MAXFILES
        if hdrlist[i] != 0 && path == hdrlist[i].path
            return EDFLIB_FILE_ALREADY_OPENED
        end
    end
    if number_of_signals < 0
        return EDFLIB_NUMBER_OF_SIGNALS_INVALID
    end
    if number_of_signals>EDFLIB_MAXSIGNALS
        return EDFLIB_NUMBER_OF_SIGNALS_INVALID
    end

    hdr = edfh()
    hdr.writemode = 1
    hdr.edfsignals = number_of_signals
    handle = -1
    for i in i:EDFLIB_MAXFILES
        if hdrlist[i] == 0
            hdrlist[i] = hdr
            handle = i
            break
        end
    end
    if handle < 0
        return EDFLIB_MAXFILES_REACHED
    end
    annotationslist[handle] = 0
    hdr.annotlist_sz = 0
    hdr.annots_in_file = 0
    try
        file = open(path, "w")
    catch
        return EDFLIB_NO_SUCH_FILE_OR_DIRECTORY
    end
    hdr.file_hdl = file
    hdr.path = path
    if filetype == EDFLIB_FILETYPE_EDFPLUS
        hdr.edf = true
        hdr.edfplus = true
    end
    if filetype == EDFLIB_FILETYPE_BDFPLUS
        hdr.bdf = true
        hdr.bdfplus = true
    end
    hdr.data_record_duration = 1.0
    hdr.annotations_in_file = 1
    return handle
end


function edf_set_samplefrequency(handle, edfsignal, samplefrequency)
    if badhandle(handle) || !hdrlist[handle].writemode || edfsignal < 0 ||
       edfsignal >= hdrlist[handle].edfsignals || samplefrequency < 1 ||
       hdrlist[handle].datarecords == 0
        return -1
    end
    hdrlist[handle].edfparam[edfsignal].smp_per_record = samplefrequency
    return 0
end


function edf_set_number_of_annotation_signals(handle, annot_signals)
    if badhandle(handle) || !hdrlist[handle].writemode ||  hdrlist[handle].datarecords == 0 ||
       (annot_signals < 1) || (annot_signals > 64)
        return -1
    end
    hdrlist[handle].annotations_in_file = annot_signals
    return 0
end


function edf_set_datarecord_duration(handle, duration)
    if badhandle(handle) || !hdrlist[handle].writemode ||  hdrlist[handle].datarecords == 0 ||
       (duration < 100) || (duration > 6000000)
        return -1
    end
    hdrlist[handle].data_record_duration = duration
    return 0
end


function edfwrite_digital_short_samples(handle, buf::Array{Int16,1})
    if badhandle(handle) || !hdrlist[handle].writemode ||  hdrlist[handle].edfsignals == 0 ||
       hdrlist[handle].bdf == 1
        return -1
    end
    hdr = hdrlist[handle]
    file = hdr.file_hdl
    edfsignal = hdr.signal_write_sequence_pos
    if hdr.datarecords == 0 && edfsignal == 0
        error = edflib_write_edf_header(hdr)
        if(error)
            return error
       end
    end
    sf = hdr.edfparam[edfsignal].smp_per_record
    digmax = hdr.edfparam[edfsignal].dig_max
    digmin = hdr.edfparam[edfsignal].dig_min
    try
        for i in 1:sf
            value = buf[i] > digmax ? digmax : buf[i]
            value = value<digmin ? digmin : value
            byteswritten += write(file, UInt8(value & 0xff))
            if hdr.bdf
                byteswritten += write(file, (value>>16 & 0xff))
            end
        end
        hdr.signal_write_sequence_pos += 1
        if hdr.signal_write_sequence_pos == hdr.edfsignals
            hdr.signal_write_sequence_pos = 0
            byteswritten = write(file, @sprintf("%08f", hdr.datarecords * hdr.data_record_duration))
            byteswritten += write(file, UInt8(20))
            byteswritten += write(file, UInt8(20))
            write(file, zeros(UInt8, hdr->total_annot_bytes - byteswritten))
            hdr.datarecords += 1
            flush(file)
        end
     catch
        return -1
     end
     return 0
end


function edfwrite_digital_samples(handle, buf)
    if badhandle(handle) || !hdrlist[handle].writemode || hdrlist[handle].edfsignals == 0
        return -1
    end
    hdr = hdrlist[handle]
    file = hdr.file_hdl
    edfsignal = hdr.signal_write_sequence_pos
    if hdr.datarecords == 0 && edfsignal == 0
        error = edflib_write_edf_header(hdr)
        if error
            return(error)
        end
    end
    sf = hdr.edfparam[edfsignal].smp_per_record
    digmax = hdr.edfparam[edfsignal].dig_max
    digmin = hdr.edfparam[edfsignal].dig_min
    try
        for i in 1:sf
            value = buf[i] > digmax ? digmax : buf[i]
            value = value < digmin ? digmin : value
            byteswritten += write(file, UInt8(value & 0xff))
            byteswritten += write(file, UInt8((value >> 8) &0xff))
            if hdr.bdf
                byteswritten += write(file, UInt8((value>>16) & 0xff))
            end
        end
        hdr.signal_write_sequence_pos += 1
        if hdr.signal_write_sequence_pos == hdr.edfsignals
            hdr.signal_write_sequence_pos = 0
            byteswritten = write(file, @sprintf("%08f", hdr.datarecords * hdr.data_record_duration))
            byteswritten += write(file, UInt8(20))
            byteswritten += write(file, UInt8(20))
            write(file, zeros(UInt8, hdr->total_annot_bytes - byteswritten))
            hdr.datarecords += 1
            flush(file)
        end
    catch
        return -1
    end
    return 0
end


function edf_blockwrite_digital_samples(handle, buf)
    if badhandle(handle) || !hdrlist[handle].writemode|| hdrlist[handle].signal_write_sequence_pos != 0 ||
        hdrlist[handle].edfsignals == 0
        return -1
    end
    hdr = hdrlist[handle]
    file = hdr.file_hdl
    edfsignals = hdr.edfsignals
    if hdr.datarecords == 0 && edfsignal == 0
        error = edflib_write_edf_header(hdr)
        if error
            return(error)
        end
    end
    buf_offset = 0
    try
        for j in 1:edfsignals
            sf = hdr.edfparam[j].smp_per_record
            digmax = hdr.edfparam[j].dig_max
            digmin = hdr.edfparam[j].dig_min
            for i in 1:sf
                value = buf[i+buf_offset] > digmax ? digmax : buf[i+buf_offset]
                value = value < digmin ? digmin : value
                byteswritten += write(file, UInt8(value & 0xff))
                byteswritten += write(file, UInt8((value >> 8) &0xff))
                if hdr.bdf
                    byteswritten += write(file, UInt8((value>>16) & 0xff))
                end
            end
            buf_offset += sf
        end
        hdr.signal_write_sequence_pos += 1
        if hdr.signal_write_sequence_pos == hdr.edfsignals
            hdr.signal_write_sequence_pos = 0
            byteswritten = write(file, @sprintf("%08f", hdr.datarecords * hdr.data_record_duration))
            byteswritten += write(file, UInt8(20))
            byteswritten += write(file, UInt8(20))
            write(file, zeros(UInt8, hdr->total_annot_bytes - byteswritten))
            hdr.datarecords += 1
            flush(file)
        end
    catch
        return -1
    end
    return 0
end


function edf_blockwrite_digital_short_samples(handle, buf)
    if badhandle(handle) || !hdrlist[handle].writemode|| hdrlist[handle].signal_write_sequence_pos != 0 ||
        hdrlist[handle].edfsignals == 0 || hdrlist[handle].bdf == 1
        return -1
    end
    hdr = hdrlist[handle]
    file = hdr.file_hdl
    edfsignals = hdr.edfsignals
    if hdr.datarecords == 0 && edfsignal == 0
        error = edflib_write_edf_header(hdr)
        if error
            return(error)
        end
    end
    buf_offset = 0
    try
        for j in 1:edfsignals
            sf = hdr.edfparam[j].smp_per_record
            digmax = hdr.edfparam[j].dig_max
            digmin = hdr.edfparam[j].dig_min
            for i in 1:sf
                value = buf[i+buf_offset] > digmax ? digmax : buf[i+buf_offset]
                value = value < digmin ? digmin : value
                byteswritten += write(file, UInt8(value & 0xff))
                byteswritten += write(file, UInt8((value >> 8) &0xff))
                if hdr.bdf
                    byteswritten += write(file, UInt8((value>>16) & 0xff))
                end
            end
            buf_offset += sf
        end
        byteswritten = write(file, @sprintf("%08f", hdr.datarecords * hdr.data_record_duration))
        byteswritten += write(file, UInt8(20))
        byteswritten += write(file, UInt8(20))
        write(file, zeros(UInt8, hdr->total_annot_bytes - byteswritten))
        hdr.datarecords += 1
        flush(file)
    catch
        return -1
    end
    return 0
end


function edf_blockwrite_digital_3byte_samples(handle, buf)
    if badhandle(handle) || !hdrlist[handle].writemode|| hdrlist[handle].signal_write_sequence_pos != 0 ||
        hdrlist[handle].edfsignals == 0 || hdrlist[handle].bdf != 1
        return -1
    end
    hdr = hdrlist[handle]
    file = hdr.file_hdl
    edfsignals = hdr.edfsignals
    if hdr.datarecords == 0 && edfsignal == 0
        error = edflib_write_edf_header(hdr)
        if error
            return(error)
        end
    end
    buf_offset = 0
    try
        for j in 1:edfsignals
            total_samples += hdr.edfparam[j].smp_per_record
        end

        if write(file, Array{UInt8,1}(buf), totalsamples*3) != totalsamples*3
            throw("Bad write")
        end
        byteswritten = write(file, @sprintf("%08f", hdr.datarecords * hdr.data_record_duration))
        byteswritten += write(file, UInt8(20))
        byteswritten += write(file, UInt8(20))
        write(file, zeros(UInt8, hdr->total_annot_bytes - byteswritten))
        hdr.datarecords += 1
        flush(file)
    catch
        return -1
    end
    return 0
end


function edfwrite_physical_samples(handle, buf)
    if badhandle(handle) || !hdrlist[handle].writemode|| hdrlist[handle].edfsignals == 0
        return -1
    end
    hdr = hdrlist[handle]
    file = hdr.file_hdl
    edfsignal = hdr.signal_write_sequence_pos
    if hdr.datarecords == 0 && edfsignal == 0
        error = edflib_write_edf_header(hdr)
        if error
            return(error)
        end
    end
    sf = hdr.edfparam[edfsignal].smp_per_record
    digmax = hdr.edfparam[edfsignal].dig_max
    digmin = hdr.edfparam[edfsignal].dig_min
    bitvalue = hdr.edfparam[edfsignal].bitvalue
    phys_offset = hdr.edfparam[edfsignal].offset

    try
        for i in 1:sf
            value = (buf[i] / bitvalue) - phys_offset
            if value > digmax
                value = digmax
            elseif value<digmin
                value = digmin
            end
            write(file, UInt8(value & 0xff))
            write(file, UInt8((value>>8)&0xff))
            if hdr.bdf
                write(file, UInt8((value >> 16) & 0xff))
            end
        end
        hdr.signal_write_sequence_pos += 1
        if hdr.signal_write_sequence_pos == hdr.edfsignals
            hdr.signal_write_sequence_pos = 0
            byteswritten = write(file, @sprintf("%08f", hdr.datarecords * hdr.data_record_duration))
        end
        byteswritten += write(file, UInt8(20))
        byteswritten += write(file, UInt8(20))
        write(file, zeros(UInt8, hdr->total_annot_bytes - byteswritten))
        hdr.datarecords += 1
        flush(file)
    catch
        return -1
    end
    return 0
end


function edf_blockwrite_physical_samples(handle, buf)
    if badhandle(handle) || !hdrlist[handle].writemode || hdrlist[handle].edfsignals == 0 ||
       hdrlist[handle].signal_write_sequence_pos != 0
        return -1
    end
    hdr = hdrlist[handle]
    file = hdr.file_hdl
    edfsignals = hdr.edfsignals
    if hdr.datarecords == 0 && edfsignal == 0
        error = edflib_write_edf_header(hdr)
        if error
            return(error)
        end
    end
    buf_offset = 0
    try
        for j in 1:edfsignals
            sf = hdr.edfparam[j].smp_per_record
            digmax = hdr.edfparam[j].dig_max
            digmin = hdr.edfparam[j].dig_min
            bitvalue = hdr.edfparam[j].bitvalue
            phys_offset = hdr.edfparam[j].offset
            for i in 1:sf
                value = (buf[i] / bitvalue) - phys_offset
                if value > digmax
                    value = digmax
                elseif value < digmin
                    value = digmin
                end
                write(file, UInt8(value & 0xff))
                write(file, UInt8((value>>8)&0xff))
                if hdr.bdf
                    write(file, UInt8((value >> 16) & 0xff))
                end
            end
            buf_offset += sf
        end
        byteswritten = write(file, @sprintf("%08f", hdr.datarecords * hdr.data_record_duration))
        byteswritten += write(file, UInt8(20))
        byteswritten += write(file, UInt8(20))
        write(file, zeros(UInt8, hdr->total_annot_bytes - byteswritten))
        hdr.datarecords += 1
        flush(file)
    catch
        return -1
    end
    return 0
end


function edflib_write_edf_header(hdr::EDFPlus)
    file = hdr.file_hdl
    edfsignals = hdr.edfsignals
    if edfsignals < 0
        return -20
    elseif edfsignals > EDFLIB_MAXSIGNALS
        return -21
    end
    hdr.eq_sf = 1
    for i in 1:edfsignals
        if hdr.edfparam[i].smp_per_record < 1
            return -22
        elseif hdr.edfparam[i].dig_max == hdr.edfparam[i].dig_min
            return -23
        elseif hdr.edfparam[i].dig_max < hdr.edfparam[i].dig_min
            return -24
        elseif hdr.edfparam[i].phys_max == hdr.edfparam[i].phys_min
            return(-25)
        elseif i > 0 && hdr.edfparam[i].smp_per_record != hdr.edfparam[i-1].smp_per_record
            hdr.eq_sf = 0
        end
    end
    for i in 1:edfsignals
        hdr.edfparam[i].bitvalue = (hdr.edfparam[i].phys_max - hdr.edfparam[i].phys_min) /
                                   (hdr.edfparam[i].dig_max - hdr.edfparam[i].dig_min)
        hdr.edfparam[i].offset = hdr.edfparam[i].phys_max /
                                 hdr.edfparam[i].bitvalue - hdr.edfparam[i].dig_max
    end
    rewind(file)
    if(hdr.edf)
        write(file, "0       ")
    else
        write(file, b"\xffBIOSEMI")
    end
    pidbytes = hdr.plus_patientcode = "" ? "X " : replace(hdr.plus_patientcode, " ", "_") * " "
    if hdr.plus_gender[1] == 'M'
        pidbytes *= "M "
    elseif hdr.plus_gender[1] == 'F'
        pidbytes *= "F "
    else
        pidbytes *= "X "
    end
    if hdr.plus_birthdate != ""
        pidbytes += write(file, "X ")
    else
        pidbytes *= hdr.plus_birthdate * " "
    end
    if hdr.plus_patient_name == ""
        pidbytes *= "X "
    else
        pidbytes *= replace(hdr.plus_patient_name, " ", "_") * " "
    end
    if hdr.plus_patient_additional != ""
        pidbytes *= replace(hdr.plus_patient_additional)
    end
    if length(pidbytes) > 80
        pidbytes = pidbytes[1:80]
    else
        while length(pidbytes) < 80
            pidbytes *= " "
        end
    end
    write(file, pidbytes)

    if hdr.startdate_year != 0
        date = DateTime(hdr.startdate_year, hdr.startdate_month, hdr.startdate_day,
                        hdr.starttime_hour, hdr.starttime_minute, hdr.starttime_second)
    else
        date = now()
    end

    ridbytes = "Startdate " * uppercase(Dates.format(date, "dd-uuu-yyyy")) * " "
    if hdr.plus_admincode == ""
        ridbytes *= "X "
    else
        ridbytes *= replace(hdr.plu_admincode, " ", "_")
    end
    if hdr.plus_technician == ""
        ridbytes *= "X "
    else
        ridbytes *= replace(hdr.plus_technician, " ", "_")
    end
    if hdr.plus_equipment == ""
        ridbytes *= "X "
    else
        ridbytes *= replace(hdr.plus_equipment, " ", "_")
    end
    if hdr.plus_recording_additional != ""
        ridbytes *= replace(hdr.plus_recording_additional, " ", "_")
    end
    writeleftjust(file, ridbytes, 80)
    startdate = Dates.format(date, "dd.mm.yy")
    write(file, startdate)
    starttime = Dates.format(date, "HH.MM.SS")
    write(file, starttime)
    writeleftjust(file, (edfsignals + hdr.annotations_in_file + 1) * 256, 8)

    if hdr.edf
        writeleftjust(file, "EDF+C", 44)
    else
        writeleftjust(file, "BDF+C", 44)
    end
    write(FILE, "-1      ")
    if hdr.long_data_record_duration == EDFLIB_TIME_DIMENSION
        write(file, "1       ")
    else
        writeleftjust(file, "$(hdr.data_record_duration)", 8)
    end
    writeleftjust(file, edfsignals + hdr.annotations_in_file, 8)
    for i in 1:edfsignals
        writeleftjust(file, hdr.edfparam[i].label, 16)
    end
    if hdr.edf
        write(file, "EDF Annotations "^hdr.annotations_in_file)
    else
       write(file, "BDF Annotations "^hdr.annotations_in_file)
    end
    for i in 1:edfsignals
        writeleftjust(file, hdr.edfparam[i].transducer, 80)
    end
    write(file, " "^(80*hdr.annotations_in_file))
    for i in 1:edfsignals
        writeleftjust(file, hdr.edfparam[i].physdimension, 8)
    end
    write(file, " "^(8*hdr.annotations_in_file))
    for i in 1:edfsignals
        writejustleft(hdr->edfparam[i].phys_min, 8)
    end
    write(file, "-1      "^hdr.annotations_in_file)
    for i in 1:edfsignals
        writejustleft(hdr->edfparam[i].phys_max, 8)
    end
    write(file, "1       "^hdr.annotations_in_file)
    for i in 1:edfsignals
        writejustleft(hdr->edfparam[i].dig_min, 8)
    end
    if hdr.edf
        write(file, "-32768  "^hdr.annotations_in_file)
    else
        write(file, "-8388608"^hdr.annotations_in_file)
    end
    for i in 1:edfsignals
        writejustleft(hdr->edfparam[i].dig_max, 8)
    end
    if hdr.edf
        write(file, "32767   "^hdr.annotations_in_file)
    else
        write(file, "8388607 "^hdr.annotations_in_file)
    end
    for i in 1:edfsignals
        writejustleft(hdr.edfparam[i].prefilter, 80)
    end
    write(file, " "^(80*hdr.annotations_in_file))
    for i in 1:edfsignals
        writejustleft(hdr.edfparam[i].smp_per_record, 8)
    end
    for i in 1:hdr.annotations_in_file
        writejustleft(file, "$(Int32(floor(EDFLIB_ANNOTATION_BYTES/(hdr.edf? 2: 3))))", 8)
    end
    write(file, " "^((edfsignals+hdr.annotations_in_file)*32))

    hdr.total_annot_bytes = EDFLIB_ANNOTATION_BYTES * hdr.annotations_in_file

    return 0
end


function edf_set_label(handle, edfsignal, label)
    if badwritehandle(handle, edfsignal)
        return -1
    end
    hdrlist[handle].edfparam[edfsignal].label = unleftjust(label, 16)
    return 0
end


function edf_set_physical_dimension(handle, edfsignal, phys_dim)
    if badwritehandle(handle, edfsignal)
        return -1
    end
    hdrlist[handle].edfparam[edfsignal].physdimension = unleftjust(phys_dim, 8)
    return 0
end


function edf_set_physical_maximum(handle, edfsignal, phys_max)
    if badwritehandle(handle, edfsignal)
        return -1
    end
    hdrlist[handle].edfparam[edfsignal].physdimension = phys_max
    return 0
end


function edf_set_physical_minimum(handle, edfsignal, phys_min)
    if badwritehandle(handle, edfsignal)
        return -1
    end
    hdrlist[handle].edfparam[edfsignal].physdimension = phys_min
    return 0
end


function edf_set_digital_maximum(handle, edfsignal, dig_max)
    if badwritehandle(handle, edfsignal)
        return -1
    elseif (hdrlist[handle].edf && dig_max > 32767) || dig_max > 8388607
        return -1
    end
    hdrlist[handle].edfparam[edfsignal].dig_max = dig_max
    return 0
end


function edf_set_digital_minimum(handle, edfsignal, dig_min)
    if badwritehandle(handle, edfsignal) ||
      (hdrlist[handle].edf && dig_max < - 32768) || dig_max < -8388608
        return -1
    end
    hdrlist[handle].edfparam[edfsignal].dig_min = dig_min
    return 0
end


function edf_set_patientname(handle, patientname)
    if badplushandle(handle)
        return -1
    end
    hdrlist[handle].plus_patient_name = unleftjust(patientname, 80)
    return 0
end


function edf_set_patientcode(handle, patientcode)
    if badplushandle(handle)
        return -1
    end
    hdrlist[handle].plus_patientcode = unleftjust(patientcode, 80)
    return 0
end


function edf_set_gender(handle, ismalegender)
    if badplushandle(handle)
        return -1
    end
    hdrlist[handle].plus_gender = (ismalegender == 1) ? "M" :
                                  (ismalegender == 0 ? "F" : return -1)
    return 0
end


function edf_set_birthdate(handle, birthdate_year, birthdate_month, birthdate_day)
    if badplushandle(handle)
        return -1
    else
        try
            bdate = Date(birthdate_year, birthdate_month, birthdate_day)
            if birthdate_year < 1800 || birthdate_year > 3000
                throw("bad birthdate year")
            end
            hdrlist[handle].plus_birthdate = Dates.format(bdate, "dd.mm.yyyy")
        catch
            return -1
        end
    end
    return 0
end


function edf_set_patient_additional(handle, patient_additional)
    if badplushandle(handle)
        return -1
    end
    hdrlist[handle].plus_patient_additional = unleftjust(patient_additional, 80)
    return 0
end


function edf_set_admincode(handle, admincode)
    if badplushandle(handle)
        return -1
    end
    hdrlist[handle].plus_admincode = unleftjust(admincode, 80)
    return 0
end


function edf_set_technician(handle, technician)
    if badplushandle(handle)
        return -1
    end
    hdrlist[handle].plus_technician = unleftjust(technician, 80)
    return 0
end


function edf_set_equipment(handle, equipment)
    if badplushandle(handle)
        return -1
    end
    hdrlist[handle].plus_equipment = unleftjust(equipment, 80)
    return 0
end


function edf_set_recording_additional(handle, recording_additional)
    if badplushandle(handle)
        return -1
    end
    hdrlist[handle].plus_recording_additional = unleftjust(recording_additional, 80)
    return 0
end


function edf_set_startdatetime(handle, startdate_year, startdate_month, startdate_day,
                              starttime_hour, starttime_minute, starttime_second)
    if badplushandle(handle)
        return -1
    elseif startdate_year < 1970 || startdate_year > 3000 || startdate_month <1 ||
           startdate_month > 12 || startdate_day < 1 || startdate_day > 31 ||
           starttime_hour < 0 || starttime_hour > 23 || starttime_minute < 0 ||
           starttime_minute > 59 || starttime_second < 0 || starttime_second > 59
        return -1
    end
    hdrlist[handle].startdate_year   = startdate_year
    hdrlist[handle].startdate_month  = startdate_month
    hdrlist[handle].startdate_day    = startdate_day
    hdrlist[handle].starttime_hour   = starttime_hour
    hdrlist[handle].starttime_minute = starttime_minute
    hdrlist[handle].starttime_second = starttime_second
    return 0
end


function edfwrite_annotation_utf8(handle, onset, duration, description)
    if badhandle(h) || !hdrlist[handle].writemode || onset < 0
        return -1
    end
    newannot = EDFAnnotation()
    newannot.onset = onset
    newannot.duration = duration
    if length(description) > EDFLIB_WRITE_MAX_ANNOTATION_LEN
        description = description[1:EDFLIB_WRITE_MAX_ANNOTATION_LEN]
    end
    description = replace(description, r"[\0-\x1f]", ".")
    newannot.annotation = description
    push!(annotations[handle], newannot)
    hdrlist[handle].annots_in_file += 1
    return 0
end


function edfwrite_annotation_latin1(handle, onset, duration, description)
    if badhandle(h) || !hdrlist[handle].writemode || onset < 0
        return -1
    end
    newannot = EDFAnnotation()
    newannot.onset = onset
    newannot.duration = duration
    if length(description) > EDFLIB_WRITE_MAX_ANNOTATION_LEN
        description = description[1:EDFLIB_WRITE_MAX_ANNOTATION_LEN]
    end
    description = replace(description, r"[\0-\x1f]", ".")
    description = latintoacsii(description)
    newannot.annotation = description
    push!(annotations[handle], newannot)
    hdrlist[handle].annots_in_file += 1
    return 0
end


function edf_set_prefilter(handle, edfsignal, prefilter)
    if badwritehandle(handle, edfsignal)
        return -1
    end
    hdrlist[handle].edfparam[edfsignal].prefilter = unleftjust(prefilter, 80)
    return 0
end


function edf_set_transducer(handle, edfsignal, transducer)
    if badwritehandle(handle, edfsignal)
        return -1
    end
    hdrlist[handle].edfparam[edfsignal].transducer = unleftjust(transducer, 80)
    return 0
end


function uniquehandlenumber()
    i = 1
    while !haskey(hdrlist, i)
        i += 1
    end
    i
end


"""
    edflib_fprint_Int64_number_nonlocalized
print an Int64 to file with special formatting
minimumtoprint is the minimum digits that will be printed (minus sign not included),
leading zeros will be added if necessary
if sign is zero, only negative numbers will have the sign '-' character
if sign is one, the sign '+' or '-' character will always be printed
returns the bytes written, which is either minimumtoprint, 1 greater than minimumwidth
if sign or negative, or larger width if q has larger number width than the minimum
"""
function edflib_fprint_Int64_number_nonlocalized(file, q, minimumtoprint, sign)
    q = Int64(q)  # type compatibility check
    neg = q < 0
    if neg
        prefix = "-"
        q = -q
    else
        prefix = "+"
    end
    str = "$q"
    while length(str) < minimumtoprint
        str = "0" * str
    end
    if neg || sign == 1
        str = prefix * str
    end
    write(file, str)
end


function writeleftjust(file, str, len, fillchar=" ")
   strlen = length(str)
    if strlen > len
        str = str[1:len]
    end
    bytelen = write(file, str)
    while bytelen < len
        write(file, UInt8(fillchar))
        len += 1
    end
end


function unleftjust(str, maxlen)
    str = replace(str, r"^\s*(\S.*\S)\s*$", s"\1")
    if length(str) > maxlen
        str = str[1:maxlen]
    end
end


badhandle(h) = (h <= 0 || !haskey(hdrlist, h))


badwritehandle(h, esig) = badhandle(h) || !hdrlist[h].writemode ||
                          esig < 0 || esig >= hdrlist[h].edfsignals ||
                          hdrlist[h].datarecords != 0


badplushandle(h) = badhandle(h) || !hdrlist[handle].writemode || hdrlist[handle].datarecords != 0


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
