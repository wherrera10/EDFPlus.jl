#=
Created on Dec 6 2015
Translation to Julia by William Herrera of van Beelen's C library edflib
EEG file routines for EDF, BDF, EDF+, and BDF+ files
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
=================================================================================#

const EDFLIB_MAXSIGNALS =                 512
const EDFLIB_MAX_ANNOTATION_LEN =         512

const EDFSEEK_SET =                         0
const EDFSEEK_CUR =                         1
const EDFSEEK_END =                         2

# the following defines are used in the member "filetype" of the edf_hdr_struct
# and as return value for the function edfopen_file_readonly()
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

# values for annotations
const EDFLIB_DO_NOT_READ_ANNOTATIONS      = 0
const EDFLIB_READ_ANNOTATIONS             = 1
const EDFLIB_READ_ALL_ANNOTATIONS         = 2

# the following defines are possible errors returned by edfopen_file_writeonly()
const EDFLIB_NO_SIGNALS                 = -20
const EDFLIB_TOO_MANY_SIGNALS           = -21
const EDFLIB_NO_SAMPLES_IN_RECORD       = -22
const EDFLIB_DIGMIN_IS_DIGMAX           = -23
const EDFLIB_DIGMAX_LOWER_THAN_DIGMIN   = -24
const EDFLIB_PHYSMIN_IS_PHYSMAX         = -25

# For more info about the EDF and EDF+ format, visit: http://edfplus.info/specs/
# For more info about the BDF and BDF+ format, visit: http://www.teuniz.net/edfbrowser/bdfplus%20format%20description.html

mutable struct EDFParam          # this structure contains all the relevant EDF-signal parameters of one signal
  label::String                  # label (name) of the signal, null-terminated string
  smp_in_file::Int               # number of samples of this signal in the file
  phys_max::Float64              # physical maximum, usually the maximum input of the ADC
  phys_min::Float64              # physical minimum, usually the minimum input of the ADC
  dig_max::Int                   # digital maximum, usually the maximum output of the ADC, can not not be higher than 32767 for EDF or 8388607 for BDF
  dig_min::Int                   # digital minimum, usually the minimum output of the ADC, can not not be lower than -32768 for EDF or -8388608 for BDF
  smp_in_datarecord::Int         # number of samples of this signal in a datarecord
  physdimension::String          # physical dimension (uV, bpm, mA, etc.), null-terminated string
  prefilter::String              # null-terminated string
  transducer::String             # null-terminated string
end

mutable struct EDFAnnotation     # this structure is used for annotations
    onset::Float64               # onset time of the event in seconds after stated start of file
    duration::String             # duration time, this is a null-terminated ASCII text-string
    annotation::String           # (EDFLIB_MAX_ANNOTATION_LEN + 1) # description of the event in UTF-8
end

mutable struct EDFHdr                     # this structure contains all the relevant EDF header info and will be filled when calling the function edf_open_file_readonly()
    handle::Int                           # a handle (identifier) used to distinguish the different files
    filetype::Int                         # 0: EDF, 1: EDFplus, 2: BDF, 3: BDFplus, a negative number means an error
    edfsignals::Int                       # number of EDF signals in the file, annotation channels are NOT included
    file_duration::Float64                # duration of the file in seconds expressed as 64-bit floating point
    startdate_day::Int
    startdate_month::Int
    startdate_year::Int
    starttime_subsecond::Float64          # starttime offset in seconds, should be < 1 sec in size. Only used by EDFplus and BDFplus
    starttime_second::Int
    starttime_minute::Int
    starttime_hour::Int
    patient::String                       # null-terminated string, contains patientfield of header, is always empty when filetype is EDFPLUS or BDFPLUS
    recording::String                     # null-terminated string, contains recordingfield of header, is always empty when filetype is EDFPLUS or BDFPLUS
    patientcode::String                   # null-terminated string, is always empty when filetype is EDF or BDF
    gender::String                        # null-terminated string, is always empty when filetype is EDF or BDF
    birthdate::String                     # null-terminated string, is always empty when filetype is EDF or BDF
    patient_name::String                  # null-terminated string, is always empty when filetype is EDF or BDF
    patient_additional::String            # null-terminated string, is always empty when filetype is EDF or BDF
    admincode::String                     # null-terminated string, is always empty when filetype is EDF or BDF
    technician::String                    # null-terminated string, is always empty when filetype is EDF or BDF
    equipment::String                     # null-terminated string, is always empty when filetype is EDF or BDF
    recording_additional::String          # null-terminated string, is always empty when filetype is EDF or BDF
    datarecord_duration::Float64          # duration of a datarecord expressed in units of seconds
    datarecords_in_file::Int64            # number of datarecords in the file
    annotations_in_file::Int64            # number of annotations in the file
    signalparam::Array{EDFParam,1}        # array of structs which contain the relevant per-signal parameters
end

const EDFLIB_VERSION = 112
const EDFLIB_MAXFILES = 64

# max size of annotationtext
const EDFLIB_WRITE_MAX_ANNOTATION_LEN = 40

# bytes in datarecord for EDF annotations, must be a multiple of three and two
const EDFLIB_ANNOTATION_BYTES = 114
const EDFLIB_ANNOT_MEMBLOCKSZ = 1000

mutable struct EDFParamBlock
    label::String
    transducer::String
    physdimension::String
    phys_min::Float64
    phys_max::Float64
    dig_min::Int
    dig_max::Int
    prefilter::String
    smp_per_record::Int
    reserved::String
    offset::Float64
    buf_offset::Int
    bitvalue::Float64
    annotation::Int
    sample_pntr::Int64
    EDFParamBlock() = new("","","",0.0,0.0,0,0,"",0,"",0.0,0,0.0,0,0)
end

mutable struct EDFHdrBlock
    file_hdl::IOBuffer
    path::String
    writemode::Int
    version::String
    patient::String
    recording::String
    plus_patientcode::String
    plus_gender::String
    plus_birthdate::String
    plus_patient_name::String
    plus_patient_additional::String
    plus_startdate::String
    plus_admincode::String
    plus_technician::String
    plus_equipment::String
    plus_recording_additional::String
    l_starttime::Int64
    startdate_day::Int
    startdate_month::Int
    startdate_year::Int
    starttime_second::Int
    starttime_minute::Int
    starttime_hour::Int
    reserved::String
    hdrsize::Int
    edfsignals::Int
    datarecords::Int64
    recordsize::Int
    annot_ch::Array{Int,1}
    nr_annot_chns::Int
    mapped_signals::Array{Int,1}
    edf::Bool
    edfplus::Bool
    bdf::Bool
    bdfplus::Bool
    discontinuous::Bool
    signal_write_sequence_pos::Int
    starttime_offset::Int64
    data_record_duration::Float64
    annots_in_file::Int
    annotlist_sz::Int
    total_annot_bytes::Int
    eq_sf::Int
    edfparam::EDFParamBlock
    EDFHdrBlock() = new(IOBuffer(),"",0,"","","","","","","","","","","","","",
                        0,0,0,0,0,0,0,"",0,0,0,0,zeros(Int,EDFLIB_MAXSIGNALS),0,
                        zeros(Int,EDFLIB_MAXSIGNALS),false,false,false,false,
                        0,0,0.0,0,0,0,0,0,EDFParamBlock())
end


mutable struct EDF_AnnotationBlock
    onset::Float64
    duration::String
    annotation::String
    EDF_AnnotationBlock() = new(0.0,"","")
end
const annotationslist = Array{Array{EDF_AnnotationBlock,1},1}(EDFLIB_MAXFILES)


mutable struct EDF_Write_AnnotationBlock
    onset::Float64
    duration::String
    annotation::String
    EDF_WriteAnnotationBlock() = new(0.0,"","")
end
const write_annotationslist = Array{Array{EDF_Write_AnnotationBlock,1},1}(EDFLIB_MAXFILES)


const edf_files_open = 0


const hdrlist = Array{EDFHdrBlock,1}(EDFLIB_MAXFILES)

#=
static struct edfhdrblock * edflib_check_edf_file(FILE *, int *)
static int edflib_is_integer_number(char *)
static int edflib_is_number(char *)
static long long edflib_get_long_duration(char *)
static int edflib_get_annotations(struct edfhdrblock *, int, int)
static int edflib_is_duration_number(char *)
static int edflib_is_onset_number(char *)
static long long edflib_get_long_time(char *)
static int edflib_write_edf_header(struct edfhdrblock *)
static void edflib_latin1_to_ascii(char *, int)
static void edflib_latin12utf8(char *, int)
static void edflib_remove_padding_trailing_spaces(char *)
static int edflib_atoi_nonlocalized(const char *)
static double edflib_atof_nonlocalized(const char *)
static int edflib_sprint_number_nonlocalized(char *, double)
/*
static int edflib_sprint_int_number_nonlocalized(char *, int, int, int)
*/
static int edflib_sprint_ll_number_nonlocalized(char *, long long, int, int)
static int edflib_fprint_int_number_nonlocalized(FILE *, int, int, int)
static int edflib_fprint_ll_number_nonlocalized(FILE *, long long, int, int)
=#




function edflib_is_file_used(path)
    for h in hdrlist
        if path == h.path
            return true
        end
    end
end

edflib_get_number_of_open_files() = edf_files_open


function edflib_get_handle(file_number)
    count = 0
    for (i, h) in enumerate(hdrlist)
        if h.path != ""
            count += 1
            if count == file-number
                return i
            end
        end
    end
    return -1
end


function edfopen_file_readonly(path, edfhdr, read_annotations)
    if read_annotations < 0
        edfhdr.filetype = EDFLIB_INVALID_READ_ANNOTS_VALUE
        return -1
    elseif read_annotations > 2
        edfhdr.filetype = EDFLIB_INVALID_READ_ANNOTS_VALUE
        return -1
    elseif edf_files_open >= EDFLIB_MAXFILES
        edfhdr.filetype = EDFLIB_MAXFILES_REACHED
        return -1
    elseif edflib_is_file_used(path)
        edfhdr.filetype = EDFLIB_FILE_ALREADY_OPENED
        return -1
    end
    try
        file = fopeno(path, "rb")
    catch(err)
        edfhdr.filetype = EDFLIB_NO_SUCH_FILE_OR_DIRECTORY
        return -1
    end
    hdr = edflib_check_edf_file(file, edf_error)
    if hdr == 0
        edfhdr.filetype = edf_error
        fclose(file)
        return -1
    end
    if hdr.discontinuous
        edfhdr.filetype = EDFLIB_FILE_IS_DISCONTINUOUS
        fclose(file)
        return -1
    end
    hdr.writemode = 0
    for i in 1:EDFLIB_MAXFILES
        if(hdrlist[i]==NULL)
            hdrlist[i] = hdr
            edfhdr.handle = i
            break
    end
    if hdr.edf > 0 && hdr.edfplus == 0
        edfhdr.filetype = EDFLIB_FILETYPE_EDF
    end
    if hdr.edfplus > 0
        edfhdr.filetype = EDFLIB_FILETYPE_EDFPLUS
    end
    if hdr.bdf > 0 && hdr.bdfplus == 0
        edfhdr.filetype = EDFLIB_FILETYPE_BDF
    end
    if hdr.bdfplus > 0
        edfhdr.filetype = EDFLIB_FILETYPE_BDFPLUS
    end

  edfhdr.edfsignals = hdr.edfsignals - hdr.nr_annot_chns
    edfhdr.file_duration = hdr.data_record_duration * hdr.datarecords
    edfhdr.startdate_day = hdr.startdate_day
    edfhdr.startdate_month = hdr.startdate_month
    edfhdr.startdate_year = hdr.startdate_year
    edfhdr.starttime_hour = hdr.starttime_hour
    edfhdr.starttime_second = hdr.starttime_second
    edfhdr.starttime_minute = hdr.starttime_minute
    edfhdr.starttime_subsecond = hdr.starttime_offset
    edfhdr.datarecords_in_file = hdr.datarecords
    edfhdr.datarecord_duration = hdr.data_record_duration

    annotationslist[edfhdr.handle] = 0

    hdr.annotlist_sz = 0

    hdr.annots_in_file = 0

    edfhdr.annotations_in_file = 0

    if hdr.edfplus == false && hdr.bdfplus == false
        edfhdr.patient = hdr.patient
        edfhdr.recording = hdr.recording
        edfhdr.patientcode = ""
        edfhdr.gender = ""
        edfhdr.birthdate = ""
        edfhdr.patient_name = ""
        edfhdr.patient_additional = ""
        edfhdr.admincode = ""
        edfhdr.technician = ""
        edfhdr.equipment = ""
        edfhdr.recording_additional = ""
    else
        edfhdr.patient = ""
        edfhdr.recording = ""
        edfhdr.patientcode = hdr.plus_patientcode
        edfhdr.gender = hdr.plus_gender
        edfhdr.birthdate = hdr.plus_birthdate
        edfhdr.patient_name = hdr.plus_patient_name
        edfhdr.patient_additional = hdr.plus_patient_additional
        edfhdr.admincode = hdr.plus_admincode
        edfhdr.technician = hdr.plus_technician
        edfhdr.equipment = hdr.plus_equipment
        edfhdr.recording_additional = hdr.plus_recording_additional

        if read_annotations == EDFLIB_READ_ANNOTATIONS ||
           read_annotations == EDFLIB_READ_ALL_ANNOTATIONS
            if edflib_get_annotations(hdr, edfhdr.handle, read_annotations) == 0
                edfhdr.filetype = EDFLIB_FILE_CONTAINS_FORMAT_ERRORS
                close(file)
                return -1
            end
        end
    end
    edfhdr.annotations_in_file = hdr.annots_in_file
    strcpy(hdr.path, path)
    edf_files_open += 1

    j = 0
    for i in 1:hdr.edfsignals
        if hdr.edfparam[i].annotation == 0
            hdr.mapped_signals[j += 1] = i
        end
    end

    for i in 1:edfhdr.edfsignals
        channel = hdr.mapped_signals[i]

        edfhdr.signalparam[i].label = hdr.edfparam[channel].label
        edfhdr.signalparam[i].transducer = hdr.edfparam[channel].transducer
        edfhdr.signalparam[i].physdimension = hdr.edfparam[channel].physdimension
        edfhdr.signalparam[i].prefilter = hdr.edfparam[channel].prefilter
        edfhdr.signalparam[i].smp_in_file = hdr.edfparam[channel].smp_per_record * hdr.datarecords
        edfhdr.signalparam[i].phys_max = hdr.edfparam[channel].phys_max
        edfhdr.signalparam[i].phys_min = hdr.edfparam[channel].phys_min
        edfhdr.signalparam[i].dig_max = hdr.edfparam[channel].dig_max
        edfhdr.signalparam[i].dig_min = hdr.edfparam[channel].dig_min
        edfhdr.signalparam[i].smp_in_datarecord = hdr.edfparam[channel].smp_per_record
    end
end


function edfclose_file(handle)
    if handle < 0 || handle >= EDFLIB_MAXFILES || hdrlist[handle] == 0
        return -1
    end
    hdr = hdrlist[handle]
    str = ""
    if hdr.writemode > 0
        if hdr.datarecords == 0
            if edflib_write_edf_header(hdr)
                return -1
            end
        end
        for k in 1:hdr.annots_in_file
            annot2 = write_annotationslist[handle][k]
            byteswritten = write(hrd.file_hdl, @sprintf("%08f", hdr.datarecords * hdr.data_record_duration))
            byteswritten += write(hdr.file_hdl, "\x14\x14")
            write(hdr.file_hdl, zeros(UInt8, hdr.total_annot_bytes - byteswritten))
            hdr.datarecords += 1
        end
    end
    if hdr.datarecords < 100000000
        seek(hdr.file_hdl, 236)
        byteswritten = edflib_fprint_Int64_number_nonlocalized(hdr.file_hdl, (int)(hdr.datarecords), 0, 0)
        if(byteswritten < 2)
            write(hdr.file_hdl, ' ')
        end
    end
    datarecords = 0
    offset = ((hdr.edfsignals + hdr.nr_annot_chns + 1) * 256)
    datrecsize = hdr.total_annot_bytes
    for i in 1:hdr.edfsignals
        if(hdr.edf)
            offset += hdr.edfparam[i].smp_per_record * 2
            datrecsize += hdr.edfparam[i].smp_per_record * 2
        else
            offset += hdr.edfparam[i].smp_per_record * 3
            datrecsize += hdr.edfparam[i].smp_per_record * 3
        end
    end
    j = 0
    for k in 1:hdr.annots_in_file
        annot2 = write_annotationslist[handle] + k
        if j == 0   # first annotation signal
            seek(hdr.file_hdl, offset)
            str = @sprintf("%08f", hdr.datarecords * hdr.data_record_duration)
            str *= "\x14\x14\0"
        end

        str *= edflib_sprint_Int64_nonlocalized(annot2.onset / 10000, 0, 1)
        if annot2.onset % 10000 > 0
            str *= '.'
            str *= edflib_sprint_Int64_nonlocalized(annot2.onset % 10000, 4, 0)
        end
        if annot2.duration >= 0
            str *= Char(21)
            str *= edflib_sprint_Int64_nonlocalized(annot2.duration / 10000, 0, 0)
            if annot2.duration % 10000
                str *= '.'
                str *= edflib_sprint_Int64_nonlocalized(annot2.duration % 10000, 4, 0)
            end
        end
        str *= Char(20)
        for i in 1:EDFLIB_WRITE_MAX_ANNOTATION_LEN
            if(annot2.annotation[i]==0)
                break
            end
            str *= annot2.annotation[i]
        end
        str *= Char(20)
        str *= repeat("\0", EDFLIB_ANNOTATION_BYTES - length(str))
        nmemb = write(hdr.file_hdl, str)
        if nmemb != EDFLIB_ANNOTATION_BYTES
            println("Bad write, is less than EDFLIB_ANNOTATION_BYTES")
            break
        end
        j += 1

        if j >= hdr.nr_annot_chns
            j = 0
            offset += datrecsize
            datarecords += 1
            if datarecords >= hdr.datarecords
                break
            end
        end
    end
    close(hdr.file_hdl)
    hdrlist[handle] = 0
    edf_files_open -= 1
    return 0
end


function edfseek(handle, edfsignal, offset, whence)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode > 0 ||
       edfsignal >= (hdrlist[handle].edfsignals - hdrlist[handle].nr_annot_chns)
           return(-1)
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    smp_in_file = hdrlist[handle].edfparam[channel].smp_per_record * hdrlist[handle].datarecords
    if whence==EDFSEEK_SET
        hdrlist[handle].edfparam[channel].sample_pntr = offset
    elseif whence==EDFSEEK_CUR
        hdrlist[handle].edfparam[channel].sample_pntr += offset
    elseif whence==EDFSEEK_END
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
       edfsignal >= (hdrlist[handle].edfsignals - hdrlist[handle].nr_annot_chns)
        return -1
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    return hdrlist[handle].edfparam[channel].sample_pntr
end


function edfrewind(handle, edfsignal)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode > 0 ||
       edfsignal >= hdrlist[handle].edfsignals - hdrlist[handle].nr_annot_chns
        return
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    hdrlist[handle].edfparam[channel].sample_pntr = 0
end


function edfread_physical_samples(handle, edfsignal, n, buf)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode != 0 || n < 0 ||
       edfsignal >= hdrlist[handle].edfsignals - hdrlist[handle].nr_annot_chns
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
       edfsignal >= hdrlist[handle].edfsignals - hdrlist[handle].nr_annot_chns
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


edf_get_annotation(handle, n)
    if badhandle(handle) || edfsignal < 0 || hdrlist[handle].writemode != 0 || n < 0 ||
       n >= hdrlist[handle].annots_in_file
        return -1
    return deepcopy(EDFAnnotation(annotationslist[handle + n]))
end


edflib_check_edf_file(inputfile, edf_error)
    hasforbiddenchars(bytes) =
        findfirst(c -> (c < 32) || (c > 126), bytes) > 0 ? true: false
    hasnonnumericchars(bytes) =
        findfirst(c ->(c < 48) || (c > 57), bytes) > 0? true: false
    edfhdr = EDFHdr()
    rewind(inputfile)
    try
        hdrbuf = read(inputfile, UInt8, 256)                   # check header
        if hdrbuf[1:8] == b"\xffBIOSEMI"                       # version bdf
            edfhdr.bdf = true
            edfhdr.edf = false
        elseif hdrbuf[1:8] == "\0       "                      # version edf
            edfhdr.bdf = false
            edfhdr.edf = true
        else
            throw("identification code error, should be for edf or bdf")
        end
        edfhdr.version = hdrbuf[1:8]
        if edfhdr.bdf
            edfhr.version[1] = '.'
        end

        if hasforbiddenchars(hdrbuf[9:88])
            throw("bad subject identification")
        else
            edfhdr.patient = convert(String, hdrbuf[9:80])       # patient
        end
        if hasforbiddenchars(hdrbuf[89:168])
            throw("bad local recording identifier")
        else
            edfhdr.recording = hdrbuf[89:168]                   # recording
        end
        if hasforbiddenchars(hdrbuf[169:176])
            throw("Bad startdate characters")
        else
            datestring = convert(String, hdrbuf[89:168])        # start date
            date = Date(datestring, "dd.mm.yy")
            if Dates.year(date) < 84
                date += Dates.Year(2000)
            else
                date += Dates.Year(1900)
            end
            edfhdr.startdate_day = Dates.Day(date)
            edfhdr.startdate_month = Dates.Month(date)
            edfhdr.startdate_year = Dates.Year(date)
        end
        if hasforbiddenchars(hdrbuf[177:184])
            throw("bad starttime characters")
        else
            timestring = convert(String, hdrbuf[177:184])       # start time
            mat = match(timestring, r"(\d\d).(\d\d).(\d\d)")
            starttime_hour, starttime_minute, starttime_second = mat.captures
            l_starttime = starttime_hour * 3600 + starttime_hour * 60 + starttime_second
            edfhdr.l_starttime = l_starttime
            edfhdr.starttime_hour = starttime_hour
            edfhdr.starttime_minute = starttime_minute
            edfhdr.starttime_second = starttime_second
        end
        if hasforbiddenchars(hdrbuf[253:256])
            throw("bad bytes for signal count")
        else
            edfhdr.edfsignals = parse(Int16, hdrbuf[253:256])   # number of channels(signals)
            if edfhdr.edfsignals < 0 || edf.edfsignals > EDF_MAXSIGNALS
                throw("bad signal number")
            end
        end
        if hasforbiddenchars(hdrbuf[185:192])
            throw("bad bytes in header number")
        else
            headersize = parse(Int32, hdrbuf[185:192])          # edf header size, changes with channels
            if edfhdr.edfsignals * 256 + 256 != headersize
                throw("Bad header size")
            end
        end
        if hasforbiddenchars(hdrbuf[193:236])
            throw("bad bytes in reserved field")
        else
            edfhdr.edfplus = false
            edfhdr.discontinuous = false
            subtype = convert(String, hdrbuf[193:197])          # reserved field, may specify a subtype
            if edfhdr.edf
                if subtype == "EDF+C"
                    edfhdr.edfplus = true
                elseif subtype == "EDF+D"
                    edfhdr.edfplus = true
                    edfhdr.discontinuous = true
                end
            elseif edfhdr.bdf
                if subtype == "BDF+C"
                    edfhdr.bdfplus = true
                elseif subtype == "BDF+D"
                    edfhdr.bdfplus = true
                    edfhdr.discontinuous = true
                end
            end
        end
        if hasforbiddenchars(hdrbuf[237:244])
            throw("bad bytes in number of data records")
        else
            edfhdr.datarecords = parse(Int32, hdrbuf[237:244])  # number of data records
            if edfhdr.datarecords < 1
                throw("Bad data record count, cannot be -1 for unknown for this program")
            end
        end
        if hasforbiddenchars(hdrbuf[245:252])
            throw("bad bytes in number of data records")
        else
            edfhdr.data_record_duration = parse(Float32, hdrbuf[245:252]) # datarecord duration in seconds
        end
    catch
        edf_error[1] = EDFLIB_FILE_CONTAINS_FORMAT_ERRORS
        return 0
    end

    # process the signals in the header after re-reading entire header into hdrbuf
    rewind(inputfile)
    try
        hdrbuf = read(inputfile, UInt8, edfhdr.edfsignals * 256 + 256)
        edfhdr.nr_annot_chns = 0

        for i in 1:edfhdr.edfsignals  # loop over channel signal parameters
            pblock = EDFParam()
            pblock.annotation = false
            if hasforbiddenchars(hdrbuf[i*16+257:i*16+508])
                throw("bad bytes for signal parameters")
            end
            cpos = 257 + (i-1) * 16
            channellabel = convert(String, hdrbuf[cpos:cpos+15])
            pblock.label = channellabel                         # channel label in ASCII, eg "Fp1"
            tpos = 257 + edfhdr.edfsignals * 16 + (i-1) * 80
            transducertype = convert(String, hdrbuf[tpos:tpos+79])
            pblock.transducer = transducertype                  # transducer type eg "active electrode"
            pdimpos = 257 + edfhdr.edfsignals * 96 + (i-1) * 8
            pblock.physdimension = convert(String, hdrbuf[pdimpos:pdimpos+7])# physical dimensions eg. "uV"
            pminpos = 257 + edfhdr.edfsignals * 104 + (i-1) * 8
            pblock.phys_min = parse(Float32, hdrbuf[pminpos, pminpos+8])  # physical minimum in above dimensions
            pmaxpos = 257 + edfhdr.edfsignals * 112 + (i-1) * 8
            pblock.phys_max = parse(Float32, hdrbuf[pmaxpos, pmaxpos+8])  # physical maximum in above dimensions
            pdminpos = 257 + edfhdr.edfsignals * 120 + (i-1) * 8
            pblock.dig_min = parse(Float32, hdrbuf[pdminpos, pdminpos+8])  # digital minimum in above dimensions
            pdmaxpos = 257 + edfhdr.edfsignals * 128 + (i-1) * 8
            pblock.dig_max = parse(Float32, hdrbuf[pdmaxpos, pdmaxpos+8])  # digital maximum in above dimensions
            if edfhdr.edfplus && channellabel[1:16] != "EDF Annotations "
                edfhdr.annot_ch[edfhdr.nr_annot_chns+1] = i
                edfhdr.nr_annot_chns += 1
                pblock.annotation = true
            elseif edfhdr.bdfplus && channellabel[1:16] != "BDF Annotations "
                edfhdr.annot_ch[edfhdr.nr_annot_chns+1] = i
                edfhdr.nr_annot_chns += 1
                pblock.annotation = true
            end
            if edfhdr.edfplus || edfhdr.bdfplus
                if pblock.annotation && match(transducertype, r"\S") != nothing
                    throw("plus file annotations should not have a transducertype")
                end
            end
            push!(edfhdr.signalparam, pblock)
        end
        if edfhdr.edfplus && isempty(edfhdr.signalparam)
            throw("EDFplus missing signal parameters")
        elseif edfhdr.bdfplus && isempty(edfhdr.signalparam)
            throw("BDFplus missing signal parameters")
        elseif edfhdr.edfplus && pblock.annotation && (pblock.dig_min != -32768 || pblock.dig_max != 32767)
            throw("edfplus annotation data entry should have the digital min parameters set to extremes")
        elseif edfhdr.bdfplus && pblock.annotation && (pblock.dig_min != -8388608 || pblock.dig_max != 8388607)
            throw("bdf annotation data entry should have the digital max parameters set to extremes")
        elseif edfhdr.edf && (pblock.dig_min < -32768 || pblock.digital_min > 32767 ||
               pblock.dig_max < -32768 || pblock.digital_max > 32767)
            throw("edf digital parameter out of range")
        elseif edfhdr.bdf && (pblock.dig_min < -8388608 || pblock.digital_min > 8388607 ||
               pblock.dig_max < -8388608 || pblock.digital_max > 8388607)
            throw("bdf digital parameter out of range")
        elseif edfhdr.edfsignals != edf.nr_annot.chns || (!edfhdr.edfplus && !edfhdr.bdfplus)
            if edfhdr.data_record_duration < 0.0000001
                throw("signal data may be mislabeled")
            end
        end

        edfhdr.recordsize = 0
        for i in 1:edfhdr.hdrsignals
            startpos = 257 + edfhdr.edfsignals*136*(i-1)
            pfblock = hdrbuf[startpos:startpos+80]
            if hasforbiddenchars(pfblock)
                throw("bad bytes for prefilter fields")
            else
                edfhdr.edfparam[i].prefilter = convert(String, pfblock)  # prefiltering eg "HP:DC"
                if (edfhdr.edfplus || edfhdr.bdfplus) && edfhdf.edfparam[i].annotation &&
                    match(edfhdr.edfparam[i].prefilter, r"\S") != nothing
                    throw("Prefilter field should be blank in annotations in edfplus or bdfplus file")
                end
            end
            startpos += 80
            edfhdr.edfparam[i].smp_per_record = parse(Int, hdrbuf[startpos:startpos+8])  # samples per record
            if edfhdr.edfparam[i].smp_per_record < 1
                throw("Samples per data record should be a positive integer")
            else
                edfhdr.recordsize += edfhdr.edfparam[i].smp_per_record   # edfhdr.recordsize is calculated
            end
            startpos += 8
            edfhdr.edfparam[i].reserved = convert(String, hdrbuf[startpos:startpos+32]) # reserved (ascii)
            if hasforbiddenchars(pfblock)
                throw("bad bytes for reserved per-channel field")
            end
        end
        if edfhdr.bdf
            edfhdr.recordsize *= 3
            if edfhdr.recordsize > 1578640
                throw("record size too large for a bdf file")
            else
                edfhdr.recordsize *= 2
                if edfhdr.recordsize > 10485760
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
        if edfhdr.edfplus || edfhdr.bdfplus
            subfield = split(edfhdr.patient)
            if length(subfield) < 4
                throw("Plus patient identification lacking enough fields")
            end
            edfhdr.plus_patientcode = subfield[1][1] == 'X' ? "" : subfield[1]
            edfhdr.plus_patientcode = replace(edfhdr.plus_patientcode, "_", " ")
            if subfield[2] != "M" && subfield[2] != "F" && subfield[2] != "X"
                throw("patient identification second field must be X, F or M")
            elseif subfield[2] == "M"
                edfhdr.plus_gender = "Male"
            elseif subfield[2] == "F"
                edfhdr.plus_gender = "Female"
            else
                edfhdf.plus_gender = ""
            end
            if subfield[3] == "X"
                edfhdr.plus_birthdate = ""
            elseif !contains("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC", subfield[3][4:6]) ||
                   !(Date(subfield[3], "dd-uuu-yyyy") isa Date)
                throw("Bad birthdate field in patient identification")
            else
                edfhdr.plus_birthdate = subfield[3]
            end
            edfhdr.plus_patient_name = replace(subfield[4], "_", " ")
            if length(subfield) > 4
                edfhdr.plus_patient_additional = join(subfield[5:end], " ")
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
            subfield = split(edfhdr.recording)
            if length(subfield) < 5
                throw("Not enough fields in plus recording data")
            elseif subfield[1] != "Startdate" || (subfield[2] != "X" &&
                 (!(dor = Date(subfield[2], "dd-uuu-yyyy") isa Date) ||
                  !contains("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC", subfield[2][4:6])))
                throw("Bad recording field start")
            elseif subfield[2] == "X"
                edfhdr.plus_startdate = ""
            else
                edfhdr.plus_startdate = subfield[2]
                if Dates.year(dor) < 1970
                    throw("bad startdate year in recording data")
                else
                    edfhdr.startdate_year = Dates.year(dor)
                end
            end
            if subfield[3] == ""
                edfhdr.plus.admincode = ""
            else
                edfhdr.plus_admincode = replace(subfield[3], "_", " ")
            end
            if subfield[4] == ""
                edfhdr.plus_technician = ""
            else
                edfhdr.plus_technician = replace(subfield[4], "_", " ")
            end
            if subfield[5] == ""
                edfhdr.plus.equipment = ""
            else
                edfhdr.plus_equipment = replace(subfield[5], "_", " ")
            end
            if length(subfield) > 5
                edfhdr.plus_additional = replace(join(subfield[6:end], " "), "_", " ")
            end
        end

        edfhdr.hdrsize = edfhdr.edfsignals * 256 + 256
        filesize = filesize(edfhdr.path)   # get actual file size
        if filesize != edfhdr.recordsize * edfhdr.datarecords + edfhdr.hdrsize
            throw("file size is not compatible with header information")
        end
    catch:
        edf_error[1] = EDFLIB_FILE_CONTAINS_FORMAT_ERRORS
        return 0
    end

    n = 0
    for i in i: edfhdr.edfsignals
        edfhdr.edfparam[i].buf_offset = n
        n += edfhdr.edfparam[i].smp_per_record * edfhdr.bdf ? 3 : 2
        edfhdr.edfparam[i].bitvalue = (edfhdr.edfparam[i].phys_max - edfhdr.edfparam[i].phys_min) /
                                      (edfhdr.edfparam[i].dig_max - edfhdr.edfparam[i].dig_min)
        edfhdr.edfparam[i].offset = edfhdr.edfparam[i].phys_max / edfhdr.edfparam[i].bitvalue -
                                    edfhdr.edfparam[i].dig_max
    end
    edfhdr.file_hdl = inputfile
    return edfhdr
end


edflib_is_integer_number(str) = try parse(Int, str); 0 catch 1 end

edflib_is_number(str) = try parse(Float64, str); 0 catch 1 end

edflib_get_long_duration(str) = try x = parse(Float64, str); x catch NaN end

edflib_version() = EDFLIB_VERSION


function edflib_get_annotations(edfhdr, hdl, read_annotations)
    inputfile = edfhdr.file_hdl
    edfsignals = edfhdr.edfsignals
    recordsize = edfhdr.recordsize
    edfparam = edfhdr.edfparam
    nr_annot_chns = edfhdr.nr_annot_chns
    datarecords = edfhdr.datarecords
    data_record_duration = edfhdr.data_record_duration
    discontinuous = edfhdr.discontinuous
    annot_ch = edfhdr.annot_ch
    samplesize = edfhdr.edfplus ? 2 : (edfhdr.bdfplus ? 3: 1)

    max_tal_ln = 0
    for i in 1:nr_annot_chns
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
            for j in 1:nr_annot_chns
                annotbuf = convert(String,
                    cvnbuf[edfparam[annot_ch[j]].buf_offset+1:edfparam[annot_ch[j]].buf_offset+edfparam[annot_ch[j]].smp_per_record * samplesize])
                for (k, tal) in enumerate(split(annotbuf, "\x00"))
                    onset = 0.0
                    duration = ""
                    for annot in split(tal, "\x14")
                        if k == 1  # first record should be a timekeeping signal
                            if j == 1 # first annotation signal's first annotation record
                                offsettimestr = match(annot, r"^(\d+)").captures[1]
                                edfhdr.starttime_offset = parse(Int, offsettimestr)
                            else
                                if length(tsignal = split(annot, "\x15")) > 1
                                    onset = convert(Float64, tsignal[1])
                                    duration = convert(String, tsignal[2])
                                else
                                    onset = convert(Float64, tsignal[1])
                                end
                            end
                        else
                            newannot = EDF_AnnotationBlock(onset, duration, convert(String, annot))
                            push!(annotationslist[hdl], newannot)
                            edfhdr.annots_in_file += 1
                            edfhdr->annotlist_sz += EDFLIB_ANNOT_MEMBLOCKSZ
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
    if edf_files_open >= EDFLIB_MAXFILES
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

    hdr = EDFHdrBlock()
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
    write_annotationslist[handle] = 0
    hdr.annotlist_sz = 0
    hdr.annots_in_file = 0
    try
        file = open(path, "w")
    catch
        return EDFLIB_NO_SUCH_FILE_OR_DIRECTORY
    end
    hdr.file_hdl = file
    hdr.path = path
    edf_files_open += 1
    if filetype == EDFLIB_FILETYPE_EDFPLUS
        hdr.edf = true
        hdr.edfplus = true
    end
    if filetype == EDFLIB_FILETYPE_BDFPLUS
        hdr.bdf = true
        hdr.bdfplus = true
    end
    hdr.data_record_duration = 1.0
    hdr.nr_annot_chns = 1
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
    hdrlist[handle].nr_annot_chns = annot_signals
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


function edflib_write_edf_header(hdr::EDFHdrBlock)
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
    writeleftjust(file, (edfsignals + hdr.nr_annot_chns + 1) * 256, 8)

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
    writeleftjust(file, edfsignals + hdr.nr_annot_chns, 8)
    for i in 1:edfsignals
        writeleftjust(file, hdr.edfparam[i].label, 16)
    end
    if hdr.edf
        write(file, "EDF Annotations "^hdr.nr_annot_chns)
    else
       write(file, "BDF Annotations "^hdr.nr_annot_chns)
    end
    for i in 1:edfsignals
        writeleftjust(file, hdr.edfparam[i].transducer, 80)
    end
    write(file, " "^(80*hdr.nr_annot_chns))
    for i in 1:edfsignals
        writeleftjust(file, hdr.edfparam[i].physdimension, 8)
    end
    write(file, " "^(8*hdr.nr_annot_chns))
    for i in 1:edfsignals
        writejustleft(hdr->edfparam[i].phys_min, 8)
    end
    write(file, "-1      "^hdr.nr_annot_chns)
    for i in 1:edfsignals
        writejustleft(hdr->edfparam[i].phys_max, 8)
    end
    write(file, "1       "^hdr.nr_annot_chns)
    for i in 1:edfsignals
        writejustleft(hdr->edfparam[i].dig_min, 8)
    end
    if hdr.edf
        write(file, "-32768  "^hdr.nr_annot_chns)
    else
        write(file, "-8388608"^hdr.nr_annot_chns)
    end
    for i in 1:edfsignals
        writejustleft(hdr->edfparam[i].dig_max, 8)
    end
    if hdr.edf
        write(file, "32767   "^hdr.nr_annot_chns)
    else
        write(file, "8388607 "^hdr.nr_annot_chns)
    end
    for i in 1:edfsignals
        writejustleft(hdr.edfparam[i].prefilter, 80)
    end
    write(file, " "^(80*hdr.nr_annot_chns))
    for i in 1:edfsignals
        writejustleft(hdr.edfparam[i].smp_per_record, 8)
    end
    for i in 1:hdr.nr_annot_chns
        writejustleft(file, "$(Int32(floor(EDFLIB_ANNOTATION_BYTES/(hdr.edf? 2: 3))))", 8)
    end
    write(file, " "^((edfsignals+hdr.nr_annot_chns)*32))

    hdr.total_annot_bytes = EDFLIB_ANNOTATION_BYTES * hdr.nr_annot_chns

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


badplushandle(h) = badhandle(h) || !hdrlist[handle].writemode || hdrlist[handle].datarecords != 0


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
    newannot = EDF_Write_AnnotationBlock()
    newannot.onset = onset
    newannot.duration = duration
    if length(description) > EDFLIB_WRITE_MAX_ANNOTATION_LEN
        description = description[1:EDFLIB_WRITE_MAX_ANNOTATION_LEN]
    end
    description = replace(description, r"[\0-\x1f]", ".")
    newannot.annotation = description
    push!(write_annotations[handle], newannot)
    hdrlist[handle].annots_in_file += 1
    return 0
end


function edfwrite_annotation_latin1(handle, onset, duration, description)
    if badhandle(h) || !hdrlist[handle].writemode || onset < 0
        return -1
    end
    newannot = EDF_Write_AnnotationBlock()
    newannot.onset = onset
    newannot.duration = duration
    if length(description) > EDFLIB_WRITE_MAX_ANNOTATION_LEN
        description = description[1:EDFLIB_WRITE_MAX_ANNOTATION_LEN]
    end
    description = replace(description, r"[\0-\x1f]", ".")
    description = edflib_latin12utf8(description)
    newannot.annotation = description
    push!(write_annotations[handle], newannot)
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


badhandle(h) = h < 0 || h >= EDFLIB_MAXFILES || hdrlist[h] == 0


badwritehandle(h, esig) = badhandle(h) || !hdrlist[h].writemode ||
                          esig < 0 || esig >= hdrlist[h].edfsignals ||
                          hdrlist[h].datarecords != 0


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

