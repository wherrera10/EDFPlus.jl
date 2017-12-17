#==============================================================================
/*
*****************************************************************************
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
*****************************************************************************
*/
=================================================================================#



const EDFLIB_TIME_DIMENSION      =   10000000
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
    onset::Int64                 # onset time of the event, expressed in units of 100 nanoSeconds and relative to the starttime in the header
    duration::String             # duration time, this is a null-terminated ASCII text-string
    annotation::Array{String,1}  # (EDFLIB_MAX_ANNOTATION_LEN + 1) # description of the event in UTF-8, this is a null terminated string
end

mutable struct EDFHdr                     # this structure contains all the relevant EDF header info and will be filled when calling the function edf_open_file_readonly()
    handle::Int                           # a handle (identifier) used to distinguish the different files
    filetype::Int                         # 0: EDF, 1: EDFplus, 2: BDF, 3: BDFplus, a negative number means an error
    edfsignals::Int                       # number of EDF signals in the file, annotation channels are NOT included
    file_duration::Int64                  # duration of the file expressed in units of 100 nanoSeconds
    startdate_day::Int
    startdate_month::Int
    startdate_year::Int
    starttime_subsecond::Int64            # starttime offset expressed in units of 100 nanoSeconds. Is always less than 10000000 (one second). Only used by EDFplus and BDFplus
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
    datarecord_duration::Int64            # duration of a datarecord expressed in units of 100 nanoSeconds
    datarecords_in_file::Int64            # number of datarecords in the file
    annotations_in_file::Int64            # number of annotations in the file
    signalparam::Array{EDFParam,1}()      # array of structs which contain the relevant per-signal parameters
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
    dig_min::Int32
    dig_max::Int32
    prefilter::String
    smp_per_record::Int16
    reserved::String
    offset::Float64
    buf_offset::Int16
    bitvalue::Float64
    annotation::Int16
    sample_pntr::Int64
end

mutable struct EDFHdrBlock
    file_hdl::IOBuffer
    path::String
    writemode::Int16
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
    startdate_day::Int16
    startdate_month::Int16
    startdate_year::Int16
    starttime_second::Int16
    starttime_minute::Int16
    starttime_hour::Int16
    reserved::String
    hdrsize::Int16
    edfsignals::Int16
    datarecords::Int64
    recordsize::Int16
    annot_ch::Array{Int16,1}
    nr_annot_chns::Int16
    mapped_signals::Array{Int16,1}
    edf::Int16
    edfplus::Int16
    bdf::Int16
    bdfplus::Int16
    discontinuous::Int16
    signal_write_sequence_pos::Int16
    starttime_offset::Int64
    data_record_duration::Float64
    long_data_record_duration::Int64
    annots_in_file::Int16
    annotlist_sz::Int16
    total_annot_bytes::Int16
    eq_sf::Int16
    edfparam::EDFParamBlock
end


mutable struct EDF_AnnotationBlock
    onset::Int64
    duration::String
    annotation::String
    EDF_AnnotationBlock() = new(0,"","")
end
const annotationslist = Array{EDF_AnnotationBlock,1}(EDFLIB_MAXFILES)


mutable struct EDF_Write_AnnotationBlock
    onset::Int64
    duration::Int64
    annotation::String
    EDF_WriteAnnotationBlock() = new(0,0,"")
end
const write_annotationslist = Array{EDF_Write_AnnotationBlock,1}(EDFLIB_MAXFILES)

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
    edfhdr.file_duration = hdr.long_data_record_duration * hdr.datarecords
    edfhdr.startdate_day = hdr.startdate_day
    edfhdr.startdate_month = hdr.startdate_month
    edfhdr.startdate_year = hdr.startdate_year
    edfhdr.starttime_hour = hdr.starttime_hour
    edfhdr.starttime_second = hdr.starttime_second
    edfhdr.starttime_minute = hdr.starttime_minute
    edfhdr.starttime_subsecond = hdr.starttime_offset
    edfhdr.datarecords_in_file = hdr.datarecords
    edfhdr.datarecord_duration = hdr.long_data_record_duration

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
            annot2 = write_annotationslist[handle] + k
            byteswritten = edflib_fprint_Int64_number_nonlocalized(hdr.file_hdl, (hdr.datarecords * hdr.long_data_record_duration) / EDFLIB_TIME_DIMENSION, 0, 1)
            if(hdr.long_data_record_duration % EDFLIB_TIME_DIMENSION)
                write(hdr.file_hdl, '.')
                byteswritten += 1
                byteswritten += edflib_fprint_Int64_number_nonlocalized(hdr.file_hdl, (hdr.datarecords * hdr.long_data_record_duration) % EDFLIB_TIME_DIMENSION, 7, 0)
            end
            write(hdr.file_hdl, "\x14\x14")
            byteswritten += 2
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
            str = edflib_sprint_Int64_nonlocalized((datarecords * hdr.long_data_record_duration) / EDFLIB_TIME_DIMENSION, 0, 1)
            if hdr.long_data_record_duration % EDFLIB_TIME_DIMENSION > 0
                str *= '.'
                str *= edflib_sprint_Int64_nonlocalized((datarecords * hdr.long_data_record_duration) % EDFLIB_TIME_DIMENSION, 7, 0)
            end
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
    if handle < 0 || handle >= EDFLIB_MAXFILES || hdrlist[handle] == 0 ||
       edfsignal < 0 || hdrlist[handle].writemode > 0 ||
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
    if handle < 0 || handle >= EDFLIB_MAXFILES || hdrlist[handle] == 0 ||
       edfsignal < 0 || hdrlist[handle].writemode > 0 ||
       edfsignal >= (hdrlist[handle].edfsignals - hdrlist[handle].nr_annot_chns)
        return -1
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    return hdrlist[handle].edfparam[channel].sample_pntr
end


function edfrewind(handle, edfsignal)
    if handle < 0 || handle >= EDFLIB_MAXFILES || hdrlist[handle] == 0 ||
       edfsignal < 0 || hdrlist[handle].writemode > 0 ||
       edfsignal >= hdrlist[handle].edfsignals - hdrlist[handle].nr_annot_chns
        return
    end
    channel = hdrlist[handle].mapped_signals[edfsignal]
    hdrlist[handle].edfparam[channel].sample_pntr = 0
end


function edfread_physical_samples(handle, edfsignal, n, buf)
    if handle < 0 || handle >= EDFLIB_MAXFILES || hdrlist[handle] == 0 ||
       edfsignal < 0 || hdrlist[handle].writemode != 0 || n < 0 ||
       edfsignal >= hdrlist[handle].edfsignals - hdrlist[handle].nr_annot_chns)
        return -1

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
            if sample_pntr % smp_per_record) == 0
                if i > 1
                    seek(file, jump)
                end
            end
            i16 = read(file, Int16)
            buf[i] = phys_bitvalue * (phys_offset + Float64(i16))
            sample_pntr++
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
            sample_pntr++
        end
    end
    hdr.edfparam[channel].sample_pntr = sample_pntr
    return n
end


function edfread_digital_samples(handle, edfsignal, n, buf)
    if handle < 0 || handle >= EDFLIB_MAXFILES || hdrlist[handle] == 0 ||
       edfsignal < 0 || hdrlist[handle].writemode != 0 || n < 0 ||
       edfsignal >= hdrlist[handle].edfsignals - hdrlist[handle].nr_annot_chns)
        return -1

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
            if sample_pntr % smp_per_record) == 0
                if i > 1
                    seek(file, jump)
                end
            end
            buf[i] = read(file, Int16)
            sample_pntr++
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
            sample_pntr++
        end
    end
    hdr.edfparam[channel].sample_pntr = sample_pntr
    return n
end


edf_get_annotation(handle, n)
    if handle < 0 || handle >= EDFLIB_MAXFILES || hdrlist[handle] == 0 ||
       edfsignal < 0 || hdrlist[handle].writemode != 0 || n < 0 ||
       n >= hdrlist[handle].annots_in_file
        return -1
    return deepcopy(EDFAnnotation(annotationslist[handle + n])
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
        elseif hdrbuf[1:8] = "\0       "                       # version edf
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
            edfhdr.l_starttime = l_starttime * EDFLIB_TIME_DIMENSION
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
                if subtype = "BDF+C"
                    edfhdr.bdfplus = true
                elseif subtype == "BDF+D"
                    edfhdr.bdfplus = true
                    edfhdr.discontinuous = true
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
            pblock.physdimension = convert(String, hdrbuf[pdimpos:pdimpos+7] # physical dimensions eg. "uV"
            pminpos = 257 + edfhdr.edfsignals * 104 + (i-1) * 8
            pblock.phys_min = parse(Float32, hdrbuf[pminpos, pminpos+8])  # physical minimum in above dimensions
            pmaxpos = 257 + edfhdr.edfsignals * 112 + (i-1) * 8
            pblock.phys_max = parse(Float32, hdrbuf[pmaxpos, pmaxpos+8])  # physical maximum in above dimensions
            pdminpos = 257 + edfhdr.edfsignals * 120 + (i-1) * 8
            pblock.dig_min = parse(Float32, hdrbuf[pdminpos, pdminpos+8])  # digital minimum in above dimensions
            pdmaxpos = 257 + edfhdr.edfsignals * 128 + (i-1) * 8
            pblock.dig_max = parse(Float32, hdrbuf[pdmaxpos, pdmaxpos+8])  # digital maximum in above dimensions
            if edfhdr.edfplus && channellabel[1:16] != "EDF Annotations "
                edfhdr.annot_ch[edfhdr.nr_annot_chns] = i-1
                edfhdr.nr_annot_chns += 1
                pblock.annotation = true
            elseif edfhdr.bdfplus && channellabel[1:16] != "BDF Annotations "
                edfhdr.annot_ch[edfhdr.nr_annot_chns] = i-1
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
            pfblock = hdrbuf[startpos:startpos+80])
            if hasforbiddenchars(pfblock)
                throw("bad bytes for prefilter fields")
            else
                edfhdr.edfparam[i].prefilter = convert(String, pfblock)  # prefiltering eg "HP:DC"
                if (edfhdr.edfplus || edfhdr.bdfplus) && edfhdf.edfparam[i].annotation
                    && match(edfhdr.edfparam[i].prefilter, r"\S") != nothing
                    throw("Prefilter field should be blank in annotations in edfplus or bdfplus file")
                end
            end
            startpos += 80
            edfhdr.edfparam[i].smp_per_record = parse(Int, hdrbuf[startpos:startpos+8])
            if edfhdr.edfparam[i].smp_per_record < 1
                throw("Samples per data record should be a positive integer")
            else
                edfhdr.recordsize += n                          # edfhdr.recordsize is calculated
                if edfhdr.bdf
                    edfhdr.recordsize *= 3
                    if edfhdr.recordsize > 1578640
                        throw("record size too large for a bdf file")
                    end
                else
                    edfhdr.recordsize *= 2
                    if edfhdr.recordsize > 10485760
                        throw("Record size too large for an edf file")
                    end
                end
            end
            startpos += 8
            edfhdr.edfparam[i].reserved = convert(String, hdrbuf[startpos:startpos+32]) # reserved (ascii)
            if hasforbiddenchars(pfblock)
                throw("bad bytes for reserved per-channel field")
            end
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
        if subfield[3] = "X"
            edfhdr.plus_birthdate = ""
        elseif !contains("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC", subfield[3][4:6])
            || !(Date(subfield[3], "dd-uuu-yyyy") isa Date)
            throw("Bad birthdate field in patient identification")
        else
            edfhdr.plus_birthdate = subfield[3]
        end
        edfhdr.plus_patient_name = replace(subfield[4], "_", " ")
        if length(subfield > 4
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
              !contains("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC", subfield[2][4:6]))
        throw("Bad recording field start")
    else
        if subfield[2] == "X"
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




static int edflib_is_integer_number(char *str)
{
  int i=0, l, hasspace = 0, hassign=0, digit=0

  l = strlen(str)

  if(!l)  return(1)

  if((str[0]=='+')||(str[0]=='-'))
  {
    hassign++
    i++
  }

  for( i<l i++)
  {
    if(str[i]==' ')
    {
      if(!digit)
      {
        return(1)
      }
      hasspace++
    }
    else
    {
      if((str[i]<48)||(str[i]>57))
      {
        return(1)
      }
      else
      {
        if(hasspace)
        {
          return(1)
        }
        digit++
      }
    }
  }

  if(digit)  return(0)
  else  return(1)
}



static int edflib_is_number(char *str)
{
  int i=0, l, hasspace = 0, hassign=0, digit=0, hasdot=0, hasexp=0

  l = strlen(str)

  if(!l)  return(1)

  if((str[0]=='+')||(str[0]=='-'))
  {
    hassign++
    i++
  }

  for( i<l i++)
  {
    if((str[i]=='e')||(str[i]=='E'))
    {
      if((!digit)||hasexp)
      {
        return(1)
      }
      hasexp++
      hassign = 0
      digit = 0

      break
    }

    if(str[i]==' ')
    {
      if(!digit)
      {
        return(1)
      }
      hasspace++
    }
    else
    {
      if(((str[i]<48)||(str[i]>57))&&str[i]!='.')
      {
        return(1)
      }
      else
      {
        if(hasspace)
        {
          return(1)
        }
        if(str[i]=='.')
        {
          if(hasdot)  return(1)
          hasdot++
        }
        else
        {
          digit++
        }
      }
    }
  }

  if(hasexp)
  {
    if(++i==l)
    {
      return(1)
    }

    if((str[i]=='+')||(str[i]=='-'))
    {
      hassign++
      i++
    }

    for( i<l i++)
    {
      if(str[i]==' ')
      {
        if(!digit)
        {
          return(1)
        }
        hasspace++
      }
      else
      {
        if((str[i]<48)||(str[i]>57))
        {
          return(1)
        }
        else
        {
          if(hasspace)
          {
            return(1)
          }

          digit++
        }
      }
    }
  }

  if(digit)  return(0)
  else  return(1)
}


static long long edflib_get_long_duration(char *str)
{
  int i, len=8, hasdot=0, dotposition=0

  long long value=0, radix

  if((str[0] == '+') || (str[0] == '-'))
  {
    for(i=0 i<7 i++)
    {
      str[i] = str[i+1]
    }

    str[7] = ' '
  }

  for(i=0 i<8 i++)
  {
    if(str[i]==' ')
    {
      len = i
      break
    }
  }

  for(i=0 i<len i++)
  {
    if(str[i]=='.')
    {
      hasdot = 1
      dotposition = i
      break
    }
  }

  if(hasdot)
  {
    radix = EDFLIB_TIME_DIMENSION

    for(i=dotposition-1 i>=0 i--)
    {
        value += ((long long)(str[i] - 48)) * radix
        radix *= 10
    }

    radix = EDFLIB_TIME_DIMENSION / 10

    for(i=dotposition+1 i<len i++)
    {
        value += ((long long)(str[i] - 48)) * radix
        radix /= 10
    }
  }
  else
  {
    radix = EDFLIB_TIME_DIMENSION

    for(i=len-1 i>=0 i--)
    {
        value += ((long long)(str[i] - 48)) * radix
        radix *= 10
    }
  }

  return(value)
}


int edflib_version(void)
{
  return(EDFLIB_VERSION)
}


static int edflib_get_annotations(struct edfhdrblock *edfhdr, int hdl, int read_annotations)
{
  int i, j, k, p, r=0, n,
      edfsignals,
      datarecords,
      recordsize,
      discontinuous,
      *annot_ch,
      nr_annot_chns,
      max,
      onset,
      duration,
      duration_start,
      zero,
      max_tal_ln,
      error,
      annots_in_record,
      annots_in_tal,
      samplesize=2

  char *scratchpad,
       *cnv_buf,
       *time_in_txt,
       *duration_in_txt


  long long data_record_duration,
            elapsedtime,
            time_tmp=0

  FILE *inputfile

  struct edfparamblock *edfparam

  struct edf_annotationblock *new_annotation=NULL,
                             *malloc_list

  inputfile = edfhdr.file_hdl
  edfsignals = edfhdr.edfsignals
  recordsize = edfhdr.recordsize
  edfparam = edfhdr.edfparam
  nr_annot_chns = edfhdr.nr_annot_chns
  datarecords = edfhdr.datarecords
  data_record_duration = edfhdr.long_data_record_duration
  discontinuous = edfhdr.discontinuous
  annot_ch = edfhdr.annot_ch

  if(edfhdr.edfplus)
  {
    samplesize = 2
  }
  if(edfhdr.bdfplus)
  {
    samplesize = 3
  }

  cnv_buf = (char *)calloc(1, recordsize)
  if(cnv_buf==NULL)
  {
    return(1)
  }

  max_tal_ln = 0

  for(i=0 i<nr_annot_chns i++)
  {
    if(max_tal_ln<edfparam[annot_ch[i]].smp_per_record * samplesize)  max_tal_ln = edfparam[annot_ch[i]].smp_per_record * samplesize
  }

  if(max_tal_ln<128)  max_tal_ln = 128

  scratchpad = (char *)calloc(1, max_tal_ln + 3)
  if(scratchpad==NULL)
  {
    free(cnv_buf)
    return(1)
  }

  time_in_txt = (char *)calloc(1, max_tal_ln + 3)
  if(time_in_txt==NULL)
  {
    free(cnv_buf)
    free(scratchpad)
    return(1)
  }

  duration_in_txt = (char *)calloc(1, max_tal_ln + 3)
  if(duration_in_txt==NULL)
  {
    free(cnv_buf)
    free(scratchpad)
    free(time_in_txt)
    return(1)
  }

  if(fseeko(inputfile, (long long)((edfsignals + 1) * 256), SEEK_SET))
  {
    free(cnv_buf)
    free(scratchpad)
    free(time_in_txt)
    free(duration_in_txt)
    return(2)
  }

  elapsedtime = 0

  for(i=0 i<datarecords i++)
  {
    if(fread(cnv_buf, recordsize, 1, inputfile)!=1)
    {
      free(cnv_buf)
      free(scratchpad)
      free(time_in_txt)
      free(duration_in_txt)
      return(2)
    }


/************** process annotationsignals (if any) **************/

    error = 0

    for(r=0 r<nr_annot_chns r++)
    {
      n = 0
      zero = 0
      onset = 0
      duration = 0
      duration_start = 0
      scratchpad[0] = 0
      annots_in_tal = 0
      annots_in_record = 0

      p = edfparam[annot_ch[r]].buf_offset
      max = edfparam[annot_ch[r]].smp_per_record * samplesize

/************** process one annotation signal ****************/

      if(cnv_buf[p + max - 1]!=0)
      {
        error = 5
        goto END
      }

      if(!r)  /* if it's the first annotation signal, then check */
      {       /* the timekeeping annotation */
        error = 1

        for(k=0 k<(max-2) k++)
        {
          scratchpad[k] = cnv_buf[p + k]

          if(scratchpad[k]==20)
          {
            if(cnv_buf[p + k + 1]!=20)
            {
              error = 6
              goto END
            }
            scratchpad[k] = 0
            if(edflib_is_onset_number(scratchpad))
            {
              error = 36
              goto END
            }
            else
            {
              time_tmp = edflib_get_long_time(scratchpad)
              if(i)
              {
                if(discontinuous)
                {
                  if((time_tmp-elapsedtime)<data_record_duration)
                  {
                    error = 4
                    goto END
                  }
                }
                else
                {
                  if((time_tmp-elapsedtime)!=data_record_duration)
                  {
                    error = 3
                    goto END
                  }
                }
              }
              else
              {
                if(time_tmp>=EDFLIB_TIME_DIMENSION)
                {
                  error = 2
                  goto END
                }
                else
                {
                  edfhdr.starttime_offset = time_tmp
                }
              }
              elapsedtime = time_tmp
              error = 0
              break
            }
          }
        }
      }

      for(k=0 k<max k++)
      {
        scratchpad[n] = cnv_buf[p + k]

        if(!scratchpad[n])
        {
          if(!zero)
          {
            if(k)
            {
              if(cnv_buf[p + k - 1]!=20)
              {
                error = 33
                goto END
              }
            }
            n = 0
            onset = 0
            duration = 0
            duration_start = 0
            scratchpad[0] = 0
            annots_in_tal = 0
          }
          zero++
          continue
        }
        if(zero>1)
        {
          error = 34
          goto END
        }
        zero = 0

        if((scratchpad[n]==20)||(scratchpad[n]==21))
        {
          if(scratchpad[n]==21)
          {
            if(duration||duration_start||onset||annots_in_tal)
            {               /* it's not allowed to have multiple duration fields */
              error = 35   /* in one TAL or to have a duration field which is   */
              goto END     /* not immediately behind the onsetfield             */
            }
            duration_start = 1
          }

          if((scratchpad[n]==20)&&onset&&(!duration_start))
          {
            if(r||annots_in_record)
            {
              if(n >= 0)
              {
                if(edfhdr.annots_in_file >= edfhdr.annotlist_sz)
                {
                  malloc_list = (struct edf_annotationblock *)realloc(annotationslist[hdl],
                                                                      sizeof(struct edf_annotationblock) * (edfhdr.annotlist_sz + EDFLIB_ANNOT_MEMBLOCKSZ))
                  if(malloc_list==NULL)
                  {
                    free(cnv_buf)
                    free(scratchpad)
                    free(time_in_txt)
                    free(duration_in_txt)
                    return(-1)
                  }

                  annotationslist[hdl] = malloc_list

                  edfhdr.annotlist_sz += EDFLIB_ANNOT_MEMBLOCKSZ
                }

                new_annotation = annotationslist[hdl] + edfhdr.annots_in_file

                new_annotation.annotation[0] = 0

                if(duration)  strcpy(new_annotation.duration, duration_in_txt)
                else  new_annotation.duration[0] = 0

                for(j=0 j<n j++)
                {
                  if(j==EDFLIB_MAX_ANNOTATION_LEN)  break
                  new_annotation.annotation[j] = scratchpad[j]
                }
                new_annotation.annotation[j] = 0

                new_annotation.onset = edflib_get_long_time(time_in_txt)

                edfhdr.annots_in_file++

                if(read_annotations==EDFLIB_READ_ANNOTATIONS)
                {
                  if(!(strncmp(new_annotation.annotation, "Recording ends", 14)))
                  {
                    if(nr_annot_chns==1)
                    {
                      goto END
                    }
                  }
                }
              }
            }

            annots_in_tal++
            annots_in_record++
            n = 0
            continue
          }

          if(!onset)
          {
            scratchpad[n] = 0
            if(edflib_is_onset_number(scratchpad))
            {
              error = 36
              goto END
            }
            onset = 1
            n = 0
            strcpy(time_in_txt, scratchpad)
            continue
          }

          if(duration_start)
          {
            scratchpad[n] = 0
            if(edflib_is_duration_number(scratchpad))
            {
              error = 37
              goto END
            }

            for(j=0 j<n j++)
            {
              if(j==15)  break
              duration_in_txt[j] = scratchpad[j]
              if((duration_in_txt[j]<32)||(duration_in_txt[j]>126))
              {
                duration_in_txt[j] = '.'
              }
            }
            duration_in_txt[j] = 0

            duration = 1
            duration_start = 0
            n = 0
            continue
          }
        }

        n++
      }

 END:

/****************** end ************************/

      if(error)
      {
        free(cnv_buf)
        free(scratchpad)
        free(time_in_txt)
        free(duration_in_txt)
        return(9)
      }
    }
  }

  free(cnv_buf)
  free(scratchpad)
  free(time_in_txt)
  free(duration_in_txt)

  return(0)
}


static int edflib_is_duration_number(char *str)
{
  int i, l, hasdot = 0

  l = strlen(str)

  if(!l)  return(1)

  if((str[0] == '.')||(str[l-1] == '.'))  return(1)

  for(i=0 i<l i++)
  {
    if(str[i]=='.')
    {
      if(hasdot)  return(1)
      hasdot++
    }
    else
    {
      if((str[i]<48)||(str[i]>57))  return(1)
    }
  }

  return(0)
}



static int edflib_is_onset_number(char *str)
{
  int i, l, hasdot = 0

  l = strlen(str)

  if(l<2)  return(1)

  if((str[0]!='+')&&(str[0]!='-'))  return(1)

  if((str[1] == '.')||(str[l-1] == '.'))  return(1)

  for(i=1 i<l i++)
  {
    if(str[i]=='.')
    {
      if(hasdot)  return(1)
      hasdot++
    }
    else
    {
      if((str[i]<48)||(str[i]>57))  return(1)
    }
  }

  return(0)
}



static long long edflib_get_long_time(char *str)
{
  int i, len, hasdot=0, dotposition=0

  long long value=0, radix

  str = str + 1

  len = strlen(str)

  for(i=0 i<len i++)
  {
    if(str[i]=='.')
    {
      hasdot = 1
      dotposition = i
      break
    }
  }

  if(hasdot)
  {
    radix = EDFLIB_TIME_DIMENSION

    for(i=dotposition-1 i>=0 i--)
    {
        value += ((long long)(str[i] - 48)) * radix
        radix *= 10
    }

    radix = EDFLIB_TIME_DIMENSION / 10

    for(i=dotposition+1 i<len i++)
    {
        value += ((long long)(str[i] - 48)) * radix
        radix /= 10
    }
  }
  else
  {
    radix = EDFLIB_TIME_DIMENSION

    for(i=len-1 i>=0 i--)
    {
        value += ((long long)(str[i] - 48)) * radix
        radix *= 10
    }
  }

  if(str[-1]=='-')  value = -value

  return(value)
}


static void edflib_latin1_to_ascii(char *str, int len)
{
  int i, value

  for(i=0 i<len i++)
  {
    value = *((unsigned char *)(str + i))

    if((value>31)&&(value<127))
    {
      continue
    }

    switch(value)
    {
      case 128 : str[i] = 'E'  break

      case 130 : str[i] = ','  break

      case 131 : str[i] = 'F'  break

      case 132 : str[i] = '\"'  break

      case 133 : str[i] = '.'  break

      case 134 : str[i] = '+'  break

      case 135 : str[i] = '+'  break

      case 136 : str[i] = '^'  break

      case 137 : str[i] = 'm'  break

      case 138 : str[i] = 'S'  break

      case 139 : str[i] = '<'  break

      case 140 : str[i] = 'E'  break

      case 142 : str[i] = 'Z'  break

      case 145 : str[i] = '`'  break

      case 146 : str[i] = '\''  break

      case 147 : str[i] = '\"'  break

      case 148 : str[i] = '\"'  break

      case 149 : str[i] = '.'  break

      case 150 : str[i] = '-'  break

      case 151 : str[i] = '-'  break

      case 152 : str[i] = '~'  break

      case 154 : str[i] = 's'  break

      case 155 : str[i] = '>'  break

      case 156 : str[i] = 'e'  break

      case 158 : str[i] = 'z'  break

      case 159 : str[i] = 'Y'  break

      case 171 : str[i] = '<'  break

      case 180 : str[i] = '\''  break

      case 181 : str[i] = 'u'  break

      case 187 : str[i] = '>'  break

      case 191 : str[i] = '\?'  break

      case 192 : str[i] = 'A'  break

      case 193 : str[i] = 'A'  break

      case 194 : str[i] = 'A'  break

      case 195 : str[i] = 'A'  break

      case 196 : str[i] = 'A'  break

      case 197 : str[i] = 'A'  break

      case 198 : str[i] = 'E'  break

      case 199 : str[i] = 'C'  break

      case 200 : str[i] = 'E'  break

      case 201 : str[i] = 'E'  break

      case 202 : str[i] = 'E'  break

      case 203 : str[i] = 'E'  break

      case 204 : str[i] = 'I'  break

      case 205 : str[i] = 'I'  break

      case 206 : str[i] = 'I'  break

      case 207 : str[i] = 'I'  break

      case 208 : str[i] = 'D'  break

      case 209 : str[i] = 'N'  break

      case 210 : str[i] = 'O'  break

      case 211 : str[i] = 'O'  break

      case 212 : str[i] = 'O'  break

      case 213 : str[i] = 'O'  break

      case 214 : str[i] = 'O'  break

      case 215 : str[i] = 'x'  break

      case 216 : str[i] = 'O'  break

      case 217 : str[i] = 'U'  break

      case 218 : str[i] = 'U'  break

      case 219 : str[i] = 'U'  break

      case 220 : str[i] = 'U'  break

      case 221 : str[i] = 'Y'  break

      case 222 : str[i] = 'I'  break

      case 223 : str[i] = 's'  break

      case 224 : str[i] = 'a'  break

      case 225 : str[i] = 'a'  break

      case 226 : str[i] = 'a'  break

      case 227 : str[i] = 'a'  break

      case 228 : str[i] = 'a'  break

      case 229 : str[i] = 'a'  break

      case 230 : str[i] = 'e'  break

      case 231 : str[i] = 'c'  break

      case 232 : str[i] = 'e'  break

      case 233 : str[i] = 'e'  break

      case 234 : str[i] = 'e'  break

      case 235 : str[i] = 'e'  break

      case 236 : str[i] = 'i'  break

      case 237 : str[i] = 'i'  break

      case 238 : str[i] = 'i'  break

      case 239 : str[i] = 'i'  break

      case 240 : str[i] = 'd'  break

      case 241 : str[i] = 'n'  break

      case 242 : str[i] = 'o'  break

      case 243 : str[i] = 'o'  break

      case 244 : str[i] = 'o'  break

      case 245 : str[i] = 'o'  break

      case 246 : str[i] = 'o'  break

      case 247 : str[i] = '-'  break

      case 248 : str[i] = '0'  break

      case 249 : str[i] = 'u'  break

      case 250 : str[i] = 'u'  break

      case 251 : str[i] = 'u'  break

      case 252 : str[i] = 'u'  break

      case 253 : str[i] = 'y'  break

      case 254 : str[i] = 't'  break

      case 255 : str[i] = 'y'  break

      default  : str[i] = ' '  break
    }
  }
}


static void edflib_latin12utf8(char *latin1_str, int len)
{
  int i, j

  unsigned char *str, tmp_str[512]


  str = (unsigned char *)latin1_str

  j = 0

  for(i=0 i<len i++)
  {
    if(str[i]==0)
    {
      tmp_str[j] = 0

      break
    }

    tmp_str[j] = str[i]

    if(str[i]<32) tmp_str[j] = '.'

    if((str[i]>126)&&(str[i]<160))  tmp_str[j] = '.'

    if(str[i]>159)
    {
      if((len-j)<2)
      {
        tmp_str[j] = ' '
      }
      else
      {
        tmp_str[j] = 192 + (str[i]>>6)
        j++
        tmp_str[j] = 128 + (str[i]&63)
      }
    }

    j++

    if(j>=len)  break
  }

  for(i=0 i<len i++)
  {
    str[i] = tmp_str[i]
  }
}


int edfopen_file_writeonly(const char *path, int filetype, int number_of_signals)
{
  int i, handle

  FILE *file

  struct edfhdrblock *hdr


  if((filetype!=EDFLIB_FILETYPE_EDFPLUS)&&(filetype!=EDFLIB_FILETYPE_BDFPLUS))
  {
    return(EDFLIB_FILETYPE_ERROR)
  }

  if(edf_files_open>=EDFLIB_MAXFILES)
  {
    return(EDFLIB_MAXFILES_REACHED)
  }

  for(i=0 i<EDFLIB_MAXFILES i++)
  {
    if(hdrlist[i]!=NULL)
    {
      if(!(strcmp(path, hdrlist[i].path)))
      {
        return(EDFLIB_FILE_ALREADY_OPENED)
      }
    }
  }

  if(number_of_signals<0)
  {
    return(EDFLIB_NUMBER_OF_SIGNALS_INVALID)
  }

  if(number_of_signals>EDFLIB_MAXSIGNALS)
  {
    return(EDFLIB_NUMBER_OF_SIGNALS_INVALID)
  }

  hdr = (struct edfhdrblock *)calloc(1, sizeof(struct edfhdrblock))
  if(hdr==NULL)
  {
    return(EDFLIB_MALLOC_ERROR)
  }

  hdr.edfparam = (struct edfparamblock *)calloc(1, sizeof(struct edfparamblock) * number_of_signals)
  if(hdr.edfparam==NULL)
  {
    free(hdr)

    return(EDFLIB_MALLOC_ERROR)
  }

  hdr.writemode = 1

  hdr.edfsignals = number_of_signals

  handle = -1

  for(i=0 i<EDFLIB_MAXFILES i++)
  {
    if(hdrlist[i]==NULL)
    {
      hdrlist[i] = hdr

      handle = i

      break
    }
  }

  if(handle<0)
  {
    free(hdr.edfparam)

    free(hdr)

    return(EDFLIB_MAXFILES_REACHED)
  }

  write_annotationslist[handle] = NULL

  hdr.annotlist_sz = 0

  hdr.annots_in_file = 0

  file = fopeno(path, "wb")
  if(file==NULL)
  {
    free(hdr.edfparam)

    free(hdr)

    return(EDFLIB_NO_SUCH_FILE_OR_DIRECTORY)
  }

  hdr.file_hdl = file

  strcpy(hdr.path, path)

  edf_files_open++

  if(filetype==EDFLIB_FILETYPE_EDFPLUS)
  {
    hdr.edf = 1
    hdr.edfplus = 1
  }

  if(filetype==EDFLIB_FILETYPE_BDFPLUS)
  {
    hdr.bdf = 1
    hdr.bdfplus = 1
  }

  hdr.long_data_record_duration = EDFLIB_TIME_DIMENSION

  hdr.data_record_duration = 1.0

  hdr.nr_annot_chns = 1

  return(handle)
}


int edf_set_samplefrequency(int handle, int edfsignal, int samplefrequency)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(edfsignal<0)
  {
    return(-1)
  }

  if(edfsignal>=hdrlist[handle].edfsignals)
  {
    return(-1)
  }

  if(samplefrequency<1)
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  hdrlist[handle].edfparam[edfsignal].smp_per_record = samplefrequency

  return(0)
}


int edf_set_number_of_annotation_signals(int handle, int annot_signals)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  if((annot_signals < 1) || (annot_signals > 64))
  {
    return(-1)
  }

  hdrlist[handle].nr_annot_chns = annot_signals

  return(0)
}


int edf_set_datarecord_duration(int handle, int duration)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  if((duration < 100) || (duration > 6000000))
  {
    return(-1)
  }

  hdrlist[handle].long_data_record_duration = (long long)duration * 100LL

  if(hdrlist[handle].long_data_record_duration < (EDFLIB_TIME_DIMENSION * 10LL))
  {
    hdrlist[handle].long_data_record_duration /= 10LL

    hdrlist[handle].long_data_record_duration *= 10LL
  }
  else
  {
    hdrlist[handle].long_data_record_duration /= 100LL

    hdrlist[handle].long_data_record_duration *= 100LL
  }

  hdrlist[handle].data_record_duration = ((double)(hdrlist[handle].long_data_record_duration)) / EDFLIB_TIME_DIMENSION

  return(0)
}


int edfwrite_digital_short_samples(int handle, short *buf)
{
  int  i, p,
       error,
       sf,
       digmax,
       digmin,
       edfsignal,
       value

  FILE *file

  struct edfhdrblock *hdr


  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].edfsignals == 0)
  {
    return(-1)
  }

  if(hdrlist[handle].bdf == 1)
  {
    return(-1)
  }

  hdr = hdrlist[handle]

  file = hdr.file_hdl

  edfsignal = hdr.signal_write_sequence_pos

  if(!hdr.datarecords)
  {
    if(!edfsignal)
    {
      error = edflib_write_edf_header(hdr)

      if(error)
      {
        return(error)
      }
    }
  }

  sf = hdr.edfparam[edfsignal].smp_per_record

  digmax = hdr.edfparam[edfsignal].dig_max

  digmin = hdr.edfparam[edfsignal].dig_min

  for(i=0 i<sf i++)
  {
    value = buf[i]

    if(value>digmax)
    {
      value = digmax
    }

    if(value<digmin)
    {
      value = digmin
    }

    fputc((value)&0xff, file)

    if(fputc((value>>8)&0xff, file)==EOF)
    {
      return(-1)
    }

    if(hdr.bdf)
    {
      fputc((value>>16)&0xff, file)
    }

  }

  hdr.signal_write_sequence_pos++

  if(hdr.signal_write_sequence_pos == hdr.edfsignals)
  {
    hdr.signal_write_sequence_pos = 0

    p = edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) / EDFLIB_TIME_DIMENSION, 0, 1)
    if(hdr.long_data_record_duration % EDFLIB_TIME_DIMENSION)
    {
      fputc('.', file)
      p++
      p += edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) % EDFLIB_TIME_DIMENSION, 7, 0)
    }
    fputc(20, file)
    fputc(20, file)
    p += 2
    for( p<hdr.total_annot_bytes p++)
    {
      fputc(0, file)
    }

    hdr.datarecords++

    fflush(file)
  }

  return(0)
}


int edfwrite_digital_samples(int handle, int *buf)
{
  int  i, p,
       error,
       sf,
       digmax,
       digmin,
       edfsignal,
       value

  FILE *file

  struct edfhdrblock *hdr


  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].edfsignals == 0)
  {
    return(-1)
  }

  hdr = hdrlist[handle]

  file = hdr.file_hdl

  edfsignal = hdr.signal_write_sequence_pos

  if(!hdr.datarecords)
  {
    if(!edfsignal)
    {
      error = edflib_write_edf_header(hdr)

      if(error)
      {
        return(error)
      }
    }
  }

  sf = hdr.edfparam[edfsignal].smp_per_record

  digmax = hdr.edfparam[edfsignal].dig_max

  digmin = hdr.edfparam[edfsignal].dig_min

  for(i=0 i<sf i++)
  {
    value = buf[i]

    if(value>digmax)
    {
      value = digmax
    }

    if(value<digmin)
    {
      value = digmin
    }

    fputc((value)&0xff, file)

    if(fputc((value>>8)&0xff, file)==EOF)
    {
      return(-1)
    }

    if(hdr.bdf)
    {
      fputc((value>>16)&0xff, file)
    }
  }

  hdr.signal_write_sequence_pos++

  if(hdr.signal_write_sequence_pos == hdr.edfsignals)
  {
    hdr.signal_write_sequence_pos = 0

    p = edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) / EDFLIB_TIME_DIMENSION, 0, 1)
    if(hdr.long_data_record_duration % EDFLIB_TIME_DIMENSION)
    {
      fputc('.', file)
      p++
      p += edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) % EDFLIB_TIME_DIMENSION, 7, 0)
    }
    fputc(20, file)
    fputc(20, file)
    p += 2
    for( p<hdr.total_annot_bytes p++)
    {
      fputc(0, file)
    }

    hdr.datarecords++

    fflush(file)
  }

  return(0)
}


int edf_blockwrite_digital_samples(int handle, int *buf)
{
  int  i, j, p,
       error,
       sf,
       digmax,
       digmin,
       edfsignals,
       buf_offset,
       value

  FILE *file

  struct edfhdrblock *hdr


  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].signal_write_sequence_pos)
  {
    return(-1)
  }

  if(hdrlist[handle].edfsignals == 0)
  {
    return(-1)
  }

  hdr = hdrlist[handle]

  file = hdr.file_hdl

  edfsignals = hdr.edfsignals

  if(!hdr.datarecords)
  {
    error = edflib_write_edf_header(hdr)

    if(error)
    {
      return(error)
    }
  }

  buf_offset = 0

  for(j=0 j<edfsignals j++)
  {
    sf = hdr.edfparam[j].smp_per_record

    digmax = hdr.edfparam[j].dig_max

    digmin = hdr.edfparam[j].dig_min

    for(i=0 i<sf i++)
    {
      value = buf[i + buf_offset]

      if(value>digmax)
      {
        value = digmax
      }

      if(value<digmin)
      {
        value = digmin
      }

      fputc(value&0xff, file)

      if(fputc((value>>8)&0xff, file)==EOF)
      {
        return(-1)
      }

      if(hdr.bdf)
      {
        fputc((value>>16)&0xff, file)
      }
    }

    buf_offset += sf
  }

  p = edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) / EDFLIB_TIME_DIMENSION, 0, 1)
  if(hdr.long_data_record_duration % EDFLIB_TIME_DIMENSION)
  {
    fputc('.', file)
    p++
    p += edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) % EDFLIB_TIME_DIMENSION, 7, 0)
  }
  fputc(20, file)
  fputc(20, file)
  p += 2
  for( p<hdr.total_annot_bytes p++)
  {
    fputc(0, file)
  }

  hdr.datarecords++

  fflush(file)

  return(0)
}


int edf_blockwrite_digital_short_samples(int handle, short *buf)
{
  int  i, j, p,
       error,
       sf,
       digmax,
       digmin,
       edfsignals,
       buf_offset,
       value

  FILE *file

  struct edfhdrblock *hdr


  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].signal_write_sequence_pos)
  {
    return(-1)
  }

  if(hdrlist[handle].edfsignals == 0)
  {
    return(-1)
  }

  if(hdrlist[handle].bdf == 1)
  {
    return(-1)
  }

  hdr = hdrlist[handle]

  file = hdr.file_hdl

  edfsignals = hdr.edfsignals

  if(!hdr.datarecords)
  {
    error = edflib_write_edf_header(hdr)

    if(error)
    {
      return(error)
    }
  }

  buf_offset = 0

  for(j=0 j<edfsignals j++)
  {
    sf = hdr.edfparam[j].smp_per_record

    digmax = hdr.edfparam[j].dig_max

    digmin = hdr.edfparam[j].dig_min

    for(i=0 i<sf i++)
    {
      value = buf[i + buf_offset]

      if(value>digmax)
      {
        value = digmax
      }

      if(value<digmin)
      {
        value = digmin
      }

      fputc(value&0xff, file)

      if(fputc((value>>8)&0xff, file)==EOF)
      {
        return(-1)
      }

      if(hdr.bdf)
      {
        fputc((value>>16)&0xff, file)
      }
    }

    buf_offset += sf
  }

  p = edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) / EDFLIB_TIME_DIMENSION, 0, 1)
  if(hdr.long_data_record_duration % EDFLIB_TIME_DIMENSION)
  {
    fputc('.', file)
    p++
    p += edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) % EDFLIB_TIME_DIMENSION, 7, 0)
  }
  fputc(20, file)
  fputc(20, file)
  p += 2
  for( p<hdr.total_annot_bytes p++)
  {
    fputc(0, file)
  }

  hdr.datarecords++

  fflush(file)

  return(0)
}


int edf_blockwrite_digital_3byte_samples(int handle, void *buf)
{
  int  j, p,
       error,
       edfsignals,
       total_samples=0

  FILE *file

  struct edfhdrblock *hdr


  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].signal_write_sequence_pos)
  {
    return(-1)
  }

  if(hdrlist[handle].edfsignals == 0)
  {
    return(-1)
  }

  if(hdrlist[handle].bdf != 1)
  {
    return(-1)
  }

  hdr = hdrlist[handle]

  file = hdr.file_hdl

  edfsignals = hdr.edfsignals

  if(!hdr.datarecords)
  {
    error = edflib_write_edf_header(hdr)

    if(error)
    {
      return(error)
    }
  }

  for(j=0 j<edfsignals j++)
  {
    total_samples += hdr.edfparam[j].smp_per_record
  }

  if(fwrite(buf, total_samples * 3, 1, file) != 1)
  {
    return(-1)
  }

  p = edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) / EDFLIB_TIME_DIMENSION, 0, 1)
  if(hdr.long_data_record_duration % EDFLIB_TIME_DIMENSION)
  {
    fputc('.', file)
    p++
    p += edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) % EDFLIB_TIME_DIMENSION, 7, 0)
  }
  fputc(20, file)
  fputc(20, file)
  p += 2
  for( p<hdr.total_annot_bytes p++)
  {
    fputc(0, file)
  }

  hdr.datarecords++

  fflush(file)

  return(0)
}


int edfwrite_physical_samples(int handle, double *buf)
{
  int  i, p,
       error,
       sf,
       digmax,
       digmin,
       value,
       edfsignal

  double bitvalue,
         phys_offset

  FILE *file

  struct edfhdrblock *hdr




  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].edfsignals == 0)
  {
    return(-1)
  }

  hdr = hdrlist[handle]

  file = hdr.file_hdl

  edfsignal = hdr.signal_write_sequence_pos

  if(!hdr.datarecords)
  {
    if(!edfsignal)
    {
      error = edflib_write_edf_header(hdr)

      if(error)
      {
        return(error)
      }
    }
  }

  sf = hdr.edfparam[edfsignal].smp_per_record

  digmax = hdr.edfparam[edfsignal].dig_max

  digmin = hdr.edfparam[edfsignal].dig_min

  bitvalue = hdr.edfparam[edfsignal].bitvalue

  phys_offset = hdr.edfparam[edfsignal].offset

  for(i=0 i<sf i++)
  {
    value = (buf[i] / bitvalue) - phys_offset

    if(value>digmax)
    {
      value = digmax
    }

    if(value<digmin)
    {
      value = digmin
    }

    fputc(value&0xff, file)

    if(fputc((value>>8)&0xff, file)==EOF)
    {
      return(-1)
    }

    if(hdr.bdf)
    {
      fputc((value>>16)&0xff, file)
    }
  }

  hdr.signal_write_sequence_pos++

  if(hdr.signal_write_sequence_pos == hdr.edfsignals)
  {
    hdr.signal_write_sequence_pos = 0

    p = edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) / EDFLIB_TIME_DIMENSION, 0, 1)
    if(hdr.long_data_record_duration % EDFLIB_TIME_DIMENSION)
    {
      fputc('.', file)
      p++
      p += edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) % EDFLIB_TIME_DIMENSION, 7, 0)
    }
    fputc(20, file)
    fputc(20, file)
    p += 2
    for( p<hdr.total_annot_bytes p++)
    {
      fputc(0, file)
    }

    hdr.datarecords++

    fflush(file)
  }

  return(0)
}


int edf_blockwrite_physical_samples(int handle, double *buf)
{
  int  i, j, p,
       error,
       sf,
       digmax,
       digmin,
       edfsignals,
       buf_offset,
       value

  double bitvalue,
         phys_offset

  FILE *file

  struct edfhdrblock *hdr




  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].signal_write_sequence_pos)
  {
    return(-1)
  }

  if(hdrlist[handle].edfsignals == 0)
  {
    return(-1)
  }

  hdr = hdrlist[handle]

  file = hdr.file_hdl

  edfsignals = hdr.edfsignals

  if(!hdr.datarecords)
  {
    error = edflib_write_edf_header(hdr)

    if(error)
    {
      return(error)
    }
  }

  buf_offset = 0

  for(j=0 j<edfsignals j++)
  {
    sf = hdr.edfparam[j].smp_per_record

    digmax = hdr.edfparam[j].dig_max

    digmin = hdr.edfparam[j].dig_min

    bitvalue = hdr.edfparam[j].bitvalue

    phys_offset = hdr.edfparam[j].offset

    for(i=0 i<sf i++)
    {
      value = (buf[i + buf_offset] / bitvalue) - phys_offset

      if(value>digmax)
      {
        value = digmax
      }

      if(value<digmin)
      {
        value = digmin
      }

      fputc(value&0xff, file)

      if(fputc((value>>8)&0xff, file)==EOF)
      {
        return(-1)
      }

      if(hdr.bdf)
      {
        fputc((value>>16)&0xff, file)
      }
    }

    buf_offset += sf
  }

  p = edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) / EDFLIB_TIME_DIMENSION, 0, 1)
  if(hdr.long_data_record_duration % EDFLIB_TIME_DIMENSION)
  {
    fputc('.', file)
    p++
    p += edflib_fprint_ll_number_nonlocalized(file, (hdr.datarecords * hdr.long_data_record_duration) % EDFLIB_TIME_DIMENSION, 7, 0)
  }
  fputc(20, file)
  fputc(20, file)
  p += 2
  for( p<hdr.total_annot_bytes p++)
  {
    fputc(0, file)
  }

  hdr.datarecords++

  fflush(file)

  return(0)
}


static int edflib_write_edf_header(struct edfhdrblock *hdr)
{
  int i, j, p, q,
      len,
      rest,
      edfsignals

  char str[128]

  struct tm *date_time

  time_t elapsed_time

  FILE *file


  file = hdr.file_hdl

  edfsignals = hdr.edfsignals

  if(edfsignals<0)
  {
    return(-20)
  }

  if(edfsignals>EDFLIB_MAXSIGNALS)
  {
    return(-21)
  }

  hdr.eq_sf = 1

  for(i=0 i<edfsignals i++)
  {
    if(hdr.edfparam[i].smp_per_record<1)
    {
      return(-22)
    }

    if(hdr.edfparam[i].dig_max==hdr.edfparam[i].dig_min)
    {
      return(-23)
    }

    if(hdr.edfparam[i].dig_max<hdr.edfparam[i].dig_min)
    {
      return(-24)
    }

    if(hdr.edfparam[i].phys_max==hdr.edfparam[i].phys_min)
    {
      return(-25)
    }

    if(i > 0)
    {
      if(hdr.edfparam[i].smp_per_record != hdr.edfparam[i-1].smp_per_record)
      {
        hdr.eq_sf = 0
      }
    }
  }

  for(i=0 i<edfsignals i++)
  {
    hdr.edfparam[i].bitvalue = (hdr.edfparam[i].phys_max - hdr.edfparam[i].phys_min) / (hdr.edfparam[i].dig_max - hdr.edfparam[i].dig_min)
    hdr.edfparam[i].offset = hdr.edfparam[i].phys_max / hdr.edfparam[i].bitvalue - hdr.edfparam[i].dig_max
  }

  rewind(file)

  if(hdr.edf)
  {
    fprintf(file, "0       ")
  }
  else
  {
    fputc(255, file)
    fprintf(file, "BIOSEMI")
  }

  p = 0

  if(hdr.plus_birthdate[0]==0)
  {
    rest = 72
  }
  else
  {
    rest = 62
  }

  len = strlen(hdr.plus_patientcode)
  if(len && rest)
  {
    if(len>rest)
    {
      len = rest
      rest = 0
    }
    else
    {
      rest -= len
    }
    strcpy(str, hdr.plus_patientcode)
    edflib_latin1_to_ascii(str, len)
    str[len] = 0
    for(i=0 i<len i++)
    {
      if(str[i]==' ')
      {
        str[i] = '_'
      }
    }
    p += fprintf(file, "%s ", str)
  }
  else
  {
    p += fprintf(file, "X ")
  }

  if(hdr.plus_gender[0]=='M')
  {
    fputc('M', file)
  }
  else
  {
    if(hdr.plus_gender[0]=='F')
    {
      fputc('F', file)
    }
    else
    {
      fputc('X', file)
    }
  }
  fputc(' ', file)
  p +=2

  if(hdr.plus_birthdate[0]==0)
  {
    fputc('X', file)
    fputc(' ', file)

    p +=2
  }
  else
  {
    fputc(hdr.plus_birthdate[0], file)
    fputc(hdr.plus_birthdate[1], file)
    fputc('-', file)
    q = edflib_atof_nonlocalized(&(hdr.plus_birthdate[3]))
    switch(q)
    {
      case  1: fprintf(file, "JAN")  break
      case  2: fprintf(file, "FEB")  break
      case  3: fprintf(file, "MAR")  break
      case  4: fprintf(file, "APR")  break
      case  5: fprintf(file, "MAY")  break
      case  6: fprintf(file, "JUN")  break
      case  7: fprintf(file, "JUL")  break
      case  8: fprintf(file, "AUG")  break
      case  9: fprintf(file, "SEP")  break
      case 10: fprintf(file, "OCT")  break
      case 11: fprintf(file, "NOV")  break
      case 12: fprintf(file, "DEC")  break
    }
    fputc('-', file)
    fputc(hdr.plus_birthdate[6], file)
    fputc(hdr.plus_birthdate[7], file)
    fputc(hdr.plus_birthdate[8], file)
    fputc(hdr.plus_birthdate[9], file)
    fputc(' ', file)

    p += 12
  }

  len = strlen(hdr.plus_patient_name)
  if(len && rest)
  {
    if(len>rest)
    {
      len = rest
      rest = 0
    }
    else
    {
      rest -= len
    }
    strcpy(str, hdr.plus_patient_name)
    edflib_latin1_to_ascii(str, len)
    str[len] = 0
    for(i=0 i<len i++)
    {
      if(str[i]==' ')
      {
        str[i] = '_'
      }
    }
    p += fprintf(file, "%s ", str)
  }
  else
  {
    fputc('X', file)

    p++
  }

  len = strlen(hdr.plus_patient_additional)
  if(len && rest)
  {
    if(len>rest)
    {
      len = rest
    }
    strcpy(str, hdr.plus_patient_additional)
    edflib_latin1_to_ascii(str, len)
    str[len] = 0
    p += fprintf(file, "%s", str)
  }

  for( p<80 p++)
  {
    fputc(' ', file)
  }

  if(!hdr.startdate_year)
  {
    elapsed_time = time(NULL)
    date_time = localtime(&elapsed_time)

    hdr.startdate_year = date_time.tm_year + 1900
    hdr.startdate_month = date_time.tm_mon + 1
    hdr.startdate_day = date_time.tm_mday
    hdr.starttime_hour = date_time.tm_hour
    hdr.starttime_minute = date_time.tm_min
    hdr.starttime_second = date_time.tm_sec % 60
  }

  p = 0

  p += fprintf(file, "Startdate %02u-", hdr.startdate_day)
  switch(hdr.startdate_month)
  {
    case  1 : fprintf(file, "JAN")  break
    case  2 : fprintf(file, "FEB")  break
    case  3 : fprintf(file, "MAR")  break
    case  4 : fprintf(file, "APR")  break
    case  5 : fprintf(file, "MAY")  break
    case  6 : fprintf(file, "JUN")  break
    case  7 : fprintf(file, "JUL")  break
    case  8 : fprintf(file, "AUG")  break
    case  9 : fprintf(file, "SEP")  break
    case 10 : fprintf(file, "OCT")  break
    case 11 : fprintf(file, "NOV")  break
    case 12 : fprintf(file, "DEC")  break
  }
  p += 3
  fputc('-', file)
  p++
  p += edflib_fprint_int_number_nonlocalized(file, hdr.startdate_year, 4, 0)
  fputc(' ', file)
  p++

  rest = 42

  len = strlen(hdr.plus_admincode)
  if(len && rest)
  {
    if(len>rest)
    {
      len = rest
      rest = 0
    }
    else
    {
      rest -= len
    }
    strcpy(str, hdr.plus_admincode)
    edflib_latin1_to_ascii(str, len)
    str[len] = 0
    for(i=0 i<len i++)
    {
      if(str[i]==' ')
      {
        str[i] = '_'
      }
    }
    p += fprintf(file, "%s ", str)
  }
  else
  {
    p += fprintf(file, "X ")
  }

  len = strlen(hdr.plus_technician)
  if(len && rest)
  {
    if(len>rest)
    {
      len = rest
      rest = 0
    }
    else
    {
      rest -= len
    }
    strcpy(str, hdr.plus_technician)
    edflib_latin1_to_ascii(str, len)
    str[len] = 0
    for(i=0 i<len i++)
    {
      if(str[i]==' ')
      {
        str[i] = '_'
      }
    }
    p += fprintf(file, "%s ", str)
  }
  else
  {
    p += fprintf(file, "X ")
  }

  len = strlen(hdr.plus_equipment)
  if(len && rest)
  {
    if(len>rest)
    {
      len = rest
      rest = 0
    }
    else
    {
      rest -= len
    }
    strcpy(str, hdr.plus_equipment)
    edflib_latin1_to_ascii(str, len)
    str[len] = 0
    for(i=0 i<len i++)
    {
      if(str[i]==' ')
      {
        str[i] = '_'
      }
    }
    p += fprintf(file, "%s ", str)
  }
  else
  {
    p += fprintf(file, "X ")
  }

  len = strlen(hdr.plus_recording_additional)
  if(len && rest)
  {
    if(len>rest)
    {
      len = rest
    }
    strcpy(str, hdr.plus_recording_additional)
    edflib_latin1_to_ascii(str, len)
    str[len] = 0
    p += fprintf(file, "%s", str)
  }

  for( p<80 p++)
  {
    fputc(' ', file)
  }

  fprintf(file, "%02u.%02u.%02u", hdr.startdate_day, hdr.startdate_month, (hdr.startdate_year % 100))
  fprintf(file, "%02u.%02u.%02u", hdr.starttime_hour, hdr.starttime_minute, hdr.starttime_second)
  p = edflib_fprint_int_number_nonlocalized(file, (edfsignals + hdr.nr_annot_chns + 1) * 256, 0, 0)
  for( p<8 p++)
  {
    fputc(' ', file)
  }
  if(hdr.edf)
  {
    fprintf(file, "EDF+C")
  }
  else
  {
    fprintf(file, "BDF+C")
  }
  for(i=0 i<39 i++)
  {
    fputc(' ', file)
  }
  fprintf(file, "-1      ")
  if(hdr.long_data_record_duration == EDFLIB_TIME_DIMENSION)
  {
    fprintf(file, "1       ")
  }
  else
  {
    edflib_sprint_number_nonlocalized(str, hdr.data_record_duration)
    strcat(str, "        ")
    str[8] = 0
    fprintf(file, "%s", str)
  }
  p = edflib_fprint_int_number_nonlocalized(file, edfsignals + hdr.nr_annot_chns, 0, 0)
  for( p<4 p++)
  {
    fputc(' ', file)
  }

  for(i=0 i<edfsignals i++)
  {
    len = strlen(hdr.edfparam[i].label)
    edflib_latin1_to_ascii(hdr.edfparam[i].label, len)
    for(j=0 j<len j++)
    {
      fputc(hdr.edfparam[i].label[j], file)
    }
    for( j<16 j++)
    {
      fputc(' ', file)
    }
  }
  for(j=0 j<hdr.nr_annot_chns j++)
  {
    if(hdr.edf)
    {
      fprintf(file, "EDF Annotations ")
    }
    else
    {
      fprintf(file, "BDF Annotations ")
    }
  }
  for(i=0 i<edfsignals i++)
  {
    len = strlen(hdr.edfparam[i].transducer)
    edflib_latin1_to_ascii(hdr.edfparam[i].transducer, len)
    for(j=0 j<len j++)
    {
      fputc(hdr.edfparam[i].transducer[j], file)
    }
    for( j<80 j++)
    {
      fputc(' ', file)
    }
  }
  for(j=0 j<hdr.nr_annot_chns j++)
  {
    for(i=0 i<80 i++)
    {
      fputc(' ', file)
    }
  }
  for(i=0 i<edfsignals i++)
  {
    len = strlen(hdr.edfparam[i].physdimension)
    edflib_latin1_to_ascii(hdr.edfparam[i].physdimension, len)
    for(j=0 j<len j++)
    {
      fputc(hdr.edfparam[i].physdimension[j], file)
    }
    for( j<8 j++)
    {
      fputc(' ', file)
    }
  }
  for(j=0 j<hdr.nr_annot_chns j++)
  {
    fprintf(file, "        ")
  }
  for(i=0 i<edfsignals i++)
  {
    p = edflib_sprint_number_nonlocalized(str, hdr.edfparam[i].phys_min)
    for( p<8 p++)
    {
      str[p] = ' '
    }
    str[8] = 0
    fprintf(file, "%s", str)
  }
  for(j=0 j<hdr.nr_annot_chns j++)
  {
    fprintf(file, "-1      ")
  }
  for(i=0 i<edfsignals i++)
  {
    p = edflib_sprint_number_nonlocalized(str, hdr.edfparam[i].phys_max)
    for( p<8 p++)
    {
      str[p] = ' '
    }
    str[8] = 0
    fprintf(file, "%s", str)
  }
  for(j=0 j<hdr.nr_annot_chns j++)
  {
    fprintf(file, "1       ")
  }
  for(i=0 i<edfsignals i++)
  {
    p = edflib_fprint_int_number_nonlocalized(file, hdr.edfparam[i].dig_min, 0, 0)
    for( p<8 p++)
    {
      fputc(' ', file)
    }
  }
  for(j=0 j<hdr.nr_annot_chns j++)
  {
    if(hdr.edf)
    {
      fprintf(file, "-32768  ")
    }
    else
    {
      fprintf(file, "-8388608")
    }
  }
  for(i=0 i<edfsignals i++)
  {
    p = edflib_fprint_int_number_nonlocalized(file, hdr.edfparam[i].dig_max, 0, 0)
    for( p<8 p++)
    {
      fputc(' ', file)
    }
  }
  for(j=0 j<hdr.nr_annot_chns j++)
  {
    if(hdr.edf)
    {
      fprintf(file, "32767   ")
    }
    else
    {
      fprintf(file, "8388607 ")
    }
  }
  for(i=0 i<edfsignals i++)
  {
    len = strlen(hdr.edfparam[i].prefilter)
    edflib_latin1_to_ascii(hdr.edfparam[i].prefilter, len)
    for(j=0 j<len j++)
    {
      fputc(hdr.edfparam[i].prefilter[j], file)
    }
    for( j<80 j++)
    {
      fputc(' ', file)
    }
  }
  for(i=0 i<hdr.nr_annot_chns i++)
  {
    for(j=0 j<80 j++)
    {
      fputc(' ', file)
    }
  }
  for(i=0 i<edfsignals i++)
  {
    p = edflib_fprint_int_number_nonlocalized(file, hdr.edfparam[i].smp_per_record, 0, 0)
    for( p<8 p++)
    {
      fputc(' ', file)
    }
  }
  for(j=0 j<hdr.nr_annot_chns j++)
  {
    if(hdr.edf)
    {
      p = edflib_fprint_int_number_nonlocalized(file, EDFLIB_ANNOTATION_BYTES / 2, 0, 0)
      for( p<8 p++)
      {
        fputc(' ', file)
      }
    }
    else
    {
      p = edflib_fprint_int_number_nonlocalized(file, EDFLIB_ANNOTATION_BYTES / 3, 0, 0)
      for( p<8 p++)
      {
        fputc(' ', file)
      }
    }
  }
  for(i=0 i<(edfsignals * 32) i++)
  {
    fputc(' ', file)
  }
  for(i=0 i<(hdr.nr_annot_chns * 32) i++)
  {
    fputc(' ', file)
  }

  hdr.total_annot_bytes = EDFLIB_ANNOTATION_BYTES * hdr.nr_annot_chns

  return(0)
}


int edf_set_label(int handle, int edfsignal, const char *label)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(edfsignal<0)
  {
    return(-1)
  }

  if(edfsignal>=hdrlist[handle].edfsignals)
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].edfparam[edfsignal].label, label, 16)

  hdrlist[handle].edfparam[edfsignal].label[16] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].edfparam[edfsignal].label)

  return(0)
}


int edf_set_physical_dimension(int handle, int edfsignal, const char *phys_dim)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(edfsignal<0)
  {
    return(-1)
  }

  if(edfsignal>=hdrlist[handle].edfsignals)
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].edfparam[edfsignal].physdimension, phys_dim, 8)

  hdrlist[handle].edfparam[edfsignal].physdimension[8] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].edfparam[edfsignal].physdimension)

  return(0)
}


int edf_set_physical_maximum(int handle, int edfsignal, double phys_max)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(edfsignal<0)
  {
    return(-1)
  }

  if(edfsignal>=hdrlist[handle].edfsignals)
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  hdrlist[handle].edfparam[edfsignal].phys_max = phys_max

  return(0)
}


int edf_set_physical_minimum(int handle, int edfsignal, double phys_min)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(edfsignal<0)
  {
    return(-1)
  }

  if(edfsignal>=hdrlist[handle].edfsignals)
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  hdrlist[handle].edfparam[edfsignal].phys_min = phys_min

  return(0)
}


int edf_set_digital_maximum(int handle, int edfsignal, int dig_max)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(edfsignal<0)
  {
    return(-1)
  }

  if(edfsignal>=hdrlist[handle].edfsignals)
  {
    return(-1)
  }

  if(hdrlist[handle].edf)
  {
    if(dig_max > 32767)
    {
      return(-1)
    }
  }
  else
  {
    if(dig_max > 8388607)
    {
      return(-1)
    }
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  hdrlist[handle].edfparam[edfsignal].dig_max = dig_max

  return(0)
}


int edf_set_digital_minimum(int handle, int edfsignal, int dig_min)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(edfsignal<0)
  {
    return(-1)
  }

  if(edfsignal>=hdrlist[handle].edfsignals)
  {
    return(-1)
  }

  if(hdrlist[handle].edf)
  {
    if(dig_min < (-32768))
    {
      return(-1)
    }
  }
  else
  {
    if(dig_min < (-8388608))
    {
      return(-1)
    }
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  hdrlist[handle].edfparam[edfsignal].dig_min = dig_min

  return(0)
}


int edf_set_patientname(int handle, const char *patientname)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].plus_patient_name, patientname, 80)

  hdrlist[handle].plus_patient_name[80] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].plus_patient_name)

  return(0)
}


int edf_set_patientcode(int handle, const char *patientcode)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].plus_patientcode, patientcode, 80)

  hdrlist[handle].plus_patientcode[80] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].plus_patientcode)

  return(0)
}


int edf_set_gender(int handle, int gender)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  if((gender<0)||(gender>1))
  {
    return(-1)
  }

  if(gender)
  {
    hdrlist[handle].plus_gender[0] = 'M'
  }
  else
  {
    hdrlist[handle].plus_gender[0] = 'F'
  }

  hdrlist[handle].plus_gender[1] = 0

  return(0)
}


int edf_set_birthdate(int handle, int birthdate_year, int birthdate_month, int birthdate_day)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  if((birthdate_year<1800) || (birthdate_year>3000) ||
     (birthdate_month<1)   || (birthdate_month>12)  ||
     (birthdate_day<1)     || (birthdate_day>31))
  {
    return(-1)
  }

  sprintf(hdrlist[handle].plus_birthdate, "%02i.%02i.%02i%02i", birthdate_day, birthdate_month, birthdate_year / 100, birthdate_year % 100)

  hdrlist[handle].plus_birthdate[10] = 0

  return(0)
}


int edf_set_patient_additional(int handle, const char *patient_additional)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].plus_patient_additional, patient_additional, 80)

  hdrlist[handle].plus_patient_additional[80] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].plus_patient_additional)

  return(0)
}


int edf_set_admincode(int handle, const char *admincode)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].plus_admincode, admincode, 80)

  hdrlist[handle].plus_admincode[80] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].plus_admincode)

  return(0)
}


int edf_set_technician(int handle, const char *technician)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].plus_technician, technician, 80)

  hdrlist[handle].plus_technician[80] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].plus_technician)

  return(0)
}


int edf_set_equipment(int handle, const char *equipment)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].plus_equipment, equipment, 80)

  hdrlist[handle].plus_equipment[80] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].plus_equipment)

  return(0)
}


int edf_set_recording_additional(int handle, const char *recording_additional)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].plus_recording_additional, recording_additional, 80)

  hdrlist[handle].plus_recording_additional[80] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].plus_recording_additional)

  return(0)
}


int edf_set_startdatetime(int handle, int startdate_year, int startdate_month, int startdate_day,
                                      int starttime_hour, int starttime_minute, int starttime_second)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  if((startdate_year<1970) || (startdate_year>3000) ||
     (startdate_month<1)   || (startdate_month>12)  ||
     (startdate_day<1)     || (startdate_day>31)    ||
     (starttime_hour<0)    || (starttime_hour>23)   ||
     (starttime_minute<0)  || (starttime_minute>59) ||
     (starttime_second<0)  || (starttime_second>59))
  {
    return(-1)
  }

  hdrlist[handle].startdate_year = startdate_year
  hdrlist[handle].startdate_month = startdate_month
  hdrlist[handle].startdate_day = startdate_day
  hdrlist[handle].starttime_hour = starttime_hour
  hdrlist[handle].starttime_minute = starttime_minute
  hdrlist[handle].starttime_second = starttime_second

  return(0)
}


int edfwrite_annotation_utf8(int handle, long long onset, long long duration, const char *description)
{
  int i

  struct edf_write_annotationblock *list_annot, *malloc_list


  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(onset<0LL)
  {
    return(-1)
  }

  if(hdrlist[handle].annots_in_file >= hdrlist[handle].annotlist_sz)
  {
    malloc_list = (struct edf_write_annotationblock *)realloc(write_annotationslist[handle],
                                                              sizeof(struct edf_write_annotationblock) * (hdrlist[handle].annotlist_sz + EDFLIB_ANNOT_MEMBLOCKSZ))
    if(malloc_list==NULL)
    {
      return(-1)
    }

    write_annotationslist[handle] = malloc_list

    hdrlist[handle].annotlist_sz += EDFLIB_ANNOT_MEMBLOCKSZ
  }

  list_annot = write_annotationslist[handle] + hdrlist[handle].annots_in_file

  list_annot.onset = onset
  list_annot.duration = duration
  strncpy(list_annot.annotation, description, EDFLIB_WRITE_MAX_ANNOTATION_LEN)
  list_annot.annotation[EDFLIB_WRITE_MAX_ANNOTATION_LEN] = 0

  for(i=0  i++)
  {
    if(list_annot.annotation[i] == 0)
    {
      break
    }

    if(list_annot.annotation[i] < 32)
    {
      list_annot.annotation[i] = '.'
    }
  }

  hdrlist[handle].annots_in_file++

  return(0)
}


int edfwrite_annotation_latin1(int handle, long long onset, long long duration, const char *description)
{
  struct edf_write_annotationblock *list_annot, *malloc_list

  char str[EDFLIB_WRITE_MAX_ANNOTATION_LEN + 1]


  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(onset<0LL)
  {
    return(-1)
  }

  if(hdrlist[handle].annots_in_file >= hdrlist[handle].annotlist_sz)
  {
    malloc_list = (struct edf_write_annotationblock *)realloc(write_annotationslist[handle],
                                                              sizeof(struct edf_write_annotationblock) * (hdrlist[handle].annotlist_sz + EDFLIB_ANNOT_MEMBLOCKSZ))
    if(malloc_list==NULL)
    {
      return(-1)
    }

    write_annotationslist[handle] = malloc_list

    hdrlist[handle].annotlist_sz += EDFLIB_ANNOT_MEMBLOCKSZ
  }

  list_annot = write_annotationslist[handle] + hdrlist[handle].annots_in_file

  list_annot.onset = onset
  list_annot.duration = duration
  strncpy(str, description, EDFLIB_WRITE_MAX_ANNOTATION_LEN)
  str[EDFLIB_WRITE_MAX_ANNOTATION_LEN] = 0
  edflib_latin12utf8(str, strlen(str))
  strncpy(list_annot.annotation, str, EDFLIB_WRITE_MAX_ANNOTATION_LEN)
  list_annot.annotation[EDFLIB_WRITE_MAX_ANNOTATION_LEN] = 0

  hdrlist[handle].annots_in_file++

  return(0)
}


static void edflib_remove_padding_trailing_spaces(char *str)
{
  int i

  while(str[0]==' ')
  {
    for(i=0  i++)
    {
      if(str[i]==0)
      {
        break
      }

      str[i] = str[i+1]
    }
  }

  for(i = strlen(str) i>0 i--)
  {
    if(str[i-1]==' ')
    {
      str[i-1] = 0
    }
    else
    {
      break
    }
  }
}


int edf_set_prefilter(int handle, int edfsignal, const char *prefilter)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(edfsignal<0)
  {
    return(-1)
  }

  if(edfsignal>=hdrlist[handle].edfsignals)
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].edfparam[edfsignal].prefilter, prefilter, 80)

  hdrlist[handle].edfparam[edfsignal].prefilter[80] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].edfparam[edfsignal].prefilter)

  return(0)
}


int edf_set_transducer(int handle, int edfsignal, const char *transducer)
{
  if(handle<0)
  {
    return(-1)
  }

  if(handle>=EDFLIB_MAXFILES)
  {
    return(-1)
  }

  if(hdrlist[handle]==NULL)
  {
    return(-1)
  }

  if(!(hdrlist[handle].writemode))
  {
    return(-1)
  }

  if(edfsignal<0)
  {
    return(-1)
  }

  if(edfsignal>=hdrlist[handle].edfsignals)
  {
    return(-1)
  }

  if(hdrlist[handle].datarecords)
  {
    return(-1)
  }

  strncpy(hdrlist[handle].edfparam[edfsignal].transducer, transducer, 80)

  hdrlist[handle].edfparam[edfsignal].transducer[80] = 0

  edflib_remove_padding_trailing_spaces(hdrlist[handle].edfparam[edfsignal].transducer)

  return(0)
}

"""
minimumtoprint is the minimum digits that will be printed (minus sign not included),
leading zero's will be added if necessary
if sign is zero, only negative numbers will have the sign '-' character
if sign is one, the sign '+' or '-' character will always be printed
returns the string
"""
edflib_sprint_Int64_number_nonlocalized(q, minimumtoprint, sign)
    if (q < 0) || sign == 1
        minimumtoprint += 1
    q = Int64(q)
    fmt = "%" * (sign == 1 ? "+" : "") * "0$(minimumtoprint)d"
    @sprintf(file, fmt, q)
end


"""
minimumtoprint is the minimum digits that will be printed (minus sign not included),
leading zero's will be added if necessary
if sign is zero, only negative numbers will have the sign '-' character
if sign is one, the sign '+' or '-' character will always be printed
returns the number of characters printed to the file
"""
edflib_fprint_Int64_number_nonlocalized(file, q, minimumtoprint, sign)
    str = edflib_sprint_Int64_number_nonlocalized(q, minimumtoprint, sign)
    write(file, str)
end


edflib_sprint_Float64_nonlocalized(str, nr)
    minimutoprint = (nr < 0) || sign == 1 ? 20 : 19
    nr = Float64(nr)
    @sprintf(file, "%0$(minimumtoprint)f", nr)
end

edflib_atof_nonlocalized(str) = parse(Float64, str)


edflib_atoi_nonlocalized(str) = parse(Int, str)
