# annotations 
EDFLIB_DO_NOT_READ_ANNOTATIONS = 0
EDFLIB_READ_ANNOTATIONS       = 1
EDFLIB_READ_ALL_ANNOTATIONS   = 2

EDFLIB_TIME_DIMENSION = (10000000LL)
EDFLIB_MAXSIGNALS = 256
EDFLIB_MAX_ANNOTATION_LEN = 512

# file seeks
EDFSEEK_SET = 0
EDFSEEK_CUR = 1
EDFSEEK_END = 2

# the following defines are used in the member "filetype" of the edf_hdr_struct 
# and as return value for the function edfopen_file_readonly() 
EDFLIB_FILETYPE_EDF =                  0
EDFLIB_FILETYPE_EDFPLUS =              1
EDFLIB_FILETYPE_BDF    =              2
EDFLIB_FILETYPE_BDFPLUS  =             3
EDFLIB_MALLOC_ERROR       =          -1
EDFLIB_NO_SUCH_FILE_OR_DIRECTORY =   -2
EDFLIB_FILE_CONTAINS_FORMAT_ERRORS = -3
EDFLIB_MAXFILES_REACHED         =    -4
EDFLIB_FILE_READ_ERROR           =   -5
EDFLIB_FILE_ALREADY_OPENED       =   -6
EDFLIB_FILETYPE_ERROR            =   -7
EDFLIB_FILE_WRITE_ERROR          =   -8
EDFLIB_NUMBER_OF_SIGNALS_INVALID =   -9
EDFLIB_FILE_IS_DISCONTINUOUS     =  -10
EDFLIB_INVALID_READ_ANNOTS_VALUE =  -11


# the following defines are possible errors returned by edfopen_file_writeonly() 
EDFLIB_NO_SIGNALS              =    -20
EDFLIB_TOO_MANY_SIGNALS         =   -21
EDFLIB_NO_SAMPLES_IN_RECORD      =  -22
EDFLIB_DIGMIN_IS_DIGMAX           = -23
EDFLIB_DIGMAX_LOWER_THAN_DIGMIN   = -24
EDFLIB_PHYSMIN_IS_PHYSMAX         = -25
