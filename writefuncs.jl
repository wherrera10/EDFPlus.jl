
type EDF_Writable
   pathname::Str
   file_type::Int
   n_channels::Int
   channels::Dict
   sample_buffer::Dict
   handle::Cint
end

"""
Opens an EDF file for writing at file_name.
file_type is one of FILETYPE_EDF, FILETYPE_EDFPLUS, 
                    FILETYPE_BDF, FILETYPE_BDFPLUS
channel_info should be a list of dicts, one for each channel in the data. 
Each dict needs these values:          
    'label' : channel label (string, <= 16 characters, must be unique)
    'dimension' : physical dimension (e.g., mV) (string, <= 8 characters)
    'sample_rate' : sample frequency in hertz (int)
    'physical_max' : maximum physical value (float)
    'physical_min' : minimum physical value (float)
    'digital_max' : maximum digital value (int, -2**15 <= x < 2**15)
    'digital_min' : minimum digital value (int, -2**15 <= x < 2**15)
"""
function EDF_Writer(file_name, channel_info, file_type=FILETYPE_EDFPLUS, ...)
    path = file_name
    file_type = file_type
    n_channels = len(channel_info)
    channels = Dict()
    sample_buffer = Dict()
    for chn in channel_info
        if chn['label'] in channels
            info("Error: attempt to add a duplicate channel with label $(chn['label'])")
        else
            channels[chn['label']] = chn
            sample_buffer[chn['label']] = []
        end
    end
    handle = _edflib.open_file_writeonly(file_name, file_type, n_channels)
    edfw = EDF_Writable(file_name, file_type, n_channels, 
                        channels, sample_buffer, handle, 0, 0)
    _init_constants(edfw, **kwargs)
    _init_channels(edfw, channel_info)
    edfw
end

"""
Queues a digital sample for @channel_label for recording; the data won't
be written for a second or so given how edflib writes files, which is to 
wait until at least a second's worth of data has been queued.
"""
function write_sample(edfw, channel_label, sample)
        if channel_label not in edfw.channels
            info("Error: No such channel $channel_label")
        end
        edfw.sample_buffer[channel_label].append(sample)
        if len(edfw.sample_buffer[channel_label]) == edfw.channels[channel_label]['sample_rate']
            edfw._flush_samples()
        end
end

function close(edfw)
        _edflib.close_file(edfw.handle)
end

function _init_constants(edfw, ...)
        function call_if_set(fn, kw_name)
            item = kwargs.pop(kw_name, None)
            if item is not None:
                fn(self.handle, item)
            end
        end
        call_if_set(_edflib.set_technician, 'technician')
        call_if_set(_edflib.set_recording_additional, 'recording_additional')
        call_if_set(_edflib.set_patientname, 'patient_name')
        call_if_set(_edflib.set_patient_additional, 'patient_additional')
        call_if_set(_edflib.set_equipment, 'equipment')
        call_if_set(_edflib.set_admincode, 'admincode')
        call_if_set(_edflib.set_gender, 'gender')
        call_if_set(_edflib.set_datarecord_duration, 'duration')
        call_if_set((lambda hdl, dt: _edflib.set_startdatetime(hdl, dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second)), 'recording_start_time')
        call_if_set((lambda hdl, dt: _edflib.set_birthdate(hdl, dt.year, dt.month, dt.day)), 'patient_birthdate')
        if len(kwargs) > 0:
            info("Unhandled argument(s) given: %r' % kwargs.keys()")
        end
end


function _init_channels(edfw, channels)
        hdl = edfw.handle
        function call_per_channel(fn, name, optional=false)
            for i,c in enumerate(channels)
                if !optional | (name in c)
                    fn(hdl, i, c.pop(name))
                end
            end
        end
        call_per_channel(_edflib.set_samplefrequency, 'sample_rate')
        call_per_channel(_edflib.set_physical_maximum, 'physical_max')
        call_per_channel(_edflib.set_digital_maximum, 'digital_max')
        call_per_channel(_edflib.set_digital_minimum, 'digital_min')
        call_per_channel(_edflib.set_physical_minimum, 'physical_min')
        call_per_channel(_edflib.set_label, 'label')
        call_per_channel(_edflib.set_physical_dimension, 'dimension')
        call_per_channel(_edflib.set_transducer, 'transducer', optional=True)
        call_per_channel(_edflib.set_prefilter, 'prefilter', optional=True)
end

function _flush_samples(edfw)
        for c in self.channels 
            buf = np.array(self.sample_buffer[c], dtype='int16')
            _edflib.write_digital_samples(self.handle, buf)
            self.sample_buffer[c] = []
        end
end
