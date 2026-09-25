#=
[eegplotter.jl]
Version =  0.2.3
Author = "William Herrera"
Copyright = "Copyright 2018, 2026 William Herrera"
Created = "12 Jan 2018"
Purpose = "EEG file routines viewer example"
=#

using EDFPlus, DSP, GLMakie, Downloads, Scratch

"""
    signal_channels(edfh; max_channels=8)

Find the signal channels in an EDF file, excluding EDF annotation channels.

Only channels sampled at the most common (highest-count) rate among the
non-annotation channels are kept, since `filtered_pages` builds a single
shared time axis for all displayed channels -- mixing in low-rate channels
(e.g. "Activity" or "Temperature" sampled once per second) would make their
data too short to line up with that axis and break plotting.

Returns at most `max_channels` channel numbers.
"""
function signal_channels(edfh; max_channels = 8)

    candidates = Int[]

    for c in eachindex(edfh.signalparam)

        label = uppercase(strip(trim(edfh.signalparam[c].label)))

        # EDF+ annotation channels normally have "ANNOTATION" in their
        # signal label.  Check several common forms so that annotation
        # channels are not displayed as EEG signals.
        is_annotation = occursin("ANNOTATION", label) ||
            occursin("EDF ANNOT", label) ||
            label == "ANNOT" ||
            label == "EDFANNOT"

        is_annotation || push!(candidates, c)
    end

    isempty(candidates) && return Int[]

    # Pick the most common sample rate among the candidate channels, and
    # keep only channels that share it. Ties are broken in favor of the
    # higher rate, since low-rate channels (e.g. "Activity" or
    # "Temperature" sampled once per second) are typically status
    # channels rather than EEG signals.
    rates = [edfh.signalparam[c].smp_per_record for c in candidates]
    rate_counts = Dict{Int, Int}()
    for r in rates
        rate_counts[r] = get(rate_counts, r, 0) + 1
    end
    common_rate = argmax(r -> (rate_counts[r], r), collect(keys(rate_counts)))

    channels = Int[]
    for c in candidates
        edfh.signalparam[c].smp_per_record == common_rate || continue
        push!(channels, c)
        length(channels) >= max_channels && break
    end

    return channels
end

"""
    filtered_pages(edfh; channels, seconds_per_page)

Split the recording into consecutive `seconds_per_page`-second pages and
band-pass filter each channel (1.5 Hz - 24 Hz) to clean up drift and noise.

Returns:
  pages         -- pages[p][c] is a Vector{Float64} of samples for
                   page `p`, channel `c`
  timepoints    -- time axis (seconds) shared by every page
  channel_labels -- channel name for each channel, for axis labels
"""
function filtered_pages(edfh; channels, seconds_per_page = 15.0)
    fs = samplerate(edfh, first(channels))
    n_pages = floor(Int, edfh.file_duration / seconds_per_page)
    n_pages > 0 || error("The EDF file does not contain a complete page of data.")
    pages = Vector{Vector{Vector{Float64}}}(undef, n_pages)

    for p in 1:n_pages
        t1 = (p - 1) * seconds_per_page
        t2 = t1 + seconds_per_page
        segment = multichanneltimesegment(edfh, channels, t1, t2, true)
        pages[p] = [lowpassfilter(highpassfilter(ch, fs, 1.5), fs, 24.0) for ch in segment]
    end

    # Force every page/channel to the same sample count. Time-boundary
    # rounding in multichanneltimesegment can give adjacent pages counts that
    # differ by one sample, but plotting requires x and y to have the same length.
    target_len = round(Int, seconds_per_page * fs)
    for p in 1:n_pages, c in eachindex(pages[p])
        len = length(pages[p][c])
        if len > target_len
            pages[p][c] = pages[p][c][1:target_len]
        elseif len < target_len
            pad = fill(pages[p][c][end], target_len - len)
            pages[p][c] = vcat(pages[p][c], pad)
        end
    end

    timepoints = collect(range(0.0, seconds_per_page, length = target_len))
    channel_labels = [trim(edfh.signalparam[c].label) for c in channels]
    return pages, timepoints, channel_labels
end

"""
    average_reference(edfh, channels)

Average the given channels together into one signal. Used only to feed the
spectrogram panel at the bottom of the viewer, as a rough "what's happening
overall" summary.
"""
function average_reference(edfh, channels)
    data = EDFPlus.signaldata(edfh)
    n_records = size(data, 1)
    rec_len = maximum(p -> p.smp_per_record, edfh.signalparam)
    spans = [EDFPlus.signalindices(edfh, c) for c in channels]
    avg = zeros(rec_len * n_records)

    for r in 1:n_records, span in spans
        chunk = data[r, span[1]:span[2]]
        avg[(r - 1) * rec_len .+ (1:length(chunk))] .+= chunk ./ length(channels)
    end

    return avg
end

"""
    vieweeg(filename; seconds_per_page=15.0, max_channels=8)

Open an interactive window showing the EDF recording `filename` one page
at a time.

The signal channels are discovered automatically from the EDF file.
EDF annotation channels are excluded. At most `max_channels` signal
channels are displayed.

Navigation:
  - On-screen « Prev / Next » buttons
  - Left/Right (or Up/Down, Page Up/Page Down) arrow keys move a page
  - Clicking on the spectrogram jumps straight to that point in time
  - A vertical marker on the spectrogram shows the current page window
"""
function vieweeg(filename; seconds_per_page = 15.0, max_channels = 8)
    edfh = loadfile(filename)

    # Automatically discover the signal channels in the EDF file.
    # Annotation channels are excluded and no more than eight channels
    # are displayed.
    channels = signal_channels(edfh; max_channels = max_channels)

    isempty(channels) && error("The EDF file contains no displayable signal channels.")

    pages, timepoints, labels = filtered_pages(
        edfh;
        channels = channels,
        seconds_per_page = seconds_per_page,
    )
    n_pages = length(pages)
    n_channels = length(channels)
    fs = samplerate(edfh, first(channels))
    total_duration = edfh.file_duration

    # --- spectrogram (down-sampled so it fits in a GPU texture) ---
    avg = average_reference(edfh, channels)
    spec = spectrogram(avg, 256, 128; fs = fs)
    max_cols = 4000
    n_time = length(spec.time)
    step = max(1, cld(n_time, max_cols))
    t_ds = spec.time[1:step:end]

    # swap dims from what spectrogram returns (frequency × time) to (time × frequency)
    p_ds = permutedims(log.(spec.power[:, 1:step:end] .+ eps(Float32)))

    # Basename for the title bar (works with full paths or relative paths)
    fname = basename(filename)

    # `page_no` is the single source of display positioning for the EEG viewer.
    page_no = Observable(1)

    # Title includes the file name
    page_title = @lift "$fname — Page $($page_no) of $n_pages"

    # Keep one Observable for each channel
    channel_data = [Observable(copy(pages[1][c])) for c in 1:n_channels]

    # Time window of the currently displayed page (for the spectrogram marker)
    page_t0 = @lift (($page_no - 1) * seconds_per_page)
    page_t1 = @lift (min($page_t0 + seconds_per_page, total_duration))

    # Whenever the page number changes, replace each channel's Observable.
    on(page_no) do p
        for c in 1:n_channels
            channel_data[c][] = pages[p][c]
        end
    end

    # Makie plot figure
    fig = Figure(size = (1200, 850), fontsize = 14)

    # Title + navigation buttons row
    top = GridLayout(fig[1, 1])
    Label(top[1, 1], page_title, fontsize = 18, tellwidth = false)
    btn_prev = Button(top[1, 2], label = "« Prev (PgUp)", width = 140)
    btn_next = Button(top[1, 3], label = "Next (PgDn) »", width = 140)
    colsize!(top, 1, Relative(1.0))

    channel_axes = Axis[]

    # Channel axes start at row 2
    for (c, label) in enumerate(labels)
        ax = Axis(
            fig[c + 1, 1];
            ylabel = label,
            yticksvisible = false,
            yticklabelsvisible = false,
        )
        # Each channel has its own Observable.
        lines!(ax, timepoints, channel_data[c])
        hidexdecorations!(ax; grid = false)
        push!(channel_axes, ax)
    end

    linkxaxes!(channel_axes...)
    hidexdecorations!(channel_axes[end]; grid = false, ticks = false, ticklabels = false)

    channel_axes[end].xlabel = "Time (s)"

    # Spectrogram spanning the whole recording  (row = n_channels + 2)
    spec_ax = Axis(
        fig[n_channels + 2, 1];
        ylabel = "Hz",
        xlabel = "Time (s, whole recording)",
    )
    heatmap!(spec_ax, t_ds, spec.freq, p_ds; colormap = :viridis)

    # Vertical span white line marking the currently displayed page
    vspan!(spec_ax, page_t0, page_t1; color = (:white, 0.25), strokewidth = 0)

    # Thin center line for extra visibility
    vlines!(
        spec_ax,
        @lift(($page_t0 + $page_t1) / 2);
        color = :white,
        linewidth = 1.5,
        linestyle = :dash,
    )
    rowsize!(fig.layout, n_channels + 2, Relative(0.22))

    """ Navigation helper for changing pages in the EEG viewer. """
    function change_page(delta)
        newp = clamp(page_no[] + delta, 1, n_pages)
        if newp != page_no[]
            page_no[] = newp
        end
    end

    # On-screen buttons
    on(btn_prev.clicks) do _
        change_page(-1)
    end
    on(btn_next.clicks) do _
        change_page(+1)
    end

    # Keyboard: arrows + Page Up / Page Down (forward in time / backward)
    on(events(fig).keyboardbutton) do event
        event.action in (Keyboard.press, Keyboard.repeat) || return
        if event.key in (Keyboard.right, Keyboard.up, Keyboard.page_down)
            change_page(+1)
        elseif event.key in (Keyboard.left, Keyboard.down, Keyboard.page_up)
            change_page(-1)
        end
    end

    # Click (or double-click)on spectrogram → jump to that time
    # Use the high-level Axis interaction API (gives coordinates directly)
    register_interaction!(spec_ax, :page_jump) do event::MouseEvent, _
        if event.type === MouseEventTypes.leftclick
            t = event.data[1]
            # Determine the page directly from the time position. Eg.
            #   0 <= t < 15   -> page 1
            #   15 <= t < 30  -> page 2
            #   30 <= t < 45  -> page 3
            newp = clamp(floor(Int, t / seconds_per_page) + 1, 1, n_pages)
            if newp != page_no[]
                page_no[] = newp
            end
        end
    end

    display(fig)
    return fig
end

""" If run from /example directory: one-time download of 27 MB demo human EEG file """
function getsampleeeg()
    dir = @get_scratch!("eeg_samples")
    path = joinpath(dir, "eeg-sample-ma.edf")
    if !isfile(path)
        @info "Downloading sample EEG (27 MB, one-time)..."
        Downloads.download(
            "https://github.com/wherrera10/EDFPlus/releases/download/v0.2.3/eeg-sample-ma.edf",
            path,
        )
    end
    return path
end

filename = if !isempty(ARGS)
    ARGS[1]
elseif abspath(PROGRAM_FILE) == @__FILE__
    getsampleeeg()
else
    @info "usage: julia eegplotter.jl <filename>\nNo filename provided."
    println("Enter the filename or just press return to exit the program.")
    f = readline()
    if isempty(f) || !isfile(f)
        println("No file found, exiting..")
        exit()
    end
    f
end

fig = vieweeg(filename)

if !isinteractive()
    println("Press return to exit.")
    readline()
end
