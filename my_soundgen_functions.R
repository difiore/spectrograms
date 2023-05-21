##### my_spectrogram

my_spectrogram <- function (x, samplingRate = NULL, scale = NULL, from = NULL,
                            to = NULL, dynamicRange = 80, windowLength = 50, step = NULL,
                            overlap = 70, specType = c("spectrum", "reassigned", "spectralDerivative")[1],
                            wn = "gaussian", zp = 0, normalize = TRUE, smoothFreq = 0,
                            smoothTime = 0, qTime = 0, percentNoise = 10, noiseReduction = 0,
                            output = c("original", "processed", "complex", "all")[1],
                            reportEvery = NULL, cores = 1, plot = TRUE, savePlots = NULL,
                            osc = c("none", "linear", "dB")[2], heights = c(3, 1), ylim = NULL,
                            yScale = c("linear", "log", "bark", "mel", "ERB")[1], contrast = 0.2,
                            brightness = 0, blur = 0, maxPoints = c(1e+05, 5e+05), padWithSilence = TRUE,
                            colorTheme = c("bw", "seewave", "heat.colors", "...")[1],
                            extraContour = NULL, xlab = NULL, ylab = NULL, xaxp = NULL,
                            mar = c(5.1, 4.1, 4.1, 2), main = NULL, grid = NULL, width = 900,
                            height = 500, units = "px", res = NA, ...)
{
  myPars = c(as.list(environment()), list(...))
  myPars = myPars[!names(myPars) %in% c("x", "samplingRate",
                                        "scale", "from", "to", "reportEvery", "cores", "savePlots")]
  pa = my_processAudio(x, samplingRate = samplingRate, scale = scale,
                    from = from, to = to, funToCall = "my_.spectrogram", myPars = myPars,
                    reportEvery = reportEvery, cores = cores, savePlots = savePlots)
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "spectrogram", width = paste0(width, units)))
  }
  if (pa$input$n == 1)
    pa$result = pa$result[[1]]
  # invisible(pa$result)
}

###### my_processAudio

my_processAudio <- function (x, samplingRate = NULL, scale = NULL, from = NULL,
          to = NULL, funToCall, myPars = list(), var_noSummary = NULL,
          reportEvery = NULL, savePlots = TRUE, saveAudio = NULL, cores = 1)
{
  input = my_checkInputType(x)
  input$failed = rep(FALSE, input$n)
  if (is.character(savePlots)) {
    if (savePlots == "") {
      if (input$type[1] == "file") {
        savePlots = paste0(dirname(input$filenames[1]),
                           "/")
      }
      else {
        savePlots = paste0(getwd(), "/")
      }
    }
    else {
      savePlots = paste0(dirname(paste0(savePlots, "/arbitrary")),
                         "/")
    }
    if (!dir.exists(savePlots))
      dir.create(savePlots)
  }
  else {
    savePlots = NULL
  }
  input$savePlots = savePlots
  if (is.character(saveAudio)) {
    if (saveAudio == "") {
      keypr = readline(prompt = paste("NB: saveAudio='' will overwrite the originals. Proceed? (yes/no) "))
      if (substr(keypr, 1, 1) != "y")
        stop("Aborting...")
      if (input$type[1] == "file") {
        saveAudio = paste0(dirname(input$filenames[1]),
                           "/")
      }
      else {
        saveAudio = paste0(getwd(), "/")
      }
    }
    else {
      saveAudio = paste0(dirname(paste0(saveAudio, "/arbitrary")),
                         "/")
    }
    if (!dir.exists(saveAudio))
      dir.create(saveAudio)
  }
  else {
    saveAudio = NULL
  }
  input$saveAudio = saveAudio
  if (input$type[1] == "file")
    x = rep(list(NULL), input$n)
  if (!is.list(x))
    x = list(x)
  if (cores > 1 & input$n > 1) {
    cl = parallel::makeCluster(cores, outfile = "")
    doParallel::registerDoParallel(cl)
    time_start_global = proc.time()
    chunks = splitIntoChunks(1:input$n, cores)
    result = foreach::foreach(i = 1:length(chunks), .combine = "c") %dopar%
      {
        time_start = proc.time()
        chunk = chunks[[i]]
        len_chunk = length(chunk)
        a = vector("list", len_chunk)
        for (t in 1:len_chunk) {
          audio = my_readAudio(x[[chunk[t]]], input, chunk[t],
                            samplingRate = samplingRate, scale = scale,
                            from = from, to = to)
          audio$savePlots = savePlots
          audio$saveAudio = saveAudio
          if (!audio$failed) {
            an_t = try(do.call(funToCall, c(list(audio = audio),
                                            myPars)))
            if (inherits(an_t, "try-error"))
              audio$failed = TRUE
          }
          if (audio$failed) {
            if (input$n > 1) {
              warning(paste("Failed to process file",
                            input$filenames[t]))
            }
            else {
              warning("Failed to process the input")
            }
            an_t = numeric(0)
          }
          if ((is.null(reportEvery) || is.finite(reportEvery)) &
              input$n > 1) {
            reportTime(i = t, nIter = len_chunk, reportEvery = reportEvery,
                       time_start = time_start, jobs = input$filesizes[chunk],
                       prefix = paste0("Core ", i, ": "))
          }
          a[[t]] = an_t
        }
        a
      }
    parallel::stopCluster(cl)
    names(result) = input$filenames_base
    input$failed = (sapply(result, length) == 0)
    if ((is.null(reportEvery) || is.finite(reportEvery)) &
        input$n > 1) {
      soundgen::reportTime(i = input$n, nIter = input$n,
                           time_start = time_start_global)
    }
  }
  else {
    if (input$n > 5) {
      nCores = try(parallel::detectCores(), silent = TRUE)
      if (!inherits(nCores, "try-error") && is.numeric(nCores) &&
          nCores > 1) {
        msg = paste("Consider using multiple cores to speed up processing with",
                    "'cores = ...'. Your machine has", nCores,
                    "cores")
        message(msg)
      }
    }
    result = vector("list", input$n)
    names(result) = input$filenames_base
    time_start = proc.time()
    for (i in 1:input$n) {
      audio = my_readAudio(x[[i]], input, i, samplingRate = samplingRate,
                        scale = scale, from = from, to = to)
      audio$savePlots = savePlots
      audio$saveAudio = saveAudio
      if (!audio$failed) {
        an_i = try(do.call(funToCall, c(list(audio = audio),
                                        myPars)))
        if (inherits(an_i, "try-error"))
          audio$failed = TRUE
      }
      if (audio$failed) {
        if (input$n > 1) {
          warning(paste("Failed to process file", input$filenames[i]))
        }
        else {
          warning("Failed to process the input")
        }
        an_i = numeric(0)
        input$failed[i] = TRUE
      }
      result[[i]] = an_i
      gc()
      if ((is.null(reportEvery) || is.finite(reportEvery)) &
          input$n > 1) {
        reportTime(i = i, nIter = input$n, reportEvery = reportEvery,
                   time_start = time_start, jobs = input$filesizes)
      }
    }
  }
  return(list(input = input, result = result))
}

###### my_processAudio

my_checkInputType <- function (x)
{
  if (is.character(x)) {
    if (length(x) == 1 && dir.exists(x)) {
      x = dirname(paste0(x, "/arbitrary"))
      filenames = list.files(x, pattern = "*.wav|.mp3|.WAV|.MP3",
                             full.names = TRUE)
      if (length(filenames) < 1)
        stop(paste("No wav/mp3 files found in", x))
    }
    else {
      for (f in 1:length(x)) {
        if (!file.exists(x[f]) || !substr(x[f], nchar(x[f]) -
                                          3, nchar(x[f])) %in% c(".wav", ".mp3", ".WAV",
                                                                 ".MP3")) {
          stop("Input not recognized - must be a folder, wav/mp3 file(s), or numeric vector(s)")
        }
      }
      filenames = x
    }
    n = length(filenames)
    type = rep("file", n)
    filesizes = file.info(filenames)$size
    filenames_base = basename(filenames)
    filenames_noExt = sub("^(.*?)([.](([[:alnum:]]+|tar[.](gz|bz2|xz)|nb[.]html)[~#]?))$",
                          "\\1", filenames_base)
    filenames = normalizePath(filenames)
  }
  else {
    if (!is.list(x))
      x = list(x)
    n = length(x)
    if (n == 1) {
      filenames_base = filenames_noExt = "sound"
    }
    else {
      filenames_base = filenames_noExt = paste0("sound",
                                                1:n)
    }
    filenames = NULL
    filesizes = NULL
    type = rep(NA, n)
    for (i in 1:n) {
      if (is.numeric(x[[i]]) | is.logical(x[[i]])) {
        type[i] = "vector"
      }
      else if (inherits(x[[i]], "Wave")) {
        type[i] = "Wave"
      }
      else {
        stop(paste("Input not recognized - must be a folder, wav/mp3 file,",
                   "Wave object, or numeric vector"))
      }
    }
  }
  return(list(type = type, n = n, filenames = filenames, filenames_base = filenames_base,
              filenames_noExt = filenames_noExt, filesizes = filesizes))
}


###### my_readAudio

my_readAudio <- function (x, input = checkInputType(x), i, samplingRate = NULL,
                          scale = NULL, from = NULL, to = NULL)
{
  failed = FALSE
  right = NULL
  if (input$type[i] == "file") {
    fi = input$filenames[i]
    ext_i = substr(fi, nchar(fi) - 3, nchar(fi))
    if (ext_i %in% c(".wav", ".WAV")) {
      sound_wave = try(tuneR::readWave(fi))
    }
    else if (ext_i %in% c(".mp3", ".MP3")) {
      sound_wave = try(tuneR::readMP3(fi))
    }
    else {
      warning(paste("Input", fi, "not recognized: expected a wav/mp3 file"))
    }
    if (inherits(sound_wave, "try-error")) {
      failed = TRUE
      sound = samplingRate = bit = scale = NULL
    }
    else {
      sound = as.numeric(sound_wave@left)
      right = as.numeric(sound_wave@right)
      samplingRate = sound_wave@samp.rate
      bit = sound_wave@bit
      scale = 2^(sound_wave@bit - 1)
    }
  }
  else if (input$type[i] == "vector") {
    if (is.null(samplingRate)) {
      samplingRate = 16000
      message("samplingRate not specified; defaulting to 16000")
    }
    sound = x
    m = suppressWarnings(max(abs(sound), na.rm = TRUE))
    bit = 1
    if (is.null(scale)) {
      scale = max(m, 1)
    }
    else if (is.numeric(scale)) {
      if (scale < m) {
        scale = m
        warning(paste("Scale cannot be smaller than observed max;",
                      "resetting to", m))
      }
    }
  }
  else if (input$type[i] == "Wave") {
    sound = x@left
    right = x@right
    samplingRate = x@samp.rate
    bit = x@bit
    scale = 2^(x@bit - 1)
  }
  ls = length(sound)
  if (any(is.numeric(c(from, to)))) {
    if (!is.numeric(from)) {
      from_points = 1
    }
    else {
      from_points = max(1, round(from * samplingRate))
      if (from_points > ls)
        stop("Invalid from - greater than sound duration")
    }
    if (!is.numeric(to)) {
      to_points = ls
    }
    else {
      to_points = min(ls, round(to * samplingRate))
    }
    sound = sound[from_points:to_points]
    right = right[from_points:to_points]
    timeShift = from_points/samplingRate
    ls = length(sound)
  }
  else {
    timeShift = 0
  }
  duration = ls/samplingRate
  return(list(sound = sound, right = right, samplingRate = samplingRate,
              bit = bit, scale = scale, scale_used = max(abs(range(sound))),
              failed = failed, ls = ls, duration = duration, timeShift = timeShift,
              filename = input$filenames[i], filename_base = input$filenames_base[i],
              filename_noExt = input$filenames_noExt[i]))
}


###### my_.spectrogram

my_.spectrogram <- function (audio, dynamicRange = 80, windowLength = 50, step = NULL,
                             overlap = 70, specType = c("spectrum", "reassigned", "spectralDerivative")[1],
                             wn = "gaussian", zp = 0, normalize = TRUE, smoothFreq = 0,
                             smoothTime = 0, qTime = 0, percentNoise = 10, noiseReduction = 0,
                             output = c("original", "processed", "complex", "all")[1],
                             plot = TRUE, osc = c("none", "linear", "dB")[2], heights = c(3,
                                                                                          1), ylim = NULL, yScale = "linear", contrast = 0.2, brightness = 0,
                             blur = 0, maxPoints = c(1e+05, 5e+05), padWithSilence = TRUE,
                             colorTheme = c("bw", "seewave", "heat.colors", "...")[1],
                             extraContour = NULL, xlab = NULL, ylab = NULL, xaxp = NULL,
                             mar = c(5.1, 4.1, 4.1, 2), main = NULL, grid = NULL, width = 900,
                             height = 500, units = "px", res = NA, internal = NULL, ...)
{
  if (!is.null(step))
    overlap = 100 * (1 - step/windowLength)
  if (overlap < 0 | overlap > 100) {
    warning("overlap must be >0 and <= 100%; resetting to 70")
    overlap = 70
  }
  if (is.null(step))
    step = windowLength * (1 - overlap/100)
  windowLength_points = floor(windowLength/1000 * audio$samplingRate/2) *
    2
  if (windowLength_points > (audio$ls/2)) {
    windowLength_points = floor(audio$ls/4) * 2
    step = windowLength_points/audio$samplingRate * 1000 *
      (1 - overlap/100)
  }
  if (windowLength_points == 0) {
    stop("The sound and/or windowLength are too short for plotting a spectrogram")
  }
  rec_scales = c("linear", "log", "bark", "mel", "ERB")
  if (!yScale %in% rec_scales) {
    yScale = "linear"
    warning(paste0("Implemented yScale: ", paste(rec_scales,
                                                 collapse = ", "), ". Defaulting to linear"))
  }
  if (!specType %in% c("spectrum", "reassigned", "spectralDerivative"))
    warning("Unknown specType, defaulting to \"spectrum\"")
  if (is.null(internal$frameBank)) {
    internal$frameBank = my_getFrameBank(sound = audio$sound,
                                      samplingRate = audio$samplingRate, windowLength_points = windowLength_points,
                                      step = step, zp = zp, normalize = normalize, wn = wn,
                                      filter = NULL, padWithSilence = padWithSilence, timeShift = audio$timeShift)
  }
  contrast_exp = exp(3 * contrast)
  brightness_exp = exp(3 * brightness)
  X = as.numeric(colnames(internal$frameBank))
  lx = length(X)
  if (lx < 2) {
    message("The sound is too short for plotting a spectrogram")
    return(NA)
  }
  zpExtra = max(0, floor((zp - windowLength_points)/2) * 2)
  windowLength_points = windowLength_points + zpExtra
  n1 = floor(windowLength_points/2)
  bin_width = audio$samplingRate/windowLength_points
  Y = (0:(n1 - 1)) * bin_width/1000
  ly = length(Y)
  if (ly < 2) {
    message("The sound and/or the windowLength is too short for obtaining a spectrogram")
    return(NA)
  }
  z = apply(internal$frameBank, 2, function(x) stats::fft(x)[1:n1])
  if (!is.matrix(z))
    z = matrix(z, ncol = 1)
  rownames(z) = Y
  colnames(z) = X
  Z = t(Mod(z))
  if (specType == "spectralDerivative") {
    dZ_dt = cbind(rep(0, lx), t(apply(Z, 1, diff)))
    dZ_df = rbind(rep(0, ly), apply(Z, 2, diff))
    Z = sqrt(dZ_dt^2 + dZ_df^2)
  }
  else if (specType == "reassigned") {
    filter_h = seewave::ftwindow(wl = windowLength_points,
                                 wn = wn)
    filter_dh = diff(filter_h)
    filter_dh = c(filter_dh[1] - (filter_dh[2] - filter_dh[1]),
                  filter_dh)
    internal$frameBank_dh = my_getFrameBank(sound = audio$sound,
                                         samplingRate = audio$samplingRate, windowLength_points = windowLength_points,
                                         step = step, zp = zp, normalize = normalize, filter = filter_dh,
                                         padWithSilence = padWithSilence, timeShift = audio$timeShift)
    z_dh = apply(internal$frameBank_dh, 2, function(x) stats::fft(x)[1:n1])
    freqs_new = matrix(Y, nrow = nrow(z), ncol = ncol(z)) -
      Im(z_dh/z) * audio$samplingRate/(2000 * pi)
    middle = (windowLength_points + 1)/2
    filter_th = filter_h * (middle - (1:windowLength_points))
    internal$frameBank_th = my_getFrameBank(sound = audio$sound,
                                         samplingRate = audio$samplingRate, windowLength_points = windowLength_points,
                                         step = step, zp = zp, normalize = normalize, filter = filter_th,
                                         padWithSilence = padWithSilence, timeShift = audio$timeShift)
    z_th = apply(internal$frameBank_th, 2, function(x) stats::fft(x)[1:n1])
    times_new = matrix(X, nrow = ly, ncol = lx, byrow = TRUE) +
      Re(z_th/z)/audio$samplingRate * 1000
    df = data.frame(x = as.numeric(times_new), y = as.numeric(freqs_new),
                    z = as.numeric(t(Z)))
    df = na.omit(df)
    min_x = min(X)
    min_y = min(Y)
    max_x = max(X)
    max_y = max(Y)
    df = df[which(df$x > min_x & df$x < max_x & df$y > min_y &
                    df$y < max_y), ]
    reassigned_raw = df
    if (FALSE) {
      df$magn = df$z
      df$magn[df$magn <= 0] = min(df$magn[df$magn > 0])
      df$magn = zeroOne(log(df$magn))
      colfunc = colorRampPalette(c("blue", "yellow"))
      df$order = findInterval(df$magn, seq(0, 1, length.out = 30))
      plot(df$x, df$y, type = "n")
      points(df$x, df$y, col = colfunc(30)[df$order], pch = 16,
             cex = 0.5)
    }
    df$ix = findInterval(df$x, seq(min_x, max_x, length.out = lx +
                                     1))
    df$iy = findInterval(df$y, seq(min_y, max_y, length.out = ly +
                                     1))
    Z = matrix(min(df$z), nrow = lx, ncol = ly)
    for (i in 1:nrow(df)) Z[df$ix[i], df$iy[i]] = Z[df$ix[i],
                                                    df$iy[i]] + df$z[i]
    if (FALSE) {
    }
    rownames(Z) = X
    colnames(Z) = Y
  }
  Z1 = Z
  threshold = max(Z1)/10^(dynamicRange/20)
  Z1[Z1 < threshold] = 0
  if (smoothTime > 1) {
    Z1 = t(apply(Z1, 1, function(x) {
      zoo::rollmedian(x, k = smoothTime, fill = 0)
    }))
  }
  if (smoothFreq > 1) {
    Z1 = apply(Z1, 2, function(x) {
      zoo::rollmedian(x, k = smoothFreq, fill = 0)
    })
  }
  if (qTime > 0) {
    Z1 = t(apply(Z1, 1, function(x) {
      x - quantile(x, probs = qTime)
    }))
  }
  positives = which(Z1 > 0)
  nonpositives = which(Z1 <= 0)
  Z1[positives] = log(Z1[positives])
  if (length(positives) > 0 & length(nonpositives) > 0) {
    Z1[nonpositives] = min(Z1[positives])
  }
  Z1 = Z1 - min(Z1)
  if (noiseReduction > 0) {
    entr = apply(Z1, 1, function(x) getEntropy(x))
    q = quantile(entr, probs = 1 - percentNoise/100, na.rm = TRUE)
    idx = as.numeric(which(entr >= q))
    if (length(idx) > 0) {
      noise_spectrum = as.numeric(apply(Z1[idx, , drop = FALSE],
                                        2, mean))
      Z1 = t(apply(Z1, 1, function(x) x - noiseReduction *
                     noise_spectrum))
    }
    Z1[Z1 <= 0] = 0
  }
  if (contrast_exp != 1) {
    Z1 = Z1^contrast_exp
  }
  tr = try(if (any(Z1 != 0))
    Z1 = Z1/max(Z1))
  if (brightness_exp != 1) {
    Z1 = Z1/brightness_exp
  }
  if (brightness_exp < 1) {
    Z1[Z1 > 1] = 1
  }
  if (!is.null(blur) && any(is.finite(blur)) && any(blur !=
                                                    0)) {
    if (length(blur) == 1) {
      filt_dim = rep(round(blur/bin_width * 2) + 1, 2)
    }
    else if (length(blur) == 2) {
      filt_dim = c(round(blur[2]/step) * 2 + 1, round(blur[1]/bin_width) *
                     2 + 1)
    }
    else {
      stop("blur must be of length 1 or 2")
    }
    filt_dim[which(!is.finite(filt_dim))] = 0
    old_max = max(Z1)
    if (sign(prod(filt_dim)) < 0) {
      if (filt_dim[1] < 0) {
        Z1 = gaussianSmooth2D(Z1, kernelSize = c(Mod(filt_dim[1]),
                                                 0), action = "unblur")
        Z1 = gaussianSmooth2D(Z1, kernelSize = c(0, filt_dim[1]),
                              action = "blur")
      }
      else if (filt_dim[2] < 0) {
        Z1 = gaussianSmooth2D(Z1, kernelSize = c(0, Mod(filt_dim[2])),
                              action = "unblur")
        Z1 = gaussianSmooth2D(Z1, kernelSize = c(filt_dim[1],
                                                 0), action = "blur")
      }
    }
    else {
      Z1 = gaussianSmooth2D(Z1, kernelSize = Mod(filt_dim),
                            action = if (filt_dim[1] >= 0)
                              "blur"
                            else "unblur")
    }
    idx_pos = which(Z1 > 0)
    Z1[-idx_pos] = 0
    Z1[idx_pos] = Z1[idx_pos]/max(Z1[idx_pos]) * old_max
  }
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt,
                          "_spectrogram.png"), width = width, height = height,
        units = units, res = res)
  }
  if (plot) {
    my_plotSpec(X = X, Y = Y, Z = Z1, audio = audio, internal = internal,
             dynamicRange = dynamicRange, osc = osc, heights = heights,
             ylim = ylim, yScale = yScale, maxPoints = maxPoints,
             colorTheme = colorTheme, extraContour = extraContour,
             xlab = xlab, ylab = ylab, xaxp = xaxp, mar = mar,
             main = main, grid = grid, width = width, height = height,
             units = units, res = res, ...)
  }
  if (is.character(audio$savePlots)) {
    dev.off()
  }
  if (output == "original") {
    out = t(Z)
  }
  else if (output == "processed") {
    out = t(Z1)
  }
  else if (output == "complex") {
    out = z
  }
  else {
    out = list(original = t(Z), processed = t(Z1), reassigned = reassigned_raw,
               complex = z)
  }
  invisible(out)
}



###### my_getFrameBank

my_getFrameBank <- function (sound, samplingRate, windowLength_points, wn, step,
                             zp, normalize = TRUE, filter = NULL, padWithSilence = FALSE,
                             timeShift = NULL)
{
  if (!is.numeric(sound))
    return(NA)
  sound[is.na(sound)] = 0
  if (normalize & any(sound != 0)) {
    sound = sound - mean(sound)
    sound = sound/max(abs(max(sound)), abs(min(sound)))
  }
  step_points = round(step/1000 * samplingRate)
  if (padWithSilence) {
    sound = c(rep(0, windowLength_points/2), sound, rep(0,
                                                        (windowLength_points + step_points)))
  }
  myseq = seq(1, max(1, (length(sound) - windowLength_points)),
              by = step_points)
  if (padWithSilence) {
    time_stamps = (myseq - 1) * 1000/samplingRate
  }
  else {
    time_stamps = (myseq - 1 + windowLength_points/2) * 1000/samplingRate
  }
  if (!is.null(timeShift))
    time_stamps = time_stamps + round(timeShift * 1000)
  if (is.null(filter)) {
    filter = seewave::ftwindow(wl = windowLength_points,
                               wn = wn)
  }
  zpExtra = max(0, floor((zp - windowLength_points)/2) * 2)
  if (zpExtra > 0) {
    frameBank = apply(as.matrix(myseq), 1, function(x) {
      c(rep(0, zpExtra/2), sound[x:(windowLength_points +
                                      x - 1)] * filter, rep(0, zpExtra/2))
    })
  }
  else {
    frameBank = apply(as.matrix(myseq), 1, function(x) {
      sound[x:(windowLength_points + x - 1)] * filter
    })
  }
  colnames(frameBank) = time_stamps
  return(frameBank)
}


###### my_plotSpec
my_plotSpec <- function (X, Y, Z, audio = NULL, internal = NULL, dynamicRange = 80,
                         osc = c("none", "linear", "dB")[2], heights = c(3, 1), ylim = NULL,
                         yScale = "linear", contrast = 0.2, brightness = 0, maxPoints = c(1e+05,
                                                                                          5e+05), padWithSilence = TRUE, colorTheme = c("bw", "seewave",
                                                                                                                                        "heat.colors", "...")[1], extraContour = NULL, xlab = NULL,
                         ylab = NULL, xaxp = NULL, mar = c(5.1, 4.1, 4.1, 2), main = NULL,
                         grid = NULL, width = 900, height = 500, units = "px", res = NA,
                         ...)
{
  color.palette = my_switchColorTheme(colorTheme)
  if (osc == TRUE)
    osc = "linear"
  else if (!is.character(osc))
    osc = "none"
  op = par(c("mar", "xaxt", "yaxt", "mfrow"))
  if (is.null(xlab))
    xlab = ""
  if (!is.null(maxPoints)) {
    if (length(maxPoints) == 1)
      maxPoints = c(maxPoints, maxPoints)
  }
  if (is.null(ylim))
    ylim = c(0, audio$samplingRate/2/1000)
  if (is.null(main)) {
    if (audio$filename_noExt == "sound") {
      main = ""
    }
    else {
      main = audio$filename_noExt
    }
  }
  lx = length(X)
  ly = length(Y)
  x_ms = X[lx] < 1
  if (osc %in% c("linear", "dB")) {
    if (!is.null(maxPoints) && maxPoints[1] < audio$ls) {
      myseq = seq(1, audio$ls, by = ceiling(audio$ls/maxPoints[1]))
      audio$sound = audio$sound[myseq]
      audio$ls = length(myseq)
    }
    if (osc == "dB") {
      audio$sound = .osc(audio[names(audio) != "savePlots"],
                         dynamicRange = dynamicRange, dB = TRUE, plot = FALSE,
                         returnWave = TRUE)
      ylim_osc = c(-2 * dynamicRange, 0)
    }
    else {
      ylim_osc = c(-audio$scale, audio$scale)
    }
    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
    par(mar = c(mar[1:2], 0, mar[4]), xaxt = "s", yaxt = "s")
    time_stamps = seq(0, audio$duration, length.out = audio$ls) +
      audio$timeShift
    plot(time_stamps, audio$sound, type = "l", ylim = ylim_osc,
         axes = FALSE, xaxs = "i", yaxs = "i", bty = "o",
         xlab = xlab, ylab = "", main = "", ...)
    box()
    time_location = axTicks(1, axp = xaxp)
    time_labels = my_convert_sec_to_hms(time_location, 3)
    axis(side = 1, at = time_location, labels = time_labels,
         ...)
    if (osc == "dB") {
      axis(side = 4, at = seq(-dynamicRange, 0, by = 10),
           ...)
      abline(h = -dynamicRange, lty = 2, col = "gray70")
    }
    else {
      abline(h = 0, lty = 2, col = "gray70")
    }
    par(mar = c(0, mar[2:4]), xaxt = "n", yaxt = "s")
    xlab = ""
  }
  else {
    par(mar = mar)
  }
  if (x_ms) {
    xlim = c(0, audio$duration * 1000) + audio$timeShift *
      1000
  }
  else {
    X = X/1000
    xlim = c(0, audio$duration) + audio$timeShift
  }
  if (yScale == "log" & ylim[1] < 0.01)
    ylim[1] = 0.01
  idx_y = which(Y >= (ylim[1]/1.05) & Y <= (ylim[2] * 1.05))
  Y = Y[idx_y]
  ly = length(Y)
  Z = Z[, idx_y]
  y_Hz = ylim[2] < 1
  if (!exists("ylab") || is.null(ylab))
    if (y_Hz)
      ylab = "Frequency, Hz"
  else ylab = "Frequency, kHz"
  my_filled.contour.mod(x = X, y = Y, z = Z, levels = seq(0, 1,
                                                       length = 30), color.palette = color.palette, ylim = ylim,
                     main = main, xlab = xlab, ylab = ylab, xlim = xlim, xaxt = "n",
                     log = ifelse(yScale == "log", "y", ""), yScale = yScale,
                     maxPoints = maxPoints[2], ...)
  if (!(osc %in% c("linear", "dB"))) {
    time_location = axTicks(1, axp = xaxp)
    time_labels = my_convert_sec_to_hms(time_location, 3)
    axis(side = 1, at = time_location, labels = time_labels,
         ...)
  }
  if (is.numeric(grid)) {
    n_grid_per_kHz = diff(range(ylim)) * grid
    if (Y[length(Y)] < 1)
      n_grid_per_kHz = n_grid_per_kHz/1000
    grid(nx = n_grid_per_kHz, ny = n_grid_per_kHz, col = rgb(0,
                                                             0, 0, 0.25, maxColorValue = 1), lty = 3)
  }
  if (!is.null(internal$pitch)) {
    do.call(addPitchCands, c(internal$pitch, list(y_Hz = y_Hz,
                                                  yScale = yScale)))
  }
  if (!is.null(extraContour)) {
    extraContour_pars = list()
    if (is.list(extraContour)) {
      if (length(extraContour) > 1)
        extraContour_pars = extraContour[2:length(extraContour)]
      cnt = extraContour[[1]]
    }
    else {
      cnt = extraContour
    }
    lc = length(cnt)
    cnt = approx(x = 1:lc, y = cnt, xout = seq(1, lc, length.out = length(X)),
                 na.rm = FALSE)$y
    do.call(addPitchCands, list(extraContour = cnt, extraContour_pars = extraContour_pars,
                                y_Hz = y_Hz, timestamps = X, yScale = yScale, pitchCands = NA,
                                pitchCert = NA, pitchSource = NA, pitch = NA))
  }
  par(mar = op$mar, xaxt = op$xaxt, yaxt = op$yaxt, mfrow = op$mfrow)
}

###### my_switchColorTheme
my_switchColorTheme <- function (colorTheme)
{
  if (is.null(colorTheme))
    return(NULL)
  if (colorTheme == "bw") {
    color.palette = function(x) gray(seq(from = 1, to = 0,
                                         length = x))
  }
  else if (colorTheme == "seewave") {
    color.palette = seewave::spectro.colors
  }
  else {
    colFun = match.fun(colorTheme)
    color.palette = function(x) rev(colFun(x))
  }
  return(color.palette)
}

###### my_filled.contour.mod

my_filled.contour.mod <- function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
                                   z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE),
                                   zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels),
                                   nlevels = 30, color.palette = function(n) grDevices::hcl.colors(n,
                                                                                                   "YlOrRd", rev = TRUE), col = color.palette(length(levels) -
                                                                                                                                                1), asp = NA, xaxs = "i", yaxs = "i", log = "", yScale = c("orig",
                                                                                                                                                                                                           "bark", "mel", "ERB")[1], axisX = TRUE, axisY = TRUE,
                                   maxPoints = 5e+05, ...)
{
  y_Hz = ylim[2] < 1
  if (ylim[2] > tail(y, 1))
    ylim[2] = tail(y, 1)
  if (yScale == "bark") {
    y = tuneR::hz2bark(y * 1000)
    ylim = tuneR::hz2bark(ylim * 1000)
  }
  else if (yScale == "mel") {
    y = hz2mel(y * 1000)
    ylim = hz2mel(ylim * 1000)
  }
  else if (yScale == "ERB") {
    y = HzToERB(y * 1000)
    ylim = HzToERB(ylim * 1000)
  }
  else {
    if (y_Hz) {
      y = y * 1000
      ylim = ylim * 1000
    }
    if (log == "y" & ylim[1] < 0.01)
      ylim[1] = 0.01
  }
  suppressWarnings({
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs,
                asp = asp, log = log, ...)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
      stop("no proper 'z' matrix specified")
    if (!is.double(z))
      storage.mode(z) = "double"
    if (!is.null(maxPoints)) {
      len_z = length(z)
      if (len_z > maxPoints) {
        message(paste("Plotting with reduced resolution;",
                      "increase maxPoints or set to NULL to override"))
        lx = length(x)
        seqx = seq(1, lx, length.out = ceiling(lx/(len_z/maxPoints)))
        x = x[seqx]
        z = z[seqx, ]
      }
    }
    .filled.contour(as.double(x), as.double(y), z, as.double(levels),
                    col = col)
    title(...)
    if (axisX)
      axis(1, ...)
    if (axisY) {
      if (!yScale %in% c("bark", "mel", "ERB")) {
        axis(2, ...)
      }
      else {
        y_at = seq(y[1], tail(y, 1), length.out = 5)
        if (yScale == "bark") {
          if (y_Hz) {
            y_lab = round(tuneR::bark2hz(y_at))
            y_at = tuneR::hz2bark(y_lab)
          }
          else {
            y_lab = round(tuneR::bark2hz(y_at)/1000,
                          1)
            y_at = tuneR::hz2bark(y_lab * 1000)
          }
        }
        else if (yScale == "mel") {
          if (y_Hz) {
            y_lab = round(tuneR::mel2hz(y_at))
            y_at = tuneR::hz2mel(y_lab)
          }
          else {
            y_lab = round(tuneR::mel2hz(y_at)/1000, 1)
            y_at = tuneR::hz2mel(y_lab * 1000)
          }
        }
        else if (yScale == "ERB") {
          if (y_Hz) {
            y_lab = round(ERBToHz(y_at))
            y_at = HzToERB(y_lab)
          }
          else {
            y_lab = round(ERBToHz(y_at)/1000, 1)
            y_at = HzToERB(y_lab * 1000)
          }
        }
        axis(2, at = y_at, labels = y_lab, ...)
      }
    }
  })
  invisible()
}


##### my_convert_sec_to_hms

my_convert_sec_to_hms <- function (time_s, digits = 0)
{
  if (!any(time_s > 1)) {
    output = paste(round(time_s * 1000), "ms")
  }
  else {
    len = length(time_s)
    output = vector("character", len)
    for (i in 1:len) {
      days_string = hours_string = minutes_string = seconds_string = ms_string = ""
      days = time_s[i]%/%86400
      if (days > 0)
        days_string = paste(days, "d ")
      hours = time_s[i]%/%3600 - days * 24
      if (hours > 0)
        hours_string = paste(hours, "h ")
      if (days == 0) {
        minutes = time_s[i]%/%60 - days * 1440 - hours *
          60
        if (minutes > 0)
          minutes_string = paste(minutes, "min ")
        if (hours == 0) {
          seconds = time_s[i] - days * 86400 - hours *
            3600 - minutes * 60
          seconds_floor = floor(seconds)
          if (seconds_floor > 0)
            seconds_string = paste(round(seconds, digits),
                                   "s ") # here is where the offending "s" gets added... this function returns "ms" if # sec is more than one...
          if (minutes == 0 & seconds_floor == 0) {
            ms = (time_s[i]%%1) * 1000
            if (ms > 0)
              ms_string = paste(ms, "ms")
          }
        }
      }
      output[i] = paste0(days_string, hours_string, minutes_string,
                         seconds_string, ms_string)
    }
  }
  output = trimws(output)
  return(output)
}
