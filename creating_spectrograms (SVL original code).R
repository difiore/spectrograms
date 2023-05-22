# OPTIONS FOR CREATING SPECTROGRAMS
# By Silvy van Kuijk

#-------------------------------------------------------------------------------
## Content:
#-------------------------------------------------------------------------------

  # Package 1 - soundgen    Line 51 (Currently used for Frontiers paper)
  # Package 2 - seewave     Line 102
  # Package 3 - phonTools   Line 121
  # Package 4 - warbleR     Line 129
  # Package 5 - ggspectro   Line 136
  # Package 6 - dynaSpec    Line 228


#-------------------------------------------------------------------------------
## Load packages:
#-------------------------------------------------------------------------------
packages <- c("tidyverse",
              "lubridate",
              "tuneR",
              "seewave",
              "ggplot2",
              "dplyr",
              "ggpubr",
              "ggmap",
              "ggspatial",
              "plotly",
              "soundgen",
              "phonTools",
              "dynaSpec",
              "viridis",
              "grid",
              "gridExtra") # Create vector of packages

lapply(packages, require, character.only = TRUE)    # Load multiple packages


#-------------------------------------------------------------------------------
## Read in the sound files
#-------------------------------------------------------------------------------
wav1 <- tuneR::readWave('PUMA_20140214_071500_dist1.wav', from = 1, to = 21, units = c("seconds"))
wav2 <- tuneR::readWave('HARPIA_20170216_073000_Fig2.wav', from = 1, to = 21, units = c("seconds"))
wav3 <- tuneR::readWave('HARPIA_20160204_073000_dist3.wav', from = 1, to = 21, units = c("seconds"))
wav4 <- tuneR::readWave('HARPIA_20170213_071500_dist4.wav', from = 1, to = 21, units = c("seconds"))

# You can change the section that it loads into R by adjusting the 'from =' and 'to =' parameters.


#-------------------------------------------------------------------------------
## Package 1: soundgen - Used in Frontiers Manuscript
#-------------------------------------------------------------------------------

  #Add "savePlots = [path]" within the spectrogram function to save the spectrogram images!
dist1 <- soundgen::spectrogram(wav1, yScale = 'linear',
                                     wn = 'hanning',
                                     noiseReduction = 0.5,
                                     ylim = c(0, 2.5),
                                     osc = 'none',
                                     width = 1800,
                                     height = 1000,
                                     cex.lab = 1,
                                     plot = TRUE)

dist2 <- soundgen::spectrogram(wav2, yScale = 'linear',
                                   wn = 'hanning',
                                   noiseReduction = 0.5,
                                   ylim = c(0, 2.5),
                                   osc = 'none',
                                   width = 1800,
                                   height = 1000,
                                   cex.lab = 1,
                                   plot = TRUE)

dist3 <- soundgen::spectrogram(wav3, yScale = 'linear',
                               wn = 'hanning',
                               noiseReduction = 0.5,
                               ylim = c(0, 2.5),
                               osc = 'none',
                               width = 1800,
                               height = 1000,
                               cex.lab = 1,
                               plot = TRUE)

dist4 <- soundgen::spectrogram(wav4, yScale = 'linear',
                               wn = 'hanning',
                               noiseReduction = 0.5,
                               ylim = c(0, 2.5),
                               osc = 'none',
                               width = 1800,
                               height = 1000,
                               cex.lab = 1,
                               plot = TRUE)

# This package seemed most promising.
# This leads to pretty good looking images, but I cannot combine them into one
# image and I cannot seem to edit the axes (like removing 's' begind each number
# of the x-axis).


#-------------------------------------------------------------------------------
## Package 2: Seewave
#-------------------------------------------------------------------------------
Spect <- seewave::spectro(wav1,
                          flim=c(0,5),
                          noisereduction = NULL,
                          tlim = c(0,20),
                          wl = 1024,
                          ovlp = 50,
                          palette = reverse.gray.colors.1,
                          osc = FALSE)

plot(Spect)
# You can add an oscillogram below the spectrogram by changing osc = to TRUE,
# but the spectrogram - even with noisereduction at zero - just looks too washed
# out. The axes here look exactly like they should for publication though,
# unlike the former package.


#-------------------------------------------------------------------------------
## Package 3: phonTools
#-------------------------------------------------------------------------------
  # Recommended by Dena, but cannot get this to work.
sound = loadsound('HARPIA_20170216_073000_Fig2.wav')
phonTools::spectrogram (Figure2, fs = 24000, windowlength = 200)


#-------------------------------------------------------------------------------
## Package 4: warbleR
#-------------------------------------------------------------------------------
  # Cannot get this one to work either.
warbleR::specreator(wav2, flim = c(0, 5), res = 300, mar = 0.05, wl = 300)


#-------------------------------------------------------------------------------
## Package 5: ggspectro
#-------------------------------------------------------------------------------
  # This one is so cool! This might be one of our best options (with some
  # modifications of colors)?
  # Code modified from: https://rug.mnhn.fr/seewave/spec.html


## PLOT LABELLERS
  # x label formatter
s_formatter <- function(x){
  lab <- paste0(x, " s")
}

  # y label formatter
khz_formatter <- function(y){
  lab <- paste0(y, " kHz")
}


## THEMES & COLORS
  # Create theme
hot_theme <- theme(panel.grid.major.y = element_line(color="black", linetype = "dotted"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill="transparent"),
                   panel.border = element_rect(linetype = "solid", fill = NA, color = "grey"),
                   axis.line = element_blank(),
                   legend.position = "top",
                   legend.justification = "right",
                   legend.background = element_rect(fill="black"),
                   legend.key.width = unit(50, "native"),
                   legend.title = element_text(size=16, color="grey"),
                   legend.text = element_text(size=16, color="grey"),
                   plot.background = element_rect(fill="black"),
                   axis.title = element_blank(),
                   axis.text = element_text(size=16, color = "grey"),
                   axis.ticks = element_line(color="grey"))

  # Set colors
hot_colors <- inferno(n=9)


## GGSPECTRO PLOTS
  # Build spectrograms using ggspectro()
hotplot1 <- ggspectro(wave = wav1, f = wav1@samp.rate, ovlp=50)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(breaks = seq(from = 0, to = 2, by= 0.25), limits = c(0, 2), expand = c(0,0), position = "right")+
  geom_raster(aes(fill=amplitude), hjust = 0, vjust = 0, interpolate = F)+
  scale_fill_gradientn(colours = hot_colors, name = "Amplitude (dB)", na.value = "transparent", limits = c(-60,0))+
  hot_theme

hotplot2 <- ggspectro(wave = wav2, f = wav2@samp.rate, ovlp=90)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(breaks = seq(from = 0, to = 2, by=0.25), limits = c(0, 2), expand = c(0,0), position = "right")+
  geom_raster(aes(fill=amplitude), hjust = 0, vjust = 0, interpolate = F)+
  scale_fill_gradientn(colours = hot_colors, name = "Amplitude (dB)", na.value = "transparent", limits = c(-60,0))+
  hot_theme

hotplot3 <- ggspectro(wave = wav3, f = wav3@samp.rate, ovlp=90)+
  scale_x_continuous(labels=s_formatter, expand = c(0,0))+
  scale_y_continuous(breaks = seq(from = 0, to = 5, by=1), expand = c(0,0), labels = khz_formatter, position = "right")+
  geom_raster(aes(fill=amplitude), hjust = 0, vjust = 0, interpolate = F)+
  scale_fill_gradientn(colours = hot_colors, name = "Amplitude (dB)", na.value = "transparent", limits = c(-60,0))+
  hot_theme

hotplot4 <- ggspectro(wave = wav4, f = wav4@samp.rate, ovlp=90)+
  scale_x_continuous(labels=s_formatter, expand = c(0,0))+
  scale_y_continuous(breaks = seq(from = 0, to = 5, by=1), expand = c(0,0), labels = khz_formatter, position = "right")+
  geom_raster(aes(fill=amplitude), hjust = 0, vjust = 0, interpolate = F)+
  scale_fill_gradientn(colours = hot_colors, name = "Amplitude (dB)", na.value = "transparent", limits = c(-60,0))+
  hot_theme

## PLOT GRID
  #Just trying 2 spectrograms, not all four.
p1 = ggplot_gtable(ggplot_build(hotplot1))
p2 = ggplot_gtable(ggplot_build(hotplot2))

maxWidth = grid::unit.pmax(p1$widths, p2$widths)
p1$widths <- as.list(maxWidth)
p2$widths <- as.list(maxWidth)

layo <- rbind(c(1,1,1),
              c(1,1,1),
              c(2,2,2),
              c(2,2,2))

  #If these last two lines give errors, write dev.off() in the console.
grid.newpage()
grid.arrange(p1, p2, layout_matrix = layo)


#-------------------------------------------------------------------------------
## Package 6: dynaSpec
#-------------------------------------------------------------------------------
  # Create videos of spectrograms! Explore for thesis defense + website!
  # Have not tried this one yet as it's not relevant for the manuscript,
  # just keeping it here for future reference.
prep_static_ggspectro(Figure2)
