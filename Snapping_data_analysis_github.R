#####Install Packages----------------
#install.packages("seewave")
#install.packages("tuneR", dep=TRUE)
#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("stringr")



###Library of Packages Used---------
library(seewave) #audio analysis
library(tuneR) #audio analysis
library(ggplot2) #plotting
library(plyr) #data organization
library(dplyr) #data organization
library(stringr) #data cleaning


####This script contains two functions that are the same except for one important difference:####
#1) snapping_data_analysis- extracts amplitude peaks as a percentage
#relative to the maximum amplitude extracted from each .wav sound file

#2) std_snapping_data_analysis- extracts amplitude peaks as a percentage relative to a specified
#standardized maximum amplitude that will be the same for all .wav sound files analyzed.



########FULLY AUTOMATED---------------------
#Function snapping_data_analysis is a function designed to analyze a folder of sound files at once
#Each sound file will be analyzed using Seewave's "timer()" function to produce a data frame of the times
#when a signal above a certain threshold occurs (relative to the maximum amplitude extracted from each .wav sound file).
#This data frame is then cleaned and used to produce a waveform plot and a frequency plot for each sound file given.

snapping_data_analysis <- function(){

  #user prompts----------------

  #gives the user a brief overview of the function
  writeLines(c("Welcome to the Snapping Data Analysis Function!\n",
               "1) This function analyzes an entire folder of sound files (stereo .wav)\nat once using the Seewave package and the associated timer() function.\nThe folder should not contain any extraneous files.\nFile-type extensions must be .wav not .WAV (they are case sensitive).\n",
               "2) This function extracts amplitude peaks as a percentage relative\nto the maximum amplitude extracted from each sound file.\n",
               "3) Each sound file is analyzed to produce a data frame of times when a signal\noccurs above a certain threshold (percentage of maximum amplitude).\n",
               "4) This data frame is then used to produce and save in the source folder:\na) a graphical waveform plot (with peaks identified) and \nb) a frequency-distribution plot of number of peaks between thresholds (e.g., 20-40% of maximum, 40-60% of maximum, etc.).\nc) two .csv files\ni) all peaks identified above a threshold (raw),\nii) only peaks between thresholds (cleaned)\n",
               "5) After a folder has been analyzed, the Global Environment will contain for each .wav file:\na) a copy of the left channel of the sound file, and\nb) the data frame used to create the waveform and frequency distribution graphs.\n",
               "To begin, enter the folder path and press the return key."))

  #prompt to enter the folder path name within the Rproject folder holding the sound files
  {path <- readline(prompt="Enter folder path: ")}

  #gives the user a prompt to list the threshold values used for analysis, separated by commas
  {threshold_vector <- readline(prompt="Enter threshold values separated by commas (0->100% of max.): ")}
  if (threshold_vector == ""){
    {path <- readline(prompt="Enter folder path: ")}
    {threshold_vector <- readline(prompt="Enter threshold values separated by commas (0->100% of max.): ")}
  }
  #tell r to split the input string at the commas into a vector of numbers
  threshold_vector <- as.numeric(strsplit(threshold_vector, ",")[[1]])


  #gives the prompt to ask for the amount of time (in seconds) that must be present between signals:
  ##for within thresholds (as numeric)
  {latency_1 <- readline(prompt="Enter signal spacing WITHIN thresholds (s): ")}
  if (latency_1 == "") {
    {threshold_vector <- readline(prompt="Enter threshold values separated by commas (0->100% of max.): ")}
    threshold_vector <- as.numeric(strsplit(threshold_vector, ",")[[1]])
    latency_1 <- readline(prompt="Enter signal spacing WITHIN thresholds (s): ")
  }
  latency_1 <- as.numeric(latency_1)


  ##and across thresholds (as numeric)
  {latency_2 <- readline(prompt="Enter signal spacing ACROSS thresholds (s): ")}
  if (latency_2 == ""){
    {latency_1 <- readline(prompt="Enter signal spacing WITHIN thresholds (s): ")}
    latency_1 <- as.numeric(latency_1)
    {latency_2 <- readline(prompt="Enter signal spacing ACROSS thresholds (s): ")}
  }
  latency_2 <- as.numeric(latency_2)

  #gives the prompt to ask the minor tick marks on the waveform plots
  {tick <- readline(prompt="Include minor tic marks in waveform plots (in seconds)? (1, 5, both, none): ")}
  if (tick == ""){
    {latency_2 <- readline(prompt="Enter signal spacing ACROSS thresholds (s): ")}
    latency_2 <- as.numeric(latency_2)
    {tick <- readline(prompt="Include minor tic marks in waveform plots (in seconds)? (1, 5, both, none): ")}
  }
  if (tick != "1" | tick != "5"){tick <- tolower(tick)}

  #gives the prompt to ask what format of graphic file would like to be saved
  #separated by commas
  {save_form <- readline(prompt="What format of graphic file would you like saved? (separated by commas: 1 = pdf, 2 = jpg, 3 = tiff): ")}
  save_form <- as.numeric(strsplit(save_form, ",")[[1]])

  #gives prompt that forces used to press enter if they wish to confirm input
  {confirm <- readline(prompt="Press enter to confirm this input and begin the analysis.\n(Press any other key to abort analysis)")}
  if (confirm != ""){
    stop("Analysis Aborted")

  }


  #creates a list of the sound files names within the folder path
  myFiles <- list.files(path = path , pattern="\\.wav$")

  #create a .txt file containing all prompt inputs
  #print the .txt file in the folder path
  paste_vectors <- paste(threshold_vector, collapse = ", ")
  threshold <- paste(c("Thresholds analyzed:", paste_vectors), collapse=" ")
  lat_1 <- paste(c("Within threshold latency (latency_1):", latency_1), collapse=" ")
  lat_2 <- paste(c("Across threshold latency (latency_2):", latency_2), collapse=" ")
  time <- as.character(Sys.time())
  paste_names <- paste(myFiles, collapse = ", ")
  filez <- paste(c("Files analyzed:", paste_names), collapse=" ")
  tics <- paste(c("Minor tic marks in waveform plots (in seconds; 1, 5, both, none):", tick), collapse = " ")
  format <- paste(c("What format of graphic file would you like saved? (separated by commas: 1 = pdf, 2 = jpg, 3 = tiff):", save_form), collapse=" ")
  folder <- paste(c("Folder path:", path), collapse=" ")

  file.create(paste(path,"/00-Analysis_summary_", time, sep = ""))
  print("creating text file")
  output_text <- file(paste(path,"/00-Analysis_summary_", time, sep = ""))
  writeLines(c(time, folder, filez, threshold, lat_1, lat_2, tics, format), output_text)

  print("closing text file")
  close(output_text)




  ####read sound files and split channels-------------------------
  #for each soundfile in the folder path, pull out a single sound file
  #split sound file to the left channel
  #then saves the file name in the global environment if future analysis is wanted

  for (z in 1:length(myFiles)) {
    setWavPlayer('/usr/bin/afplay')
    soundfile <- readWave(paste(path, "/", myFiles[z], sep = ""))
    soundfile <- mono(soundfile, which = "left")

    assign(myFiles[z], soundfile, envir = .GlobalEnv)

    print(paste("reading sound file",z, "of", length(myFiles)))


    out <- NULL;
    raw <- NULL;

    print("create an empty table and begin analysis")



    ####begin analysis using timer()----------------------------

    #sort the thresholds in decreasing order to make sure analysis starts with highest thresholds
    threshold_vector <- sort(threshold_vector, decreasing = TRUE)


    #begins the threshold analysis using Seewave's timer() function
    #loops through each of the threshold vectors and appends the output to the empty data frame
    #removes signals that are within latency_1 (s) of eachother
    for (j in 1 : length(threshold_vector)){
      timer_applied <- timer(soundfile,
                             f = as.numeric(soundfile@samp.rate),
                             threshold = threshold_vector[j])
      s.start <- timer_applied[["s.start"]]
      s.end <- timer_applied[["s.end"]]

      timer_data <- data.frame(s.start, s.end)

      #create a new row called threshold
      #labels the threshold the data were measured at
      timer_data$threshold <- threshold_vector[j]


      raw <- rbind(raw, timer_data)

      print(paste("obtain signal data for threshold", threshold_vector[j], "in", myFiles[z]))

      #find the difference between start times
      timer_data <- timer_data %>%
        mutate(Diff = s.start - lag(s.start))

      #filter out rows that have a signal occurring within the latency_1 value set
      #e.g. latency_1 = 0.1, remove signals that occur within 0.1s of the previous signal
      timer_data <- timer_data %>%
        filter(is.na(Diff) | Diff > latency_1)


      print(paste("filter threshold data for", threshold_vector[j], "in", myFiles[z]))

      #append the values to the starting table
      out <- rbind(out, timer_data)

      print(paste("append", threshold_vector[j], "data"))

    }


    print("threshold analysis complete")

    ####Printing raw.csv----- if you wish to remove this portion, remove all lines containing "raw" above
    #e.g.: raw <- NULL
    ###raw <- rbind(raw, timer_data)

    print("writing raw.csv")
    raw <- subset(raw)
    write.csv(x = raw,
              file = (paste(path, "/", (str_remove(myFiles[z], ".wav")), "_peakdata_raw.csv", sep = "")) ,
              row.names = TRUE)


    ###clean analysed data------------------------
    #Remove any signal <latency_2 in entire data frame
    #create a new data frame containing "plot-ready" values: plot_NameOfSoundFile
    #create a loop that loops through rows until the end of the data frame
    print(paste("Begin filtering out signals within" , latency_2, "seconds of eachother"))

    h = 1


    while (h < (1 + nrow(out))){

      #find the difference in start time between the row being looped through and every row
      out <- out %>%
        mutate(Diff = s.start - (out$s.start[h]))


      #set the difference for the looped row (currently = 0) to NA
      out$Diff[h] <- NA

      #remove the out rows that are not NA OR:
      #remove signals occur within the latency_2 period
      #do this through finding the absolute value of the difference
      #comparing it to the latency value
      out <- out %>%
        filter(is.na(Diff) | (abs(Diff) > latency_2))

      #move on to the next row
      h = h + 1
    }

    #assign the new out file to the global environment as "CLEANED"
    print(paste("cleaning complete for file", z, "of", length(myFiles)))

    plot_data <- out

    plot_data$sound <- plot_data$threshold

    plot_data$sound <- as.factor(plot_data$sound)

    print("total analysis complete, see plot_data")

    #make "plot_data" accessible in global environment for future plotting

    remove_wav <- str_remove(myFiles[z], ".wav")
    assign(paste("plot_", remove_wav, sep = ""), plot_data, envir = .GlobalEnv)

    ###write clean.csv--------------------
    print("writing cleaned.csv")
    plot_data <- subset(plot_data, select = -c(Diff, sound))
    write.csv(x = plot_data,
              file = (paste(path, "/", (str_remove(myFiles[z], ".wav")), "_peakdata_cleaned.csv", sep = "")) ,
              row.names = TRUE)


    #######begin plotting---------
    #colours-----
    #creates 10 colours as a gradient between 2 colours for
    palette1 <- colorRampPalette(c("grey", "black"))
    palette1 <- palette1(10)

    palette2 <- colorRampPalette(c("black", "darkblue"))
    palette2 <- palette2(10)

    palette3 <- colorRampPalette(c("darkblue", "steelblue1"))
    palette3 <- palette3(10)

    palette4 <- colorRampPalette(c("steelblue1", "green4"))
    palette4 <- palette4(10)

    palette5 <- colorRampPalette(c("green4", "palegreen2"))
    palette5 <- palette5(10)

    palette6 <- colorRampPalette(c("palegreen2", "lightgoldenrod1"))
    palette6 <- palette6(10)

    palette7 <- colorRampPalette(c("lightgoldenrod1", "orange"))
    palette7 <- palette7(10)

    palette8 <- colorRampPalette(c("orange", "firebrick"))
    palette8 <- palette8(10)

    palette9 <- colorRampPalette(c("firebrick", "red"))
    palette9 <- palette9(10)

    palette10 <- colorRampPalette(c("red", "maroon1"))
    palette10 <- palette10(10)

    #combine all colours into one palette

    color_dict <- c(palette1, palette2, palette3, palette4,
                    palette5, palette6, palette7, palette8,
                    palette9, palette10)


    #set the colours of the plot:

    #create a column "sound" = threshold as a factor rather than numeric
    plot_data$sound <- plot_data$threshold
    plot_data$sound <- as.factor(plot_data$sound)

    #sets the value of the threshold as an individual colour in the colour palette (color_dict)
    colors <- color_dict[as.numeric(as.character(plot_data$sound))]


    ###GRAPHIC_CONTROL_PARAMETERS------------

    #lollipop point type (Google R PCH types)
    Point_Type <- 16

    #lollipop point size (typically 1-5)
    Point_Size <- 3

    #lollipop line width (typically 1-5)
    Lollipop_Line_Width <- 2

    #Waveform line width (typically 1-5)
    Waveform_Line_Width <- 1.5



    ####Lollipop and Waveform Plots--------------------
    ###save waveform/lollipop plot in folder with sound files under name: NameOfSoundFile_waveform


    #max_amp of the waveform for adding the text to the plot
    max_amp_text <- max(abs(soundfile@left))
    max_amp_text <- paste("Max. Amplitude = ", max_amp_text)


    print(paste("plotting the data as a waveform for file",z, "of", length(myFiles) ))

    #save as jpeg
    if (2 %in% save_form){
      jpeg(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_fig_waveform.jpg", sep = ""),
           units="in", width=20, height=10, res=300)

      par(mfrow = c(2, 1))

      xaxis <- c(0, duration(soundfile))

      #plot the lollipop graph:
      par(mai = c(0, 1, 1, 1))
      plot(x = plot_data$s.start,
           y = plot_data$threshold,
           col = colors,
           pch = Point_Type,
           cex = Point_Size,
           xaxt = "n",
           ylab = "Threshold (%)",
           ylim = c(0, 100),
           las = 2,
           xlim = xaxis)
      segments(x0 = plot_data$s.start ,
               y0 = 0,
               x1 = plot_data$s.start ,
               y1 = plot_data$threshold,
               col = colors,
               lwd = Lollipop_Line_Width)

      par(mai = c(1, 1, 0, 1))
      plot(soundfile,
           xlab = "Time (s)",
           xlim = xaxis,
           lwd = Waveform_Line_Width,
           xaxt = "n")
      mtext(side = 1, max_amp_text, line = 2, adj = 1, cex = 1)
      axis(side=1, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 10))
      if (tick == "1" | tick == "both") {
        axis(side=1, lwd = 0.75, labels = FALSE, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 1))}
      if (tick == "5" | tick == "both") {
        axis(side=1, lwd = 0.75 , at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 5))}


      dev.off()

    }

    #save as tiff
    if (3 %in% save_form){
      tiff(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_fig_waveform.tiff", sep = ""),
           units="in", width=20, height=10, res=300)

      par(mfrow = c(2, 1))

      xaxis <- c(0, duration(soundfile))

      #plot the lollipop graph:
      par(mai = c(0, 1, 1, 1))
      plot(x = plot_data$s.start,
           y = plot_data$threshold,
           col = colors,
           pch = Point_Type,
           cex = Point_Size,
           xaxt = "n",
           ylab = "Threshold (%)",
           ylim = c(0, 100),
           las = 2,
           xlim = xaxis)
      segments(x0 = plot_data$s.start ,
               y0 = 0,
               x1 = plot_data$s.start ,
               y1 = plot_data$threshold,
               col = colors,
               lwd = Lollipop_Line_Width) #change line width for lollipop plots

      par(mai = c(1, 1, 0, 1))
      plot(soundfile,
           xlab = "Time (s)",
           xlim = xaxis,
           lwd = Waveform_Line_Width,
           xaxt = "n")
      mtext(side = 1, max_amp_text, line = 2, adj = 1, cex = 1)
      axis(side=1, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 10))
      if (tick == "1" | tick == "both") {
        axis(side=1, lwd = 0.75, labels = FALSE, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 1))}
      if (tick == "5" | tick == "both") {
        axis(side=1, lwd = 0.75 , at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 5))}


      dev.off()

    }



    #save as pdf
    if (1 %in% save_form){
      pdf(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_fig_waveform.pdf", sep = ""),
          width=20, height=10)

      par(mfrow = c(2, 1))

      xaxis <- c(0, duration(soundfile))

      #plot the lollipop graph:
      par(mai = c(0, 1, 1, 1))
      plot(x = plot_data$s.start,
           y = plot_data$threshold,
           col = colors,
           pch = Point_Type,
           cex = Point_Size,
           xaxt = "n",
           ylab = "Threshold (%)",
           ylim = c(0, 100),
           las = 2,
           xlim = xaxis)
      segments(x0 = plot_data$s.start ,
               y0 = 0,
               x1 = plot_data$s.start ,
               y1 = plot_data$threshold,
               col = colors,
               lwd = Lollipop_Line_Width) #change line width for lollipop plots

      par(mai = c(1, 1, 0, 1))
      plot(soundfile,
           xlab = "Time (s)",
           xlim = xaxis,
           lwd = Waveform_Line_Width,
           xaxt = "n")
      mtext(side = 1, max_amp_text, line = 2, adj = 1, cex = 1)
      axis(side=1, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 10))
      if (tick == "1" | tick == "both") {
        axis(side=1, lwd = 0.75, labels = FALSE, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 1))}
      if (tick == "5" | tick == "both") {
        axis(side=1, lwd = 0.75 , at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 5))}


      dev.off()
    }


    ######Frequency Plots--------

    ###save frequency plot in folder with sound files under name: NameOfSoundFile_frequency

    #tally the data for frequency bar graphs
    threshold_vector <- sort(threshold_vector)

    freq_data <- plot_data %>% group_by(threshold) %>% tally()

    freq_data$threshold <- as.factor(freq_data$threshold)

    #set colours
    colors <- color_dict[as.numeric(as.character(freq_data$threshold))]


    print(paste("plotting the frequency data for file",z, "of", length(myFiles)))

    #save as jpeg
    if (2 %in% save_form){
      jpeg(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_fig_freqdist.jpg", sep = ""),
           units="in", width=4, height=4, res=300)

      #plot the data
      print(ggplot(freq_data, aes(x = threshold , y = n ,
                                  fill = threshold)) +
              geom_bar(stat = "identity", width = 0.8) + #width changes the width of the bars 0-1
              theme_classic() +
              xlab("Threshold (%)") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling)))) +
              #ylim(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling))) +
              ylab("Number of Signals") +
              theme(legend.position = "none") +
              geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
              scale_fill_manual(values=colors, drop = FALSE) +
              labs(caption = max_amp_text) + theme(plot.subtitle = element_text(hjust = 1)) +
              scale_x_discrete(limits = as.character(threshold_vector), drop = FALSE))


      dev.off()
    }

    #save a tiff
    if (3 %in% save_form){
      tiff(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_fig_freqdist.tiff", sep = ""),
           units="in", width=4, height=4, res=300)

      #plot the data
      print(ggplot(freq_data, aes(x = threshold , y = n ,
                                  fill = threshold)) +
              geom_bar(stat = "identity", width = 0.8) + #width changes the width of the bars 0-1
              theme_classic() +
              xlab("Threshold (%)") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling)))) +
              #ylim(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling))) +
              ylab("Number of Signals") +
              theme(legend.position = "none") +
              geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
              scale_fill_manual(values=colors, drop = FALSE) +
              labs(caption = max_amp_text) + theme(plot.subtitle = element_text(hjust = 1)) +
              scale_x_discrete(limits = as.character(threshold_vector), drop = FALSE))


      dev.off()
    }

    #save as pdf
    if (1 %in% save_form){
      pdf(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_fig_freqdist.pdf", sep = ""),
          width=4, height=4)

      #plot the data
      print(ggplot(freq_data, aes(x = threshold , y = n ,
                                  fill = threshold)) +
              geom_bar(stat = "identity", width = 0.8) + #width changes the width of the bars 0-1
              theme_classic() +
              xlab("Threshold (%)") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling)))) +
              #ylim(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling))) +
              ylab("Number of Signals") +
              theme(legend.position = "none") +
              geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
              scale_fill_manual(values=colors, drop = FALSE) +
              labs(caption = max_amp_text) + theme(plot.subtitle = element_text(hjust = 1)) +
              scale_x_discrete(limits = as.character(threshold_vector), drop = FALSE))

      dev.off()

    }

  }

}



#############STD FULLY AUTOMATED#######################################

#function snapping_data_analysis is a function designed to analyze a folder of sound files at once
#Each sound file will be analyzed using Seewave's "timer()" function *relative to a chosen amplitude value*
#to produce a data frame of the times
#when a signal above a certain threshold occurs. This data frame is then used to produce
#a waveform plot and a frequency plot for each sound file given.

std_snapping_data_analysis <- function(){

  #user prompts----

  #gives the user a brief overview of the function
  writeLines(c("Welcome to the *Standardized* Snapping Data Analysis Function!\n",
               "1) This function analyzes an entire folder of sound files (stereo .wav)\nat once using the Seewave package and the associated timer() function.\nThe folder should not contain any extraneous files.\nFile-type extensions must be .wav not .WAV (they are case sensitive).\n",
               "Note: please ensure the standardizing sound file can be found within the outer working directory (Rproject) folder",
               "2) This function extracts amplitude peaks as a percentage relative\nto a standardized maximum amplitude, which may be specified.\n",
               "3) Each sound file is analyzed to produce a data frame of times when a signal\noccurs above a certain threshold (percentage of maximum amplitude).\n",
               "4) This data frame is then used to produce and save in the source folder:\na) a graphical waveform plot (with peaks identified) and \nb) a frequency-distribution plot of number of peaks between thresholds (e.g., 20-40% of maximum, 40-60% of maximum, etc.).\nc) two .csv files\ni) all peaks identified above a threshold (raw),\nii) only peaks between thresholds (cleaned)\n",
               "5) After a folder has been analyzed, the Global Environment will contain for each .wav file:\na) a copy of the left channel of the sound file, and\nb) the data frame used to create the waveform and frequency distribution graphs.\n",
               "To begin, enter the folder path and press the return key."))

  #prompt to enter the folder path name within the Rproject folder holding the sound files
  {path <- readline(prompt="Enter folder path: ")}


  #gives the user a prompt to list the threshold values used for analysis, separated by commas
  {threshold_vector <- readline(prompt="Enter threshold values separated by commas (0->100% of max.): ")}
  if (threshold_vector == ""){
    {path <- readline(prompt="Enter folder path: ")}
    {threshold_vector <- readline(prompt="Enter threshold values separated by commas (0->100% of max.): ")}
  }
  #splits the string at commas and converts to numeric vector
  threshold_vector <- as.numeric(strsplit(threshold_vector, ",")[[1]])


  #gives the prompt to ask for the amount of time (in seconds) that must be present between signals:
  #within thresholds (as numeric)
  {latency_1 <- readline(prompt="Enter signal spacing WITHIN thresholds (s): ")}
  if (latency_1 == "") {
    {threshold_vector <- readline(prompt="Enter threshold values separated by commas (0->100% of max.): ")}
    threshold_vector <- as.numeric(strsplit(threshold_vector, ",")[[1]])
    latency_1 <- readline(prompt="Enter signal spacing WITHIN thresholds (s): ")
  }

  latency_1 <- as.numeric(latency_1)

  #across thresholds (as numeric)
  {latency_2 <- readline(prompt="Enter signal spacing ACROSS thresholds (s): ")}
  if (latency_2 == ""){
    {latency_1 <- readline(prompt="Enter signal spacing WITHIN thresholds (s): ")}
    latency_1 <- as.numeric(latency_1)
    {latency_2 <- readline(prompt="Enter signal spacing ACROSS thresholds (s): ")}
  }
  latency_2 <- as.numeric(latency_2)


  #creates a list of the sound file names within the folder path previously given
  myFiles<- list.files(path = path , pattern="\\.wav$")

  #print the max amp within the folder to help the user decide on a standardizing amplitude
  check_max_amp<- list.files(path = path , pattern="\\.wav$")

  max_amp <- 0

  for (sound in check_max_amp) {
    setWavPlayer('/usr/bin/afplay')
    check_max_amp_sound <- readWave(paste(path, "/", sound, sep = ""))
    check_max_amp_sound <- mono(check_max_amp_sound, which = "left")
    if (max(abs(check_max_amp_sound@left)) > max_amp){
      max_amp <- as.numeric(max(abs(check_max_amp_sound@left)))
    }
  }
  print(paste("The max. amplitude found for files within ", path, ":", max_amp))


  #prompt to determine standardizing multiplier (multiples of 10kHz)
  {multiplier <- readline(prompt="Set size of standardizing snap (multiples of 10kHz): ")}
  if (multiplier == ""){
    {latency_2 <- readline(prompt="Enter signal spacing ACROSS thresholds (s): ")}
    latency_2 <- as.numeric(latency_2)
    {multiplier <- readline(prompt="Set size of standardizing snap (multiples of 10kHz): ")}
  }
  multiplier <- as.numeric(multiplier)

  #print warning if the snadardizing snap is less than the folder maximum
  if (max_amp > (multiplier*10000)){
    print("WARNING: standardizing snap less than file maximum")
    {multiplier <- readline(prompt="Set size of standardizing snap (multiples of 10kHz): ")}
    multiplier <- as.numeric(multiplier)
  }

  #prompt asking whether the standardized snap should be added in the waveform plots
  {keep_snap <- readline(prompt="Include standardizing snap in waveform plots? (y/n): ")}
  if (keep_snap == ""){
    {multiplier <- readline(prompt="Set size of standardizing snap (multiples of 10kHz): ")}
    multiplier <- as.numeric(multiplier)
    {keep_snap <- readline(prompt="Include standardizing snap in waveform plots? (y/n): ")}
  }
  keep_snap <- tolower(keep_snap)


  #prompt asking whether/at what interval minor tick marks should be included
  {tick <- readline(prompt="Include minor tic marks in waveform plots (in seconds)? (1, 5, both, none): ")}
  if (tick == ""){
    {keep_snap <- readline(prompt="Include standardizing snap in waveform plots? (y/n): ")}
    keep_snap <- tolower(keep_snap)
    {tick <- readline(prompt="Include minor tic marks in waveform plots (in seconds)? (1, 5, both, none): ")}
  }
  if (tick != "1" | tick != "5"){tick <- tolower(tick)}


  #prompt asking what format the graphs should be saved as
  {save_form <- readline(prompt="What format of graphic file would you like saved? (separated by commas: 1 = pdf, 2 = jpg, 3 = tiff): ")}
  save_form <- as.numeric(strsplit(save_form, ",")[[1]])

  #prompt asking the user to confirm the inputs; else: abort
  {confirm <- readline(prompt="Press enter to confirm this input and begin the analysis.\n(Press any other key to abort analysis)")}
  if (confirm != ""){
    stop("Analysis Aborted")
  }



  ######creating snippet of standardized snapping signal---------
  #reference snap sound file must be found in outer work directory
  setWavPlayer('/usr/bin/afplay')
  reference_snap <- readWave("reference_snap.wav")
  reference_snap <- reference_snap*(10000/max(abs(reference_snap@left)))
  reference_snap <- reference_snap* multiplier
  print(paste("The Standardized Max. Amplitude is:", max(abs(reference_snap@left))))
  reference_snap <- mono(reference_snap, which = "left")


  #create .txt file containing all inputs
  paste_vectors <- paste(threshold_vector, collapse = ", ")
  threshold <- paste(c("Thresholds analyzed:", paste_vectors), collapse=" ")
  lat_1 <- paste(c("Within threshold latency (latency_1):", latency_1), collapse=" ")
  lat_2 <- paste(c("Across threshold latency (latency_2):", latency_2), collapse=" ")
  max_amp_found <- paste(c("The max. amplitude found for files within", path, ":", max_amp), collapse=" ")
  std <- paste(c("Standardized Max. Amplitude:", max(abs(reference_snap@left))), collapse=" ")
  plot_std <- paste(c("Include standardizing snap in waveform plots? (y/n):", keep_snap), collapse = ", ")
  time <- as.character(Sys.time())
  paste_names <- paste(check_max_amp, collapse = ", ")
  folder <- paste(c("Folder path:", path), collapse=" ")
  filez <- paste(c("Files analyzed:", paste_names), collapse=" ")
  tics <- paste(c("Minor tic marks in waveform plots (in seconds; 1, 5, both, none):", tick), collapse = " ")
  format <- paste(c("What format of graphic file would you like saved? (separated by commas: 1 = pdf, 2 = jpg, 3 = tiff):", save_form), collapse=" ")


  file.create(paste(path,"/00-STD_Analysis_summary_", time, sep = ""))
  print("creating text file")
  output_text <- file(paste(path,"/00-STD_Analysis_summary_", time, sep = ""))
  writeLines(c(time, folder, filez, threshold, lat_1, lat_2, max_amp_found, std, plot_std, tics, format), output_text)

  print("closing text file")
  close(output_text)



  ####Read Sound Files and Split Channels-------------------------
  #for each sound file in the analysis folder:
  #begins by removing the right channel
  #then saves the file name in the global environment if future analysis is wanted
  for (z in 1:length(myFiles)) {
    setWavPlayer('/usr/bin/afplay')
    soundfile <- readWave(paste(path, "/", myFiles[z], sep = ""))
    soundfile <- mono(soundfile, which = "left")
    soundfile_length <- soundfile
    soundfile <- pastew(reference_snap, soundfile, f = as.numeric(soundfile@samp.rate), output="Wave")

    assign(myFiles[z], soundfile, envir = .GlobalEnv)

    print(paste("reading sound file",z, "of", length(myFiles)))


    #create a new table to add the Seewave data
    out <- NULL;
    raw <- NULL;
    print("create an empty table and begin analysis")


    #collect the maximum amplitude of the soundfile early as it will get cut out in the data cleaning
    #this value should = standardizing snap value inputted
    max_amp_text <- max(abs(soundfile@left))
    max_amp_text <- paste("Max. Amplitude = ", max_amp_text)


    threshold_vector <- sort(threshold_vector, decreasing = TRUE)

    ####begin analysis using timer()----------------------------
    #begins the threshold analysis using Seewave's timer() function
    #loops through each of the threshold vectors and appends the output to the empty data frame
    #removes signals that are within latency_1 (s) of each other
    for (j in 1 : length(threshold_vector)){
      timer_applied <- timer(soundfile,
                             f = as.numeric(soundfile@samp.rate),
                             threshold = threshold_vector[j])
      s.start <- timer_applied[["s.start"]]
      s.end <- timer_applied[["s.end"]]

      timer_data <- data.frame(s.start, s.end)

      #create a column "threshold" containing the threshold the data were measured at
      timer_data$threshold <- threshold_vector[j]

      raw <- rbind(raw, timer_data)

      print(paste("obtain signal data for threshold", threshold_vector[j], "in", myFiles[z]))

      #find the difference between start times
      timer_data <- timer_data %>%
        mutate(Diff = s.start - lag(s.start))

      #filter out rows that have a signal occurring within the latency_1 value set
      #e.g. latency_1 = 0.1, remove signals that occur within 0.1s of the previous signal
      timer_data <- timer_data %>%
        filter(is.na(Diff) | Diff > latency_1)


      print(paste("filter threshold data for", threshold_vector[j], "in", myFiles[z]))

      #append the values to the starting table
      out <- rbind(out, timer_data)

      print(paste("append", threshold_vector[j], "data"))

    }


    print("threshold analysis complete")

    ####Printing raw.csv----- if you want to remove this section, remove all lines containing "raw" above
    #e.g. raw <- NULL
    ##raw <- rbind(raw, timer_data)
    #raw .csv still filters out the final standardizing snap

    print("writing raw .csv")
    raw <- subset(raw)
    raw <- raw %>%
      filter(s.start <= duration(soundfile_length))
    write.csv(x = raw,
              file = (paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_peakdata_raw.csv", sep = "")) ,
              row.names = TRUE)


    ###clean analysed data------------------------
    #Remove any signal <latency_2 in entire data frame
    #create a new data frame containing "plot-ready" values: plot_NameOfSoundFile
    #loops through rows until the end of the data frame

    print(paste("Begin filtering out signals within" , latency_2, "seconds of eachother"))

    h = 1
    while (h < (1 + nrow(out))){

      #find the difference in start time between the row being looped through and every row
      out <- out %>%
        mutate(Diff = s.start - (out$s.start[h]))


      #set the difference for the looped row (currently = 0) to NA
      out$Diff[h] <- NA

      #remove the out that not NA OR:
      #remove signals occur within the latency_2 period
      #do this through finding the absolute value of the difference
      #comparing it to the latency value
      out <- out %>%
        filter(is.na(Diff) | (abs(Diff) > latency_2))

      #move on to the next row
      h = h + 1
    }

    #assign the new out file to the global environment as "CLEANED"
    print(paste("cleaning complete for file", z, "of", length(myFiles)))

    plot_data <- out

    #create new column "sound": factor version of threshold

    plot_data$sound <- plot_data$threshold

    plot_data$sound <- as.factor(plot_data$sound)

    print("total analysis complete, see plot_data")


    #make "plot_data" accessible in global environment for future plotting
    remove_wav <- str_remove(myFiles[z], ".wav")
    assign(paste("plot_", remove_wav, sep = ""), plot_data, envir = .GlobalEnv)

    ###write clean .csv--------------------
    #remove the final standardizing snap
    print("writing cleaned .csv")
    plot_data <- subset(plot_data, select = -c(Diff, sound))
    plot_data_csv <- plot_data %>%
      filter(s.start <= duration(soundfile_length))
    write.csv(x = plot_data_csv,
              file = (paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_peakdata_cleaned.csv", sep = "")) ,
              row.names = TRUE)


    #######BEGIN PLOTS---------

    ###Graphic_Control_Parameters------------

    #lollipop point type (Google R PCH types)
    Point_Type <- 16

    #lollipop point size (typically 1-5)
    Point_Size <- 3

    #lollipop line width (typically 1-5)
    Lollipop_Line_Width <- 2

    #Waveform line width (typically 1-5)
    Waveform_Line_Width <- 1.5


    #CONTAINING STD SNAP------

    if (keep_snap == "y"){

      #set the colours of the plot

      plot_data <- plot_data %>% mutate(sound = (ifelse(s.start <= duration(soundfile_length),
                                                        threshold,
                                                        "100")))
      plot_data$sound <- as.factor(plot_data$sound)

      plot_data$threshold[plot_data$sound == "100"] <- 100


      #colours-----
      palette1 <- colorRampPalette(c("grey", "black"))
      palette1 <- palette1(10)

      palette2 <- colorRampPalette(c("black", "darkblue"))
      palette2 <- palette2(10)

      palette3 <- colorRampPalette(c("darkblue", "steelblue1"))
      palette3 <- palette3(10)

      palette4 <- colorRampPalette(c("steelblue1", "green4"))
      palette4 <- palette4(10)

      palette5 <- colorRampPalette(c("green4", "palegreen2"))
      palette5 <- palette5(10)

      palette6 <- colorRampPalette(c("palegreen2", "lightgoldenrod1"))
      palette6 <- palette6(10)

      palette7 <- colorRampPalette(c("lightgoldenrod1", "orange"))
      palette7 <- palette7(10)

      palette8 <- colorRampPalette(c("orange", "firebrick"))
      palette8 <- palette8(10)

      palette9 <- colorRampPalette(c("firebrick", "red"))
      palette9 <- palette9(10)

      palette10 <- colorRampPalette(c("red", "maroon1"))
      palette10 <- palette10(10)

      color_dict <- c(palette1, palette2, palette3, palette4,
                      palette5, palette6, palette7, palette8,
                      palette9, palette10)


      #set it so the threshold determines the colour index in 1:100 colour palette
      colors <- color_dict[as.numeric(as.character(plot_data$sound))]


      ####Plotting: Lollipop and Waveform--------------------


      ###save waveform/lollipop plot in folder with sound files under name: NameOfSoundFile_waveform

      print(paste("plotting the data as a waveform for file",z, "of", length(myFiles) ))

      #save as jpeg + std snap:
      if (2 %in% save_form){
        jpeg(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_waveform.jpg", sep = ""),
             units="in", width=20, height=10, res=300)

        par(mfrow = c(2, 1))

        xaxis <- c(0, duration(soundfile))

        #plot the lollipop graph:
        par(mai = c(0, 1, 1, 1))
        plot(x = plot_data$s.start,
             y = plot_data$threshold,
             col = colors,
             pch = Point_Type,
             cex = Point_Size,
             xaxt = "n",
             ylab = "Threshold (%)",
             ylim = c(0, 100),
             las = 2,
             xlim = xaxis)
        segments(x0 = plot_data$s.start ,
                 y0 = 0,
                 x1 = plot_data$s.start ,
                 y1 = plot_data$threshold,
                 col = colors,
                 lwd = Lollipop_Line_Width) #change line width for lollipop plots

        par(mai = c(1, 1, 0, 1))
        plot((soundfile),
             xlab = "Time (s)",
             xlim = xaxis,
             lwd = Waveform_Line_Width,
             xaxt = "n")
        mtext(side = 1, max_amp_text, line = 2, adj = 1, cex = 1)
        axis(side=1, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 10))
        if (tick == "1" | tick == "both") {
          axis(side=1, lwd = 0.75, labels = FALSE, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 1))}
        if (tick == "5" | tick == "both") {
          axis(side=1, lwd = 0.75 , at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 5))}

        dev.off()
      }


      #save as tiff + std_snap:
      if (3 %in% save_form){
        tiff(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_waveform.tiff", sep = ""),
             units="in", width=20, height=10, res=300)

        par(mfrow = c(2, 1))

        xaxis <- c(0, duration(soundfile))

        #plot the lollipop graph:
        par(mai = c(0, 1, 1, 1))
        plot(x = plot_data$s.start,
             y = plot_data$threshold,
             col = colors,
             pch = Point_Type,
             cex = Point_Size,
             xaxt = "n",
             ylab = "Threshold (%)",
             ylim = c(0, 100),
             las = 2,
             xlim = xaxis)
        segments(x0 = plot_data$s.start ,
                 y0 = 0,
                 x1 = plot_data$s.start ,
                 y1 = plot_data$threshold,
                 col = colors,
                 lwd = Lollipop_Line_Width) #change line width for lollipop plots

        par(mai = c(1, 1, 0, 1))
        plot((soundfile),
             xlab = "Time (s)",
             xlim = xaxis,
             lwd = Waveform_Line_Width,
             xaxt = "n")
        mtext(side = 1, max_amp_text, line = 2, adj = 1, cex = 1)
        axis(side=1, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 10))
        if (tick == "1" | tick == "both") {
          axis(side=1, lwd = 0.75, labels = FALSE, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 1))}
        if (tick == "5" | tick == "both") {
          axis(side=1, lwd = 0.75 , at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 5))}

        dev.off()
      }


      #save as pdf + STD snap
      if (1 %in% save_form){
        pdf(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_waveform.pdf", sep = ""),
            width=20, height=10)

        par(mfrow = c(2, 1))


        xaxis <- c(0, duration(soundfile))

        #plot the lollipop graph:
        par(mai = c(0, 1, 1, 1))
        plot(x = plot_data$s.start,
             y = plot_data$threshold,
             col = colors,
             pch = Point_Type,
             cex = Point_Size,
             xaxt = "n",
             ylab = "Threshold (%)",
             ylim = c(0, 100),
             las = 2,
             xlim = xaxis)
        segments(x0 = plot_data$s.start ,
                 y0 = 0,
                 x1 = plot_data$s.start ,
                 y1 = plot_data$threshold,
                 col = colors,
                 lwd = Lollipop_Line_Width) #change line width for lollipop plots

        par(mai = c(1, 1, 0, 1))
        plot(soundfile,
             xlab = "Time (s)",
             xlim = xaxis,
             lwd = Waveform_Line_Width,
             xaxt = "n")
        mtext(side = 1, max_amp_text, line = 2, adj = 1, cex = 1)
        axis(side=1, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 10))
        if (tick == "1" | tick == "both") {
          axis(side=1, lwd = 0.75, labels = FALSE, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 1))}
        if (tick == "5" | tick == "both") {
          axis(side=1, lwd = 0.75 , at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 5))}


        dev.off()
      }



      ######Plotting: Frequency--------

      ###save frequency plot in folder with sound files under name: NameOfSoundFile_frequency

      #tally the data for frequency bar graphs (not counting std. snap)
      plot_data <- plot_data %>%
        filter(s.start <= duration(soundfile_length))

      threshold_vector <- sort(threshold_vector)

      freq_data <- plot_data %>% group_by(threshold) %>% tally()

      freq_data$threshold <- as.factor(freq_data$threshold)


      #set colours from previous dictionary
      colors <- color_dict[as.numeric(as.character(freq_data$threshold))]

      print(paste("plotting the frequency data for file",z, "of", length(myFiles)))



      #save as a jpeg:
      if (2 %in% save_form){
        jpeg(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_freqdist.jpg", sep = ""),
             units="in", width=4, height=4, res=300)

        #plot the data
        print(ggplot(freq_data, aes(x = threshold , y = n ,
                                    fill = threshold)) +
                geom_bar(stat = "identity", width = 0.8) + #width changes the width of the bars 0-1
                theme_classic() +
                xlab("Threshold (%)") +
                scale_y_continuous(expand = c(0, 0), limits = c(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling)))) +
                #ylim(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling))) +
                ylab("Number of Signals") +
                theme(legend.position = "none") +
                geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
                scale_fill_manual(values=colors, drop = FALSE) +
                labs(caption = max_amp_text) + theme(plot.subtitle = element_text(hjust = 1)) +
                scale_x_discrete(limits = as.character(threshold_vector), drop = FALSE))


        dev.off()
      }


      #save as a tiff:
      if (3 %in% save_form){
        tiff(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_freqdist.tiff", sep = ""),
             units="in", width=4, height=4, res=300)

        #plot the data
        print(ggplot(freq_data, aes(x = threshold , y = n ,
                                    fill = threshold)) +
                geom_bar(stat = "identity", width = 0.8) + #width changes the width of the bars 0-1
                theme_classic() +
                xlab("Threshold (%)") +
                scale_y_continuous(expand = c(0, 0), limits = c(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling)))) +
                #ylim(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling))) +
                ylab("Number of Signals") +
                theme(legend.position = "none") +
                geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
                scale_fill_manual(values=colors, drop = FALSE) +
                labs(caption = max_amp_text) + theme(plot.subtitle = element_text(hjust = 1)) +
                scale_x_discrete(limits = as.character(threshold_vector), drop = FALSE))


        dev.off()
      }


      #save as a pdf:
      if (1 %in% save_form){
        pdf(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_freqdist.pdf", sep = ""),
            width=4, height=4)

        #plot the data
        print(ggplot(freq_data, aes(x = threshold , y = n ,
                                    fill = threshold)) +
                geom_bar(stat = "identity", width = 0.8) + #width changes the width of the bars 0-1
                theme_classic() +
                xlab("Threshold (%)") +
                scale_y_continuous(expand = c(0, 0), limits = c(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling)))) +
                #ylim(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling))) +
                ylab("Number of Signals") +
                theme(legend.position = "none") +
                geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
                scale_fill_manual(values=colors, drop = FALSE) +
                labs(caption = max_amp_text) + theme(plot.subtitle = element_text(hjust = 1)) +
                scale_x_discrete(limits = as.character(threshold_vector), drop = FALSE))

        dev.off()
      }


    } else {

      #####NO STD SNAP--------------

      #colours-----
      palette1 <- colorRampPalette(c("grey", "black"))
      palette1 <- palette1(10)

      palette2 <- colorRampPalette(c("black", "darkblue"))
      palette2 <- palette2(10)

      palette3 <- colorRampPalette(c("darkblue", "steelblue1"))
      palette3 <- palette3(10)

      palette4 <- colorRampPalette(c("steelblue1", "green4"))
      palette4 <- palette4(10)

      palette5 <- colorRampPalette(c("green4", "palegreen2"))
      palette5 <- palette5(10)

      palette6 <- colorRampPalette(c("palegreen2", "lightgoldenrod1"))
      palette6 <- palette6(10)

      palette7 <- colorRampPalette(c("lightgoldenrod1", "orange"))
      palette7 <- palette7(10)

      palette8 <- colorRampPalette(c("orange", "firebrick"))
      palette8 <- palette8(10)

      palette9 <- colorRampPalette(c("firebrick", "red"))
      palette9 <- palette9(10)

      palette10 <- colorRampPalette(c("red", "maroon1"))
      palette10 <- palette10(10)

      color_dict <- c(palette1, palette2, palette3, palette4,
                      palette5, palette6, palette7, palette8,
                      palette9, palette10)


      #remove the standardizing snap added to end
      plot_data <- plot_data %>%
        filter(s.start <= duration(soundfile_length))


      #set the colours of the plot
      plot_data$sound <- plot_data$threshold
      plot_data$sound <- as.factor(plot_data$sound)


      #assign colour as threshold value
      colors <- color_dict[as.numeric(as.character(plot_data$sound))]



      #Plotting: Lollipop and Waveform--------------------


      ###save waveform/lollipop plot in folder with sound files under name: NameOfSoundFile_waveform

      print(paste("plotting the data as a waveform for file",z, "of", length(myFiles) ))

      #save as jpeg
      if (2 %in% save_form){
        jpeg(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_waveform.jpg", sep = ""),
             units="in", width=20, height=10, res=300)

        par(mfrow = c(2, 1))


        xaxis <- c(0, duration(soundfile_length))

        #plot the lollipop graph:
        par(mai = c(0, 1, 1, 1))
        plot(x = plot_data$s.start,
             y = plot_data$threshold,
             col = colors,
             pch = Point_Type,
             cex = Point_Size,
             xaxt = "n",
             ylab = "Threshold (%)",
             ylim = c(0, 100),
             las = 2,
             xlim = xaxis)
        segments(x0 = plot_data$s.start ,
                 y0 = 0,
                 x1 = plot_data$s.start,
                 y1 = plot_data$threshold,
                 col = colors,
                 lwd = Lollipop_Line_Width) #change line width for lollipop plots

        par(mai = c(1, 1, 0, 1))
        plot(soundfile_length,
             xlab = "Time (s)",
             xlim = xaxis,
             lwd = Waveform_Line_Width,
             xaxt = "n")
        mtext(side = 1, max_amp_text, line = 2, adj = 1, cex = 1)
        axis(side=1, at=seq(0, (round_any(duration(soundfile_length), 5, f = ceiling)), by = 10))
        if (tick == "1" | tick == "both") {
          axis(side=1, lwd = 0.75, labels = FALSE, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 1))}
        if (tick == "5" | tick == "both") {
          axis(side=1, lwd = 0.75 , at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 5))}

        dev.off()

      }


      #save as tiff
      if (3 %in% save_form){
        tiff(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_waveform.tiff", sep = ""),
             units="in", width=20, height=10, res=300)

        par(mfrow = c(2, 1))


        xaxis <- c(0, duration(soundfile_length))

        #plot the lollipop graph:
        par(mai = c(0, 1, 1, 1))
        plot(x = plot_data$s.start,
             y = plot_data$threshold,
             col = colors,
             pch = Point_Type,
             cex = Point_Size,
             xaxt = "n",
             ylab = "Threshold (%)",
             ylim = c(0, 100),
             las = 2,
             xlim = xaxis)
        segments(x0 = plot_data$s.start ,
                 y0 = 0,
                 x1 = plot_data$s.start,
                 y1 = plot_data$threshold,
                 col = colors,
                 lwd = Lollipop_Line_Width) #change line width for lollipop plots

        par(mai = c(1, 1, 0, 1))
        plot(soundfile_length,
             xlab = "Time (s)",
             xlim = xaxis,
             lwd = Waveform_Line_Width,
             xaxt = "n")
        mtext(side = 1, max_amp_text, line = 2, adj = 1, cex = 1)
        axis(side=1, at=seq(0, (round_any(duration(soundfile_length), 5, f = ceiling)), by = 10))
        if (tick == "1" | tick == "both") {
          axis(side=1, lwd = 0.75, labels = FALSE, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 1))}
        if (tick == "5" | tick == "both") {
          axis(side=1, lwd = 0.75 , at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 5))}

        dev.off()

      }


      #save as pdf
      if (1 %in% save_form) {
        pdf(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_waveform.pdf", sep = ""),
            width=20, height=10)

        par(mfrow = c(2, 1))


        xaxis <- c(0, duration(soundfile_length))

        #plot the lollipop graph:
        par(mai = c(0, 1, 1, 1))
        plot(x = plot_data$s.start,
             y = plot_data$threshold,
             col = colors,
             pch = Point_Type,
             cex = Point_Size,
             xaxt = "n",
             ylab = "Threshold (%)",
             ylim = c(0, 100),
             las = 2,
             xlim = xaxis)
        segments(x0 = plot_data$s.start ,
                 y0 = 0,
                 x1 = plot_data$s.start,
                 y1 = plot_data$threshold,
                 col = colors,
                 lwd = Lollipop_Line_Width) #change line width for lollipop plots

        par(mai = c(1, 1, 0, 1))
        plot(soundfile_length,
             xlab = "Time (s)",
             xlim = xaxis,
             lwd = Waveform_Line_Width,
             xaxt = "n")
        mtext(side = 1, max_amp_text, line = 2, adj = 1, cex = 1)
        axis(side=1, at=seq(0, (round_any(duration(soundfile_length), 5, f = ceiling)), by = 10))
        if (tick == "1" | tick == "both") {
          axis(side=1, lwd = 0.75, labels = FALSE, at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 1))}
        if (tick == "5" | tick == "both") {
          axis(side=1, lwd = 0.75 , at=seq(0, (round_any(duration(soundfile), 5, f = ceiling)), by = 5))}

        dev.off()

      }


      #Plotting: Frequency---------

      ###save frequency plot in folder with sound files under name: NameOfSoundFile_frequency

      #tally the data for frequency bar graphs
      threshold_vector <- sort(threshold_vector)

      freq_data <- plot_data %>% group_by(threshold) %>% tally()

      freq_data$threshold <- as.factor(freq_data$threshold)

      #set colours from complete dictionary
      colors <- color_dict[as.numeric(as.character(freq_data$threshold))]

      print(paste("plotting the frequency data for file",z, "of", length(myFiles)))


      #save as jpeg
      if (2 %in% save_form) {
        jpeg(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_freqdist.jpg", sep = ""),
             units="in", width=4, height=4, res=300)

        #plot the data
        print(ggplot(freq_data, aes(x = threshold , y = n ,
                                    fill = threshold)) +
                geom_bar(stat = "identity", width = 0.8) + #width changes the width of the bars 0-1
                theme_classic() +
                xlab("Threshold (%)") +
                scale_y_continuous(expand = c(0, 0), limits = c(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling)))) +
                #ylim(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling))) +
                ylab("Number of Signals") +
                theme(legend.position = "none") +
                geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
                scale_fill_manual(values=colors, drop = FALSE) +
                labs(caption = max_amp_text) + theme(plot.subtitle = element_text(hjust = 1)) +
                scale_x_discrete(limits = as.character(threshold_vector), drop = FALSE))

        dev.off()
      }


      #save as tiff
      if (3 %in% save_form) {
        tiff(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_freqdist.tiff", sep = ""),
             units="in", width=4, height=4, res=300)

        #plot the data
        print(ggplot(freq_data, aes(x = threshold , y = n ,
                                    fill = threshold)) +
                geom_bar(stat = "identity", width = 0.8) + #width changes the width of the bars 0-1
                theme_classic() +
                xlab("Threshold (%)") +
                scale_y_continuous(expand = c(0, 0), limits = c(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling)))) +
                #ylim(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling))) +
                ylab("Number of Signals") +
                theme(legend.position = "none") +
                geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
                scale_fill_manual(values=colors, drop = FALSE) +
                labs(caption = max_amp_text) + theme(plot.subtitle = element_text(hjust = 1)) +
                scale_x_discrete(limits = as.character(threshold_vector), drop = FALSE))

        dev.off()

      }


      #save as pdf
      if (1 %in% save_form){

        pdf(paste(path, "/", (str_remove(myFiles[z], ".wav")), "_STD_fig_freqdist.jpg", sep = ""),
            width=4, height=4)

        #plot the data
        print(ggplot(freq_data, aes(x = threshold , y = n ,
                                    fill = threshold)) +
                geom_bar(stat = "identity", width = 0.8) + #width changes the width of the bars 0-1
                theme_classic() +
                xlab("Threshold (%)") +
                scale_y_continuous(expand = c(0, 0), limits = c(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling)))) +
                #ylim(0, (round_any((max(freq_data$n, na.rm = TRUE)), 5, f = ceiling))) +
                ylab("Number of Signals") +
                theme(legend.position = "none") +
                geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
                scale_fill_manual(values=colors, drop = FALSE) +
                labs(caption = max_amp_text) + theme(plot.subtitle = element_text(hjust = 1)) +
                scale_x_discrete(limits = as.character(threshold_vector), drop = FALSE))


        dev.off()

      }

    }

  }

}


