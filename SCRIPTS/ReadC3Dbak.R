
# ..............................................................................
# ReadC3D.R for public release. 
# See readme.txt file included with distribution for use.
# 
# Copyright 2023.
# ReadC3D.R is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 3 of the License.
#  
# ReadC3D.R is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ..............................................................................
# 
# ..............................................................................
# Authors:
#    Bruce A. MacWilliams, Shriners Children's and University of Utah
#    Michael H. Schwartz, Gillette Children's Specialty Hospital and University of Minnesota
#    
# ..............................................................................

# Should I leave this here?
library(tidyverse)

# Need to consider either making this a package or putting everything in one script file 
# https://r-pkgs.org/whole-game.html

# setwd("C:/Users/bmacwilliams/Box/Projects/Read C3D")
# 
# rm(list = ls())
# source("C:/Users/bmacwilliams/Box/Projects/Read C3D/ReadC3DParameters.R")

# setwd("C:/Users/bamac/Box/Projects/Read C3D")
# source("C:/Users/bamac/Box/Projects/Read C3D/ReadC3DParameters.R")

#### ---- Read C3D Exit ----
ReadC3DExit <- function(wstr, C3D) {
  warning(wstr)
  C3D$ExitStatus <- -1
  C3D$ExitMsg <- wstr
}

#### ---- Read C3D Main ----
ReadC3D <- function(
   C3DFileName,                    # REQUIRED, String, Full path and name of the C3D file to be loaded
   GetMarkerData = TRUE,           # OPTIONAL, Logical, If TRUE then return trajectories in C3D$VideoData
   GetAnalogData = TRUE,           # OPTIONAL, Logical, If TRUE then return analog data in C3D$AnalogData
   GetForcePlateData = TRUE,       # OPTIONAL, Logical, If TRUE then return transformed force plate data in C3D$ForcePlateData
   ForcePlateZero = TRUE,          # OPTIONAL, Logical, If TRUE then use FPZero parameter to determine force plate DC offsets, if FALSE then no offsets
   MarkerDataFormat = 'tall',      # OPTIONAL, String = c('tall', 'wide'). If 'tall' XYZ components are in single column, if 'wide' separate columns
   ForcePlateDataFrames = 'analog',# OPTIONAL, String = c('analog', 'video'). If 'analog' all ADC frames returned, if 'video' one ADC frame per video frame returned
   FirstReturnFrame = 0,           # OPTIONAL, Integer, First Video frame to return, defaults to first frame in C3D if 0
   LastReturnFrame = 0             # OPTIONAL, Integer, Last Video from to return, defaults to last frame in C3D if 0
   ) {


  # ............................................................................
  # Inputs:
  # 
  #   C3DFileName:        REQUIRED. Character of C3D file name, with complete path.
  #                       Format: (e.g.) C:/folder/folder/c3dname.c3d. For users new to R, note forward slash in path names. 
  #    
  #   GetMarkerData:      OPTIONAL. Logical TRUE/FALSE. Default = TRUE. options c(TRUE, FALSE)
  #                       TRUE: return motion data to  C3D$VideoData (Dataframe: rows of video frames, columns of marker names)
  #                       FALSE: no motion data read, MarkerData() remains empty
  #   
  #   GetAnalogData:      OPTIONAL. Logical TRUE/FALSE. Default = TRUE. options c(TRUE, FALSE)
  #                       TRUE: return analog data in C3D$AnalogData() (Dataframe: rows of analog frames, columns of analog names)
  #                       FALSE: AdcData() remains empty
  #                       
  #   GetForecPlateData:  OPTIONAL. Logical TRUE/FALSE. Default = TRUE, options c(TRUE, FALSE)
  #                       TRUE: return force data in C3D$ForcePlateData() (Dataframes: rows of analog/video frames, columns of component names)
  #                       One dataframe for each forece plate. These values are transformed from analog data into Forces, COP, and Tz
  #                       FALSE: ForcePlateData() remains empty
  #   
  #   MarkerDataFormat:  OPTIONAL. Character. Default = 'tall', options c('tall', 'wide')
  #                      'tall': Data returned with each trajectory stored in a single column, component column of XYZ is included
  #                      'wide': Data returned with each trajectory components stored column appended with .X, .Y, .Z
  #
  #   ForcePlateDataFrames: OPTIONAL. Character. Default = 'analog', options c('analog','video')
  #                      'analog': Data returned has all rows of analog data
  #                      'video': Data returned is filtered to include one row for each video frame
  #       
  #                      
  #   FirstReturnFrame:  OPTIONAL. Integer. If not present, return motion data beginning at first available
  #                      frame number. First index of MarkerData(column 1) is the first frame number.
  #                      If present, start returning data at the frame number passed. First index of
  #                      MarkerData(column 1) is FirstReturnFrame.
  #             ***NOTE: If analog or force plate data are requested, the value of FirstReturnFrame
  #                      impacts which data are returned. The ratio of analog sample rate to
  #                      video frame rate ("AnalogToVideoRate") is used to determine which
  #                      sample numbers to return.
  #                      The FIRST ANALOG sample returned corresponds, in time, to the
  #                      FIRST FRAME of video returned.
  #
  #   LastReturnFrame:   OPTIONAL. Integer. If not present, return motion data up to the last frame
  #                      number. Last index of MarkerData(column 1) is the last frame number.
  #                      If present, return data up to the selected frame number. Last index of
  #                      MarkerData(column 1) is LastReturnFrame
  #             ***NOTE: If analog or force plate data are requested, the value of LastReturnFrame
  #                      impacts which data are returned. The ratio of analog sample rate to
  #                      video frame rate ("AnalogToVideoRate") is used to determine which
  #                      sample numbers to return.
  #                      The LAST ANALOG SAMPLE returned DOES NOT correspond, in time, to
  #                      the LAST FRAME of video; instead, the LAST FRAME of video
  #                      corresponds, in time, to (LAST ANALOG SAMPLE - AnalogToVideoRate + 1)


  # Outputs:
  #
  #   Data is returned in a single list variable containing the elements below.
  #   Length of video and analog data returned is determined by C3D data and optional values FirstReturnFrame and LastReturnFrame.
  #   If these options are set to defaults (0), all video and analog frames present in the C3D file will be returned
  #   If valid options for FirstReturnFrame and/or LastReturnFrame are set then the data returned is trimmed by these values
     
  #   Header Information:
  #     C3D file name:          Complete path and file name of C3D file passed
  #     Number_of_Trajectories: Number of 3d markers written to this file
  #     Analog_Channels:        Number of analog channels written to this C3d
  #     First_Frame:            First motion frame available in this file (not first frame returned)
  #     Last_Frame:             Final motion frame available in this file (not last frame returned)
  #     Video_Sampling_Rate:    Value of camera frame rate (note: Single value)
  #     Analog_Sampling_Rate:   Value of analog sampling rate (note: Single value)
  #     Scale_Factor:           Video scale factor (only used for type DEC processors)
  #     Max_Interpolation_Gap:  Maximum 3D point reconstruction gap used to auto fill trajectories
  #     C3D_File_Format:        Processor type used to generate C3D (DEC, PC, or SGI). SGI not supported.
  #     Number_of_Events:       Number of gait events included in the header. Note that events may 
  #                             be present in the EVENT parameter group that are not contained in the 
  #                             header events.

  #   Events:
  #     Data frame where:
  #       Labels:               Event labels stored in header
  #       Times:                Event times in seconds stored in header
  #       Switches:             Event switches stored in header
  #
  #     Note the events in header may not be present at all or may be a subset of
  #     the events stored in the parameter EVENT group. Thus users should always
  #     use the events from the parameters OR as stored in the returned 
  #     GaitCycleEvents output dataframe.

  #   Parameters:
  #     Data frame where:
  #       Group:                Group number as stored in C3D parameters
  #       Parameter:            Sequential number for parameters in each group, assigned in the order read
  #       GroupName:            Name of the parameter group
  #       ParameterName:        Name of the parameter
  #       NumDimensions:        Number of dimensions that describes how each parameter is stored
  #       Dimensions:           List of the size of each dimension 1:NumDimensions
  #       DataType:             Integer value of c(-1, 1, 2, 4) where
  #                              -1 = character
  #                               1 = byte (raw)
  #                               2 = integer
  #                               3 = real 
  #       Data:                 List of parameter data stored using the previous number of dimensions, 
  #                             dimensions, and data types
  #
  #       All parameters in C3D are read and stored in their native format according to
  #       the dimension values which results in character arrays containing many values
  #       with trailing white space and some values with no white space.
  
  #  GaitCycleEvents:
  #     This list is compiled from both raw data as stored in the EVENTS group and
  #     additional calculations of events with the intent of simplifying access to 
  #     event times and frames for processing.
  #     
  #     List of mixed types where:
  #
  #       Each of the following has length of number of events used from EVENTS:USED:
  #         Labels:             Vector of event names,
  #         Descriptions:       Vector of event descriptions
  #         Contexts:           Vector of event contexts (Left, Right, General, or custom) 
  #         Times:              Vector of event times in seconds
  #         Subjects:           Vector of event subjects, which subject each event applies to
  #
  #       Each of the following has length of number of events per Left or Right Side:
  #         FootStrikeTime_L:   Vector of times of Right foot strikes (seconds)
  #         FootStrikeTime_R:   Vector of times of Left foot strikes (seconds)
  #         FootOffTime_L:      Vector of times of Left foot offs (seconds)
  #         FootOffTime_R:      Vector of times of Right foot offs (seconds)
  #         FootStrikeFrame_L:  Vector of frames of Left foot strikes (frames)
  #         FootStrikeFrame_R:  Vector of frames of Right foot strikes (frames)
  #         FootOffFrame_L:     Vector of frames of Left foot offs (frames)
  #         FootOffFrame_R:     Vector of frames of Right foot offs (frames)
  #         All Foot Strike/Off times and frames are returned in a sorted order from min to max
      
  #   VideoData: 
  #     Data frame where: 
  #       If (MarkerDataFormat = 'tall')
  #         data frame has 3*number of video frames returned observations
  #         column 1:           Coord (X, Y, or Z repeated for each video frame)
  #         column 2:           Frame (video Frame, repeated for each X,Y,Z)
  #         column 3-N:         Trajectory value named with marker or variable as stored in the POINTS:LABELS parameter
  #                             N = Number_of_Points + 2
  #
  #       If (MarkerDataFormat = 'wide')
  #         data frame has number of video frames returned observations
  #         column 1: Frame (video Frame, repeated for each X,Y,Z)
  #         column 2-N: Trajectory component value, each trajectory 3 sequential component columns (N=3*Number_of_Points + 1). 
  #                     Columns are marker names as stored in the POINTS:LABELS parameter with .X, .Y, .Z suffix
  #
  #     Note that residuals are not returned as these do not have meaning and are in fact all 0's in 'modern' C3Ds.
  #     These were initially a 2 byte value of marker resolution and 8 bits of data indicating which cameras (1-8) 
  #     contributed to the markers. Once more than 8 cameras were standard, these values were no longer written.
  
  #   AnalogData: 
  #     Data frame where
  #       column 1:           Analog frame number
  #       column 2:           Video frame number
  #       column 3:           Analog sub-frame number
  #       column 4-N:         Analog data named as stored in the ANALOG:LABELS parameter (N=Number_of_Analog_Channels + 3)
                 
  #   ForcePlateData: 
  #     List of data frames where
  #       Dimension 1 is a data frame for each force plate
  #         Each force plate data frame consists of 
  #           if ForcePlateDataFrames = 'analog'
  #             observations consist of all ADC frames = Video frames * Analog frames per video frame
  #               column 1:   Analog frame number
  #               column 2:   Video frame number
  #               column 3:   Analog sub-frame number
  #               column 4-9: Fx, Fy, Fz, CoPx, CoPy, Tz
  #
  #           if ForcePlateDataFrames = 'video'
  #             observations consist of 1 ADC frame per Video frame
  #               column 1:   Analog frame number
  #               column 2:   Video frame number
  #               column 3-8: Fx, Fy, Fz, CoPx, CoPy, Tz
  #
  #               Note that if low pass filtering of force plate data is to be performed, users will want to 
  #               return all force plate values using the default ForcePlateDataFrames = 'analog' option.
  #               
  #               Force plate data is zeroed with DC offset if C3D contains a valid zero frame range in parameter 
  #               FORCE_PLATFORM:ZERO. Currently the zero frame range used is applied to the beginning of the 
  #               C3D file. Thus if a zero range of 1:10 is read, and the first frame is 50, frames 50-59 are
  #               used. All channels are offset by the mean of that channel in the zero range
  #               
  #               Regardless of the zero frame range, all force plate channels are set to 0 for every frame 
  #               that the Fz component is below a threshold value. This is currently set to 0.005 (half of one percent)
  #               of the detected maximum vertical force range, or 1N in case the force plate is unloaded.
  # ............................................................................


  # Read binary type C3D Input File into con
  ##### -----  Test Data ----
  # C3DFileName <- 'Sample C3Ds/TD038AS_16.c3d'
  # C3DFileName <- 'Sample C3Ds/666090 21.c3d'
  # C3DFileName <- 'Sample C3Ds/TD039A_A18.c3d'
  # C3DFileName <- 'Sample C3Ds/Nexus 2008.c3d'
  # C3DFileName <- 'Sample C3Ds/Workstation 2007.c3d'
  # C3DFileName <- 'Sample C3Ds/Workstation 1997.C3D'
  # C3DFileName <- 'Sample C3Ds/180040 7.c3d'
  # C3DFileName <- 'Sample C3Ds/507168 24.c3d' 
  # C3DFileName <- 'Sample C3Ds/TYPE-3.c3d' # Works
  # C3DFileName <- 'Sample C3Ds/TYPE-4.c3d' 
  # C3DFileName <- 'Sample C3Ds/C3DTestSuite/Eb015pr.c3d'
  # C3DFileName <- 'Sample C3Ds/C3DPointFrames/18124framesf.c3d' # 
  # C3DFileName <- 'Sample C3Ds/C3DPointFrames/36220framesi.c3d' # works
  # C3DFileName <- 'Sample C3Ds/C3DPointFrames/72610framesf.c3d' # works
  # C3DFileName <- 'Sample C3Ds/C3DCompanies/arthuman-sample.c3d'
  # C3DFileName <- 'Sample C3Ds/147068 53.c3d'
  # C3DFileName <- 'Sample C3Ds/C3DCompanies/Vicon pyCGM2 lower limb CGM24 Walking01.c3d'
  # C3DFileName <- 'Sample C3Ds/648385 22.c3d'
  # C3DFileName <- 'Sample C3Ds/999978 15.c3d'
  # C3DFileName <- 'Sample C3Ds/999977 18.c3d'
  # C3DFileName <- 'Sample C3Ds/PyOut16_13.c3d'
  # C3DFileName <- 'Sample C3Ds/Bad_Analog_Units.c3d'
  # # 
  # FirstReturnFrame <- 0
  # LastReturnFrame <- 0
  # GetMarkerData <- TRUE
  # GetAnalogData <- TRUE
  # GetForcePlateData <- TRUE
  # ForcePlateZero <- TRUE
  # MarkerDataFormat <- 'wide'
  # ForcePlateDataFrames <- 'analog'
  # ..............................................................................
  
  # Initialize empty C3D list for return
  C3D <- list(
    'Header' = NULL,
    'Events' = NULL,
    'Parameters' = NULL,
    'GaitCycleEvents' = NULL,
    'ExitStatus' = 0,
    'ExitMsg' =  NULL 
  )
  
  # Read binary type C3D Input File into con
  if (file.exists(C3DFileName)) {
    con = file(C3DFileName, "rb")
  } else {
    wstr <- paste('File', C3DFileName, 'not found \n')
    ReadC3DExit(wstr, C3D)
    return(C3D)
  }
  
  #### ---- Read Header ----
  # Read the C3D header information
  # Word 1: Read the C3D header record
  #   Byte 1: number of 1st parameter record
  FirstParameterRecordNumber <- as.integer(readBin(con, raw(), n = 1, size = 1))
  #   Byte 2:  must contain 80 (decimal)
  Header <- as.integer(readBin(con, raw(), n = 1, size = 1))
  
  # Next, skip to beginning of the parameter section of the C3D file to determine processor type
  # Point to the first parameter block
  FirstParameterByteNumber <- 512 * (FirstParameterRecordNumber - 1)
  # cbo = Current Byte Offset returned by seek()
  cbo <- seek(con, where=FirstParameterByteNumber, origin = 'start')
  
  # Read the first 4 bytes as single byte integers
  ParamHeader <-readBin(con, integer(), n = 4, size = 1)
  
  # Fourth word of parameter record designates processor type
  # value is (83 + 1,2,or 3) where 1 is PC-DOS, 2 is DEC, 3 is SGI
  ProcessorType <- case_when(
    ParamHeader[4] == 84 ~ 'PC',
    ParamHeader[4] == 85 ~ 'DEC',
    ParamHeader[4] == 86 ~ 'SGI',
    TRUE ~ 'UNKNOWN')
    
  # Currently the SGI/MIPS format is not supported
  if (ProcessorType == 'SGI') {
    wstr <- 'ReadC3D:Read Header: attempt to read SGI-generated file, SGI not supported \n
            Fatal error. \n'
    ReadC3DExit(wstr, C3D)
    return(C3D)
  }
  
  if (ProcessorType == 'UNKNOWN') {
    wstr <- 'ReadC3D:Read Header: Unknown processer type detected \n'
    ReadC3DExit(wstr, C3D)
    return(C3D)
  }
  
  # Go back to the header, start at byte 2
  cbo <- seek(con, where = 2, origin = 'start')
  
  # Word 2: Number of 3D points (markers), unsigned 2 byte integer
  NumberOfMarkers <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)
  
  # Flag for 3D data inclusion
  VideoData <- TRUE
  if (NumberOfMarkers == 0) {
    VideoData <- FALSE
    if (GetMarkerData) {
      warning("ReadC3D:Read Header: No marker trajectories found in C3D \n")
      #If no markers, force GetMarkerData to FALSE
      GetMarkerData <- FALSE
    }
  }
  
  # Word 3: Number of analog channels for which data is stored, unsigned 2 byte integer
  NumberOfAnalogChannelsPerFrame <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)
  
  # Word 4: Number of first video frame in .C3D file, unsigned 2 byte integer
  # C3D User Guide cautions against using header information for number of frames.
  # These entries have a 2 byte upper limit which can cause issues, read here to 
  # report what is actually in header, but use values from POINT parameters for coding
  FirstC3DFrameNumber <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)
  
  # Word 5: Number of last video frame in .C3D file, unsigned 2 byte integer
  # Max value of 65535
  LastC3DFrameNumber <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)
  
  # Word 6: Maximum interpolation gap allowed (in frames), unsigned 2 byte integer
  MaxInterpolationGap <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)
  
  # Word 7,8: (REAL*4) Scale factor for converting integer 3D data to reference system units. 
  # If negative, 3D data is already in REAL*4 format.
  # Account for PC/DEC storage, also start a DEC flag 
  if (ProcessorType == 'DEC') {
    DEC <- TRUE
    VideoScaleFactor <- DECBytes2Single(readBin(con, raw(), n = 4))
  } else {
    DEC <- FALSE
    VideoScaleFactor <- readBin(con, double(), n = 1, size = 4)
  }
  
  # Word 9: Starting record number for 3D point and analog data, unsigned 2 byte integer
  FirstDataRecordNumber <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)
  
  # Word 10: Number of analog frames / video frame, unsigned 2 byte integer
  AnalogToVideoRate <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)
  
  # Word 11,12:  (REAL*4) Video frame rate (Hz), this is not necessarily an integer
  if (DEC) {
    FrameRate <- DECBytes2Single(readBin(con, raw(), n = 4))
  } else {
    FrameRate <- readBin(con, double(), n = 1, size = 4)
  }
  
  # Calculated parameters from header file
  if (NumberOfAnalogChannelsPerFrame != 0 & AnalogToVideoRate != 0) {
    # Flag for presence of any analog data
    AnalogData <- TRUE
    NumberOfAnalogChannels <- NumberOfAnalogChannelsPerFrame / AnalogToVideoRate
  } else {
    NumberOfAnalogChannels <- 0
    AnalogData <- FALSE
    if (GetAnalogData == TRUE | GetForcePlateData == TRUE) {
      GetAnalogData <- FALSE
      GetForcePlateData <- FALSE
      warning("ReadC3D:Read Header: No Analog Data Detected \n")
    }
  }
  
  # 3D & analog data format (Real or Integer) designated by sign of VideoScaleFactor (aka POINT:SCALE)
  if (VideoScaleFactor < 0) {
    Real <- TRUE
    DataFormat <- "Real"
    VideoByteLength <- NumberOfMarkers * 16             # 16 bytes per marker =  x,y,z,residual&contribution
    AnalogSampleLength <- NumberOfAnalogChannels * 4    # Assumes 4 bytes per data point
    VideoScaleFactor <- 1                               # Set to 1 for no scaling
  } else {
    Real <- FALSE
    DataFormat <- "Integer"
    VideoByteLength <- NumberOfMarkers * 8               # 8 bytes per marker =  x,y,z,residual&contribution
    AnalogSampleLength <- NumberOfAnalogChannels * 2     # Assumes 2 bytes per data point
  }
  
  # Calculate Parameters
  AnalogSampleRate <- FrameRate * AnalogToVideoRate
  FirstDataByteNumber <- 512 * (FirstDataRecordNumber - 1)
  ParameterByteSize <- FirstDataByteNumber - FirstParameterByteNumber
  if (ParameterByteSize <= 0) {
    warning("ReadC3D:Read Header: Invalid ParameterByteSize \n")
  }
  
  #### ---- Read Events ----
  # Word 13-149 unused, go to word 150
  cbo <- seek(con, where = 149*2, origin = 'start')
  
  # Word 150: 12345(decimal) keyword to identify presence of event data
  EventFlag <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)
  
  # If there are events in header, then read events
  if (EventFlag == 12345) {
    # Word 151: Number of defined time events -- maximum of 18
    NumberOfEvents <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)

    # Word 152 not used
    # Word 153-188 (REAL*4) Event times (in seconds) -- maximum of 18
    cbo <- seek(con, where = 152*2, origin = 'start')
    HeaderEventTimes <- double(NumberOfEvents)
    if (DEC) {
      HeaderEventTimes <- numeric(NumberOfEvents)
      for (i in 1:NumberOfEvents) {
        HeaderEventTimes[i] <- DECBytes2Single(readBin(con, raw(), n = 4))
      }
    } else {
      HeaderEventTimes <- readBin(con, double(), n = NumberOfEvents, size = 4)
    }
    
    # Word 189-198 (BYTE*1) Event display switches (0=OFF, 1=ON)
    cbo <- seek(con, where = 188*2, origin = 'start')
    Bytes <- readBin(con, raw(), n = NumberOfEvents)
    HeaderEventSwitches <- if_else(Bytes == 01,  'ON', 'OFF')
    
    # Word 199-234 Event labels. 4 characters for each event.
    cbo <- seek(con, where = 198*2, origin = 'start')
    Str <-rawToChar(readBin(con, raw(), n = 4*NumberOfEvents))
    HeaderEventLabels <- unlist(strsplit(Str, paste0("(?<=.{", 4, "})", "(?=.)"), perl=TRUE))
  } else {
    # No events
    NumberOfEvents <- 0
  }
  
  #### ---- Read Parameters ---- 
  # This function is only called once, but stored in a separate script due to 
  # length and complexity. Parameters are returned in a dataframe from which 
  # single parameter values can be read using the GetParameterData() function
  Parameters <- ReadC3DParameters(con)
  
  #### ---- Get Trial Parameters ----
  # Only Call optional group subroutines if that group is present in the file
  
  # Camera_Rate <- GetParameterData(Parameters,  "TRIAL", "CAMERA_RATE")
  
  #### ---- Get Point Parameters ---- 
  # Get point names and parameters to scale stored 3D values
  # Even if there are no markers there is still a POINT group with valid USED and FRAMES parameters
  
  # Number of points used, these can be markers, modeled markers, or variables
  # This is the same as NumberOfMarkers from header so don't actually need to read parameter
  # NumberOfC3DPoints <- GetParameterData(Parameters, "POINT", "USED")

  # Calculate number of frames in C3D from header info, these should always exist
  # even if they are wrong. If max is detected set to -1
  if (LastC3DFrameNumber == 65535) {
    HeaderC3DFrames <- -1
  } else {
    HeaderC3DFrames <- LastC3DFrameNumber - FirstC3DFrameNumber + 1
  }
  
  # C3D User Guide recommends using POINT:FRAMES for number of frames, not header info
  # PointFrames will be -1 if >2^16 frames
  # Keep warning on for this one as it should always be there:
  PointFrames <- GetParameterData(Parameters, "POINT", "FRAMES")
  if (!is.na(PointFrames)) {
    # Account for signed integer possibility from PointFrames
    if (PointFrames < -1) {
      NumberOfC3DFrames <- PointFrames + 65536
    } else {
      NumberOfC3DFrames <- PointFrames
    }
  } else {
    # Use the header value though this should never happen unless > 2^16 frames
    # in which case both HeaderC3DFrames and PointFrames = -1
    NumberOfC3DFrames <- HeaderC3DFrames
  }

  # If HeaderC3DFrames == -1 this indicates overflow of number of frames for 
  # integer storage. This will also be reflected by the header variable 
  # LastC3DFrameNumber == 65535
  
  # Handle files with number of frames exceed integer storage as read in the header.
  # This is system dependent: 
  # Vicon writes two parameters for all files  TRIAL:ACTUAL_START_FIELD and 
  # TRIAL:ACTUAL_END_FIELD. For files with less than 2^16 frames, these return 
  # the same values as the header values read here as FirstC3DFrameNumber and 
  # LastC3DFrameNumber. 
  # C-Motion stores the total frame count as a single floating-point value in
  # the parameter POINT:LONG_FRAMES. 
  
  # TRIAL:ACTUAL_START_FIELD and TRIAL:ACTUAL_END_FIELD  return two signed 
  # integers in a vector c(low_int, high_int), need to combine to long integer.
  # According to Edi Cramp, this is a Vicon issue, these are not written correctly 
  # as scalar so need to convert. BUT, as these are read in the header, there is no 
  # reason to read these again from teh parameter section unless the frames 
  # exceed integer storage 2^16 in which case the header will return 
  # LastC3DFrameNumber = 65535. 
  # In theory the integer representing the higher bytes could also be negative,
  # but that is not realistic so we will not attempt to convert. 
  
  if (NumberOfC3DFrames == -1) {
    # First address the Vicon method
    # Not realistic that the first frame could be > 2^16 but check anyway ...
    # Account for possible NA return
    ints <- as.integer(GetParameterData(Parameters,  "TRIAL", "ACTUAL_START_FIELD", Warn = FALSE))
    if(!is.na(ints[1])) {
      # low_int value can be negative since it is stored as a signed int
      if (ints[1] < 0) {ints[1] = ints[1] + 65536}
      # Combine unsigned low_int and high_int into long int
      FirstC3DFrameNumber <- ints[1] + 65536 * ints[2]
    }
    # Repeat for ACTUAL_END_FIELD
    ints <- as.integer(GetParameterData(Parameters,  "TRIAL", "ACTUAL_END_FIELD", Warn = FALSE))
    if(!is.na(ints[1])) {
      if (ints[1] < 0) {ints[1] = ints[1] + 65536}
      LastC3DFrameNumber <- ints[1] + 65536 * ints[2]
      NumberOfC3DFrames <- LastC3DFrameNumber - FirstC3DFrameNumber + 1
    }
    # Next check for LONG_FRAMES option which stores the number of frames, not start/end
    # Account for possible NA return
    lf <-  GetParameterData(Parameters, "POINT", "LONG_FRAMES", Warn = FALSE)
    if (!is.na(lf)) {
      NumberOfC3DFrames <- lf
    } 
  }
  
  # It is possible that some system storage for >2^16 frames has not been detected 
  # and NumberOfC3DFrames still does not have a real value if none of the above 
  # parameters were found so warn and use 2^16 frames:
  if (NumberOfC3DFrames == -1) {
    NumberOfC3DFrames <- LastC3DFrameNumber - FirstC3DFrameNumber + 1
    warning(paste('Header frame length is maximum and no parameters found\n',
            'Data will be trimmed to 65536 frames'))
  }
  
  # If a file is corrupt there still may not be a value so exit
  if ((is.na(PointFrames)) & (HeaderC3DFrames == -1)) {
    ReadC3DExit("Number of frames not read", C3D)
  }
  
  # From C3D Manual: Traditionally, all integers used in the parameter section were 
  # stored as one's complement signed integers with a range of –32767 to +32767 
  # and all bytes were one's complement signed bytes with a range of –127 to +127. 
  # However, some parameters may use unsigned integers to store data that will never 
  # have a negative value. There is no flag to indicate that a C3D file uses 
  # unsigned integers in the parameter section.
  
  # Pretty sure these are now an obsolete conditions
  # So if NumberOfC3DFrames is negative we need to adjust
  # if (NumberOfC3DFrames < 0) {NumberOfC3DFrames <- NumberOfC3DFrames + 65536}
  
  # Account for 2 byte limitation in end frame value from header if there are more than 65,535 frames
  # if (NumberOfC3DFrames > LastC3DFrameNumber) {
  #   LastC3DFrameNumber <- NumberOfC3DFrames + FirstC3DFrameNumber - 1
  # }
  
  # Verify that the (optional) user requested start frame number is valid for this data set
  if (((FirstReturnFrame > 0) & (FirstReturnFrame < FirstC3DFrameNumber)) | (FirstReturnFrame > LastC3DFrameNumber)) {
    wstr <- paste0('ReadC3D:Trial Parameters: Specified FirstReturnFrame is invalid! Valid frame range is ', FirstC3DFrameNumber, ' to ', LastC3DFrameNumber, '\n')
    ReadC3DExit(wstr, C3D)
    return(C3D)
  } else if (FirstReturnFrame == 0) {
    # Default: Set the StartFrame to the first valid video frame number
    FirstReturnFrame <- FirstC3DFrameNumber
  }
  
  # Verify that the (optional) end frame number is valid for this data set
  if ((LastReturnFrame > 0) & (LastReturnFrame > LastC3DFrameNumber)) {
    wstr <- paste0('ReadC3D:Trial Parameters: Specified LastReturnFrame is invalid! Valid frame range is ', FirstC3DFrameNumber, ' to ', LastC3DFrameNumber, '\n')
    ReadC3DExit(wstr, C3D)
    return(C3D)
  } else if (LastReturnFrame == 0) {
    # Default: Set the StartFrame to the first valid video frame number
    LastReturnFrame <- LastC3DFrameNumber
  }
  
  # Verify the start frame is before or equal to the end frame
  if (FirstReturnFrame > LastReturnFrame) {
    wstr <- paste0('ReadC3D:Trial Parameters: Specified FirstReturnFrame is after specified LastReturnFrame. Valid frame range is ', FirstFrameNumber, ' to ', LastFrameNumber, '\n')
    ReadC3DExit(wstr, C3D)
    return(C3D)
  }
  
  # Get other point parameters only if there is any marker data
  if (VideoData) {
    # Read Point:Labels parameter group 
    MarkerNames <- GetParameterData(Parameters, "POINT", "LABELS")
    
    # Likely a Vicon issue as there is no need to store this way, but Vicon files
    # limit the point labels and descriptions parameters to 255 entries.
    # If there are more points then these are stored in LABELS2, LABELS3, etc. 
    # and DISCRIPTIONS2, DISCRIPTIONS3, etc. Arrays are from 1:255 so not 256. 
    # Find the number of label groups in Parameters
    NLabels <- nrow(Parameters %>%
      dplyr::filter(GroupName == 'POINT') %>%
      dplyr::filter(grepl('LABELS', ParameterName)))
    
    # Use GetParameterData() to deal with the character lengths (can't rely on 
    # whitespace so break up strings, need to use dimensions and these can 
    # change between groups)
    
    # If there is more than one LABELS parameter then read and concatenate
    if (NLabels > 1) {
      for (i in 2:NLabels) {
        # Label for next group
        strlbl <- paste0('LABELS', i)
        # Get data for next group
        MoreNames <- GetParameterData(Parameters, "POINT", strlbl)
        # Join to current list
        MarkerNames <- c(MarkerNames, MoreNames)
      }
    }
    
    # In some cases there are blank labels not associated with actual markers so 
    # more marker names than NumberofMarkers 
    MarkerNames <- MarkerNames[1:NumberOfMarkers]
    MarkerNames <- make.names(MarkerNames, unique = TRUE)
    
    # Read Point:Descriptions parameter group (unused, no need to read, plus these are generally blank)
    # MarkerDescriptions <- GetParameterData(Parameters, "POINT", "DESCRIPTIONS")
    
    # Read Point:Units parameter group 
    MarkerUnits <- GetParameterData(Parameters, "POINT", "UNITS")
    
    # Want the marker positions specified in millimeters, scale if not
    MarkerUnitsScaleFactor <- 
      case_when(
        #already in the units desired
        grepl('mm', MarkerUnits) ~ 1,
        grepl('cm', MarkerUnits) ~ 10,
        grepl('m', MarkerUnits) ~ 1000,
        TRUE ~ -1
      )
    
    if (MarkerUnitsScaleFactor== -1) {
      wstr <- 'ReadC3D:Point Parameters: Error: 3D point data in nonstandard units. \n'
      ReadC3DExit(wstr, C3D)
      return(C3D)
    }
  
  } # if (VideoData)
  
  #### ---- Analog Parameters ----
  # Get analog names and parameters needed to offset and scale stored values
  # Get these only if there is analog data
  if (AnalogData) {
    # Offset values
    Analog_Offset <- GetParameterData(Parameters, 'ANALOG', 'OFFSET')
    # MAC Systems apparently label this as "OFFSETS"
    if (is.na(Analog_Offset[1])) {
      Analog_Offset <- GetParameterData(Parameters, 'ANALOG', 'OFFSETS')
    }
    # Trim to number of analog channels, these can sometimes have move entries than channels
    Analog_Offset <- Analog_Offset[1:NumberOfAnalogChannels]
    # Channel scale factors
    Analog_Scale_Multiplier <- GetParameterData(Parameters, 'ANALOG', 'SCALE')
    # Trim to number of analog channels, these can sometimes have move entries than channels
    Analog_Scale_Multiplier <- Analog_Scale_Multiplier[1:NumberOfAnalogChannels]
    # General scale factor
    Analog_General_Scale <- GetParameterData(Parameters, 'ANALOG', 'GEN_SCALE')
    # Units
    Analog_Units <- GetParameterData(Parameters, 'ANALOG', 'UNITS')
    # Trim to number of analog channels, these can sometimes have move entries than channels
    Analog_Units <- Analog_Units[1:NumberOfAnalogChannels]
    # Want our units to be standardized to Volts and Nmm, if Nm scale by 1000 by channel scale
    Analog_Scale_Multiplier <- 
      case_when(
        grepl('mm', Analog_Units) ~ Analog_Scale_Multiplier, # already in the units desired
        grepl('mV', Analog_Units) ~ 1000 * Analog_Scale_Multiplier,
        grepl('m', Analog_Units) ~ 1000 * Analog_Scale_Multiplier,
        TRUE ~ Analog_Scale_Multiplier
      )
    
    # First, last, and number of analog frames to be returned
    FirstReadADC <- (FirstReturnFrame - 1) * AnalogToVideoRate + 1 
    LastReadADC <- LastReturnFrame * AnalogToVideoRate
    NumberOfADCFrames <- NumberOfC3DFrames * AnalogToVideoRate
    
  } # if (AnalogData)

  #### ---- Read All Data ---- 
  # Instead of only reading marker or analog, read everything all at once since the 
  # data are interwoven and the entire block has to be read anyway. This should be 
  # faster. By everything I mean everything. Even if user passes start and stop return frames
  # read in all data because force plate offset ranges may be outside of start/stop frames
  # and needed for zeroing.
  
  # Data Structure
  # For 1 Video Frame:
  #   All Markers 4 bytes for X, Y, Z, R if Real or 2 bytes if int
  #   All Analog data for that frame
  #   Each analog sub-frame of data:
  #     4 bytes for each analog channel if Real or 2 bytes if int
  
  # Check to make sure the passed values will work
  if (VideoData) {
    if (FirstReturnFrame == 0) {warning("ReadC3D:Read All Data: FirstReturnFrame = 0 \n")}
    if (LastReturnFrame == 0) {warning("ReadC3D:Read All Data: LastReturnFrame = 0 \n")}
    if (NumberOfMarkers == 0) {warning("ReadC3D:Read All Data: NumberOfMarkers = 0 \n")}
  }
  
  if (AnalogData) {
    if (FirstReadADC == 0) {warning("ReadC3D:Read All Data: FirstReadADC = 0 \n")}
    if (LastReadADC == 0) {warning("ReadC3D:Read All Data: LastReadADC = 0 \n")}
    if (NumberOfAnalogChannels == 0) {warning("ReadC3D:Read All Data: NumberOfAnalogChannels = 0 \n")}
  }
  
  # Set pointer to start of video data
  cbo <- seek(con, where=FirstDataByteNumber, origin = 'start')
  # Read in all bytes for all frames as real values 
  # readBin has an object property, should try to see if you can read a matrix directly into it
  # Account for XYZ and Residuals 
  VidCols <- 4 * NumberOfMarkers + NumberOfAnalogChannelsPerFrame
  if (Real) {
    if (DEC) {
      # # if DEC/Real need to step through and do the byte conversions
      # VideoBytes <- numeric(VidCols * NumberOfC3DFrames)
      # for (f in 1:(VidCols * NumberOfC3DFrames)) {
      #   Bytes <- readBin(con, raw(), n=4)
      #   VideoBytes[f] <- DECBytes2Single(Bytes)
      # }
      
      # New method, change byte string indicies, above is simpler but vectorized method should be faster
      # Read raw vector
      VideoBytes <- readBin(con, raw(), n = (4 * VidCols * NumberOfC3DFrames))
      # Byte reordering
      dec_index <- c(3, 4, 1, 2)
      # Convert data to matrix 
      VideoBytes <- matrix(VideoBytes, nrow=4, ncol=(VidCols * NumberOfC3DFrames))
      # Use indicies to swap
      VideoBytes <- VideoBytes[dec_index, ]
      # Now compute floats, divide by four to account for exponent adjustment
      # Equivalent to subtracting 01 from last byte 
      VideoBytes <- readBin(VideoBytes, numeric(), size = 4, n = (VidCols * NumberOfC3DFrames)) / 4
    } else { 
      # PC/Real data format
      VideoBytes <- readBin(con, numeric(), size = 4, n = (VidCols * NumberOfC3DFrames))
    }
  } else {
    # Integer format
    VideoBytes <- readBin(con, integer(), size = 2, n = (VidCols * NumberOfC3DFrames))
  }
  
  # Reshape VideoBytes into two matrices
  # 1) VideoData: All marker and caclulated 3D variables
  # 2) ADCData: All analog channels
  # Start by making a matrix with rows of frame numbers and columns will be 3D data 
  # and every analog having NumberOfAnalogChannelsPerFrame columns
  VideoMatrix <- matrix(VideoBytes, nrow = NumberOfC3DFrames, ncol = VidCols, byrow = TRUE)
  # VideoBytes no longer needed
  rm(VideoBytes)
  # Now split this matrix of everything into Marker and ADC matrices
  
  #### ---- Create Video Data Frame ----
  # Video data in columns of NumberOfMarkers by rows of NumberOfC3DFrames
  # Use the Marker Data columns from VideoMatrix
  if (VideoData) {
    MarkerData <- VideoMatrix[1:NumberOfC3DFrames, 1:(4*NumberOfMarkers)] * VideoScaleFactor * MarkerUnitsScaleFactor
    # Reshape to tall by XYZR
    MarkerData <- matrix(MarkerData, nrow = (4*NumberOfC3DFrames), ncol = NumberOfMarkers)
    # Make MarkerData matrix into a data frame with ID columns for XYZR coordinates and video frame numbers
    Coord <- c(rep('X', NumberOfC3DFrames), rep('Y', NumberOfC3DFrames), rep('Z', NumberOfC3DFrames), rep('R', NumberOfC3DFrames))
    Frame <- rep(FirstC3DFrameNumber:LastC3DFrameNumber, 4)
    # Additionally apply marker names to columns, remove all residual rows, and filter to optionally requested frame range
    MarkerData <- 
      MarkerData %>% 
      data.frame() %>% 
      setNames(MarkerNames) %>%
      add_column(Coord, Frame) %>%
      dplyr::filter(Coord != 'R') %>%
      dplyr::filter(between(Frame, FirstReturnFrame, LastReturnFrame)) %>%
      relocate(c(Coord, Frame))
  
    # If 'wide' format requested, make wide by XYZ using Coord column
    if (MarkerDataFormat == 'wide') {
      MarkerData <- MarkerData %>%
        pivot_wider(
          id_cols = Frame, 
          names_from = Coord,
          values_from = 3:length(MarkerData)
        )
    }
  } # if (VideoData)
  
  #### ---- Create Analog Data Frame ----
  # Repackage Analog Data into single data frame
  if (AnalogData) {
    # Analogs are stored in columns of 1:analogchannel repeated AnalogToVideoRate times
    # Use the remaining columns of the VideoMatrix matrix which contain ADC channel data
    ADCData <- VideoMatrix[1:NumberOfC3DFrames, (4*NumberOfMarkers+1):VidCols] 
    # Videomatrix no longer needed
    rm(VideoMatrix)
    # Reshape into 3D array compliant with dimensions and data storage order
    ADCData <- array(ADCData, dim=c(NumberOfC3DFrames, NumberOfAnalogChannels, AnalogToVideoRate))
    # Permutate so that sub-frames are first dimension
    ADCData <- aperm(ADCData, c(3,1,2))
    # Reshape back to matrix so that sub-frames are now repeated through video frames
    ADCData <- matrix(ADCData, nrow = NumberOfADCFrames, ncol = NumberOfAnalogChannels)
    # Scale and offset: Offset values are not scaled so need to be applied first then scaled
    # ((ADCData'[frames x channels]-Offset[1xchannels]) * Channel_Scale[1xchannels]) * General_Scale(scalar)
    ADCData <- t((t(ADCData) - Analog_Offset) * Analog_Scale_Multiplier) * Analog_General_Scale
    
    # Make ADCData into a data frame with ID 3 ID columns: ADC frames (each observation), Video frames, Sub-frames 
    # Set the column names as stored in ANALOG:LABELS parameter
    AnalogNames <- GetParameterData(Parameters, "ANALOG", "LABELS")
    # Trim to number of analog channels
    AnalogNames <- AnalogNames[1:NumberOfAnalogChannels]
    AnalogNames <- make.names(AnalogNames, unique = TRUE)
    # SubFrames = sub samples of analog data for each video frame
    SubFrames <- rep(1:AnalogToVideoRate, NumberOfC3DFrames)
    VideoFrames <- rep(FirstC3DFrameNumber:LastC3DFrameNumber, each = AnalogToVideoRate)
    # Don't filter ADCData for Start/End frames yet as this data may be needed for force plate offsets
    ADCData <- 
      ADCData %>% 
      data.frame() %>% 
      setNames(AnalogNames) %>%
      mutate(
        AnalogFrames = FirstReadADC + row_number() - 1
      ) %>%
      relocate(AnalogFrames) %>%
      add_column(VideoFrames, SubFrames) %>%
      relocate(c(VideoFrames, SubFrames), .after = AnalogFrames)
  } # if (AnalogData)

  #### ----  Force Plate Data ----
  # Express moments as CoPx, CoPy and Tz, create separate data frames for each plate
  # Only run this if asked for and there is analog data ...
  if (GetForcePlateData & AnalogData) {
    # Make sure there are force plates used ...
    FP_Used <- GetParameterData(Parameters, 'FORCE_PLATFORM', 'USED')
    if (is.na(FP_Used)) {FP_Used <- 0} 
    if (FP_Used == 0) { 
      warning("ReadC3D:Force Plate Data: No Forceplates Used \n") 
      GetForcePlateData <- FALSE
    } else {
        # Initialize variable for force plate name storage
        FPName <- character(FP_Used)
        # Determine what format the force plate output was written to c3d file. Valid types are 1-4.
        FP_Type <- GetParameterData(Parameters, "FORCE_PLATFORM", "TYPE")
        # Read in the location of the origins of the force plates (these are offsets from FP centers, not lab origins)
        FP_Origin <- matrix(GetParameterData(Parameters,  "FORCE_PLATFORM", "ORIGIN"), nrow = 3, ncol = FP_Used)
        # Because the AMTI force plate manual has the wrong sense of the origin
        # vector, must verify that the Z value is negative. If not, negate the vector
        FP_Origin[3,] <- -abs(FP_Origin[3,])
        # Read in the frame numbers associated with determining "zero" values (start zero, end zero)
        FP_Zero <- GetParameterData(Parameters, "FORCE_PLATFORM", "ZERO")
        # Determine which analog channels correspond to the force plate output
        # Returns channels in rows for each column of force plates
        # For mixed force plate types channel rows not used are 0, need to remove
        FP_Channel <- GetParameterData(Parameters, "FORCE_PLATFORM", "CHANNEL")
        
        # Determine if Offset Range is wanted and if so valid, set Offset = TRUE or FALSE
        # Don't report back anything if ForcePlateZero is TRUE but range is invalid, as
        # this just means defaulting to what was written in the C3D file. Could include an option
        # to override this in case where C3D parameters indicate not to zero but user wants to zero
        # anyway. Offset is TRUE if all the following conditions are met:
        Offset <- ((FP_Zero[1] <= FP_Zero[2]) & 
                   (FP_Zero[1] > 0) & 
                   (FP_Zero[2] > 0) & 
                   (FP_Zero[2] <= NumberOfC3DFrames)) &
                    ForcePlateZero 
        
        # Make output array for force plate data: N force plates, 
        # analog frame rows x 6 columns (Fx, Fy, Fz, CoPx, CoPy, Tz)
        for (FP in 1:FP_Used) {
          # Repackage ADCData for each force plate using FP channels, remove 0 channels and offset by the 3 ID columns
          channels <- FP_Channel[, FP]
          channels <- channels[!channels == 0] + 3
          FPData <- ADCData %>%
            select(all_of(channels))
   
          # First calculate offsets for each channel if specified
          # Do this on the raw data before transformations
          # Fix zero frame if set to 0
          if (FP_Zero[1] == 0) {FP_Zero[1] <- 1}
          
          # Check if user wants the baseline offset to be removed.
          # FP_Zero array specifies starting and ending range of frames to zero values
          # Here we are assuming that if user specifies frames 1-10 to zero, but first 
          # frame of data in C3D is say 50, we will still use the first 10 frames: 50-60 
          if (Offset) {
            # If frame range is valid, remove "DC" noise. eg. Valid: FP_Zero = (1, 10)
            # If frame range is invalid, leave the signal alone. eg. FP_Zero = (10, 1)
            # (Vicon specifies this definition of Invalid frame ranges; C3d standard
            # requires FP_Zero = (0, 0) to prevent baseline removal)
            
            # Find offsets and Z-range
            # Compute range of ADCData to use as zero 
            ADCZeroStart <- (FP_Zero[1] - 1) * AnalogToVideoRate + 1
            ADCZeroEnd <- FP_Zero[2] * AnalogToVideoRate + 1
            # Dataframe for just zero range
            ZeroData <- FPData %>%
              slice(ADCZeroStart:ADCZeroEnd) %>%
              drop_na() 
            ZeroMean <- ZeroData %>%
              colMeans()
            
            # Remove any baseline offset from the force plate data
            # Having determined the mean and range of the noise, remove it
            # sweep() subtracts vector from each row of matrix, 
            # MARGIN = 2 indicates column wise application or STATS vector using 
            # the subtraction function '-'
            FPData <- sweep(FPData, MARGIN = 2, STATS =  ZeroMean, FUN = '-')
            
          } # If Offset
      
          # Type 1 Force Plates
          if (FP_Type[FP] == 1) {
            # Type 1 force plate output is Fx,Fy,Fz,COP_x,COP_y,Tz
            # Transformations from transducer outputs have occurred prior to C3D storage so no 
            # additional manipulations other than possibly flipping reaction signs and zeroing are needed
  
            # Standardize names
            names(FPData) <- c('Fx', 'Fy', 'Fz', 'CoPx', 'CoPy', 'Tz')
            
            # Force plate data is already transformed
            FPDataT <- FPData
            
          # Type 2 Force Plates
          } else if (FP_Type[FP] == 2) {
            # FP output data in form(Fx,Fy,Fz,Mx,My,Mz)
            # For a type 2 force plate, FP_Origin is the vector from the FP origin
            # to the geometric center of the FP surface in the FP coordinate system
            # Type 2 plates are made by AMTI, Bertec, ...
            
            # Standardize names
            names(FPData) <- c('Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz')
            # Add columns for standardized output
            FPData <- FPData %>%
              mutate(CoPx = -(My - FP_Origin[3, FP] * Fx)/Fz - FP_Origin[1, FP],
                     CoPy =  (Mx + FP_Origin[3, FP] * Fy)/Fz - FP_Origin[2, FP],
                     Tz = Mz + Fx*(CoPy + FP_Origin[2, FP]) - Fy*(CoPx + FP_Origin[1, FP])
              )
            
            # Select columns of standardized output
            FPDataT <- FPData %>%
              select(Fx, Fy, Fz, CoPx, CoPy, Tz)
  
          # Type 3 Force Plates
          } else if (FP_Type[FP] == 3) {
            # Kistler plate with 8 channels of output data
            # Standardize columns names
            names(FPData) <- c('Fx12', 'Fx34', 'Fy14', 'Fy23', 'Fz1', 'Fz2', 'Fz3', 'Fz4')
            # Add columns of standardized outputs
            FPData <- FPData %>%
              mutate(Fx =  Fx12 + Fx34,
                     Fy =  Fy14 + Fy23,
                     Fz =  Fz1 + Fz2 + Fz3 + Fz4,
                     Mx = FP_Origin[2, FP] * ( Fz1 + Fz2 - Fz3 - Fz4) + FP_Origin[3, FP] * Fy,
                     My = FP_Origin[1, FP] * (-Fz1 + Fz2 + Fz3 - Fz4) - FP_Origin[3, FP] * Fx,
                     Mz = FP_Origin[2, FP] * (Fx34 - Fx12) + FP_Origin[1, FP] * (Fy14 - Fy23),
                     CoPx = -My/Fz,
                     CoPy =  Mx/Fz,
                     Tz =  Mz - Fy * CoPx +  Fx * CoPy
                     )
            
            # Select columns of standardized output
            FPDataT <- FPData %>%
              select(Fx, Fy, Fz, CoPx, CoPy, Tz)
            
          # Type 4 Force Plates
          } else if (FP_Type[FP] == 4) {
            # Same as Type 2 force plate EXCEPT that parameter CAL_MATRIX is provided to
            # convert from volts to form(Fx,Fy,Fz,Mx,My,Mz)
                                                
            # For a type 4 force plate, FP_Origin is the vector from the FP origin
            # to the geometric center of the FP surface in the FP coordinate system
                    
            # The calibration matrix, CAL_MATRIX, is used to convert volts to
            # force and moment units. (ANALOG_SCALE parameter should only have taken
            # the output signals to volts.)
            # Stash this GetParameterData in this FP loop even though redundant 
            # parameter reads will happen if more than one FP with type 4, FP Type 4s will be rare
            FP_CM <- GetParameterData(Parameters, "FORCE_PLATFORM", "CAL_MATRIX")
            # Dimensions of FP_CalMatrix are (6,6,FP_Used)
            
            # Convert to matrix and multiply by calibration transpose (sensitivity matrix)
            FPmat <- (as.matrix(FPData) %*% t(FP_CM[,,FP]))
  
            Fx <- FPmat[,1]
            Fy <- FPmat[,2]
            Fz <- FPmat[,3]
            Mx <- FPmat[,4]
            My <- FPmat[,5]
            Mz <- FPmat[,6]
            
            CoPx <- -(My - FP_Origin[3, FP] * Fx) / Fz - FP_Origin[1, FP]
            CoPy <-  (Mx + FP_Origin[3, FP] * Fy) / Fz - FP_Origin[2, FP]
            Tz <- Mz + Fx * (CoPy + FP_Origin[2, FP]) - Fy * (CoPx + FP_Origin[1, FP])
      
            # Load current forces, center of pressure, and free torque into FPDataT array
            FPDataT <- tibble(Fx = Fx, Fy = Fy, Fz = Fz, 
                              CoPx = CoPx, CoPy = CoPy, Tz = Tz)
            
          } # If FP_Type[FP]
          
          # Regardless of DC offset requested, zero all data below calculated Fz threshold
          # Calculate the range of the vertical force signal--"peak-to-peak signal"
          FzSignalRange <- abs(max(FPDataT$Fz) - min(FPDataT$Fz))
          # Zero any data less than 0.5% of range
          # In case force plate has no loading and therefore no real signal range, 
          # set an artificial threshold of 1N
          FzCutOff = max(0.005 * FzSignalRange, 1)
          
          # Calculate mean vertical force to adjust force directions if flipped (Reaction Force if mean(Fz) < 0)
          # Check if the scaled FP output is a reaction force or an action force, set ReactionFactor accordingly
          ReactionFactor <- -sign(mean(FPDataT$Fz))
          
          # Augment FP data to include analog frames, video frames, and sub-frames
          # These were already made in ADCData so just copy
          # Also filter to First and Last Returned frames, flip components if reaction force was wrong sensed, and
          # apply zero threshold offsets
          FPDataT <- FPDataT %>%
            add_column(AnalogFrames = ADCData$AnalogFrames, 
                       VideoFrames = ADCData$VideoFrames, 
                       SubFrames = ADCData$SubFrames) %>%
            relocate(AnalogFrames, VideoFrames, SubFrames) %>%
            dplyr::filter(between(VideoFrames, FirstReturnFrame, LastReturnFrame)) %>% 
            mutate(
              Fx = Fx * ReactionFactor,
              Fy = Fy * ReactionFactor,
              Fz = Fz * ReactionFactor,
              Tz = Tz * ReactionFactor,
              across(Fx:Tz, ~ case_when(abs(Fz) < FzCutOff ~ 0,
                                        TRUE ~ .))
              )
          
          # If shorter format requested then filter to just video frames and remove SubFrames column
          if (ForcePlateDataFrames == 'video') {
            FPDataT <- FPDataT %>%
              dplyr::filter(SubFrames == 1) %>%
              select(-SubFrames)
          }
          
          # Store each set of FP data in one list
          if (FP == 1) {
            # Seemingly no names for forceplates stored in C3D so ...
            FPName[1] <- 'ForcePlate1'
            ForcePlateData <- list(FPDataT)
          } else {
            FPName[FP] <- paste0('ForcePlate', FP)
            ForcePlateData <- c(ForcePlateData, list(FPDataT))
          }
          
        } # Next FP  
          
        # Name the forceplates in the list after all are compiled
        names(ForcePlateData) <- FPName
        
        # Cleanup
        rm(FPData, FPDataT)
      
      } # if FP_USED
    
    }  # if (GetForcePlateData & AnalogData) 

  # Now can filter ADCData to requested frames
  if (AnalogData) {
    ADCData <- ADCData %>%
      dplyr::filter(between(VideoFrames, FirstReturnFrame, LastReturnFrame))
  }
  
  #### ---- Gait Cycle Events ----
  # Repackage parameter events into more user-friendly vectors
  # Check if any events exists before trying to read individual parameters
  GaitCycleEventsUsed <- GetParameterData(Parameters, "EVENT", "USED", Warn = FALSE)
  if (is.na(GaitCycleEventsUsed)) {GaitCycleEventsUsed <- 0}
  if (GaitCycleEventsUsed == 0) {
    # warning('ReadC3D:Events: No events found in EVENT group \n')
    # Report EVENT group doesn't exist in returned variable
    GaitCycleEvents <- 'No Gait Cyle Events from EVENT:USED'
  } else {
    # Calculate Gait Cycle Events
    # GaitCycleEventsIconIDs <- GetParameterData(Parameters, "EVENT", "ICON_IDS")
    # GaitCycleEventsGenericFlags <- GetParameterData(Parameters, "EVENT", "GENERIC_FLAGS")
    # Read other parameters into temp variables
    EventTimes <- GetParameterData(Parameters, "EVENT", "TIMES")
    EventDescriptions <- GetParameterData(Parameters, "EVENT", "DESCRIPTIONS")
    EventNames <- GetParameterData(Parameters, "EVENT", "LABELS")
    EventContexts <- GetParameterData(Parameters, "EVENT", "CONTEXTS")
    EventSubjects <- GetParameterData(Parameters, "EVENT", "SUBJECTS")
    # Check if more than 255 EVENTS and if so read and concatenate 
    if (GaitCycleEventsUsed > 255) {
      EventBlocks <- ceiling(GaitCycleEventsUsed/255)
      for (i in 2:EventBlocks) {
        # Label for next group
        strtimes <- paste0('TIMES', i)
        strdesc <- paste0('DESCRIPTIONS', i)
        strlbls <- paste0('LABELS', i)
        strcntx <- paste0('CONTEXTS', i)
        strsubj <- paste0('SUBJECTS', i)
        
        # Get data for next group then join to current list
        # Times (use cbind as multidimensional)
        Block <- GetParameterData(Parameters, "EVENT", strtimes)
        EventTimes <- cbind(EventTimes, Block)
        # Descriptions
        Block <- GetParameterData(Parameters, "EVENT", strdesc)
        EventDescriptions <- c(EventDescriptions, Block)
        # Labels
        Block <- GetParameterData(Parameters, "EVENT", strlbls)
        EventNames <- c(EventNames, Block)
        # Contexts
        Block <- GetParameterData(Parameters, "EVENT", strcntx)
        EventContexts <- c(EventContexts, Block)
        # Subjects
        Block <- GetParameterData(Parameters, "EVENT", strsubj)
        EventSubjects <- c(EventSubjects, Block)
      }
    }
    
    # Fix times and compress character labels into string arrays
    # Each time is stored as minutes in one field, second in next field
    # Need to assemble the event times by adding two fields together;
    EventTimes <- 60* as.double(EventTimes[1,]) + EventTimes[2,]

    # Create vectors for Event Times
    FootStrikeTime_R <- numeric()
    FootStrikeTime_L <- numeric()
    FootOffTime_R <- numeric()
    FootOffTime_L <- numeric()
    NumFootStrike_L <- NumFootOff_L <- 0
    NumFootStrike_R <- NumFootOff_R <- 0

    # Sort through all events and distribute into vectors  
    for (i in 1:GaitCycleEventsUsed) {
      if (EventContexts[i] == "Right") {
        if (EventNames[i] == "Foot Strike") {
          NumFootStrike_R <- NumFootStrike_R + 1
          FootStrikeTime_R[NumFootStrike_R] <- EventTimes[i]
        } else if (EventNames[i] == "Foot Off") {
          NumFootOff_R <- NumFootOff_R + 1
          FootOffTime_R[NumFootOff_R] <- EventTimes[i]
        }
      } else if (EventContexts[i] == "Left") {
        if (EventNames[i] == "Foot Strike") {
          NumFootStrike_L <- NumFootStrike_L + 1
          FootStrikeTime_L[NumFootStrike_L] <- EventTimes[i]
        } else if (EventNames[i] == "Foot Off") {
          NumFootOff_L <- NumFootOff_L + 1
          FootOffTime_L[NumFootOff_L] <- EventTimes[i]
        }
      }
    }
  
    # Sort these so that they are sequential
    FootStrikeTime_L <- sort(FootStrikeTime_L)
    FootStrikeTime_R <- sort(FootStrikeTime_R)
    FootOffTime_L <- sort(FootOffTime_L)
    FootOffTime_R <- sort(FootOffTime_R)
  
    # Also store Event Frames
    # Need to add one frame interval as time = 0 is frame = 1
    FootStrikeFrame_L <- round(FrameRate * FootStrikeTime_L) + 1
    FootStrikeFrame_R <- round(FrameRate * FootStrikeTime_R) + 1
    FootOffFrame_L <- round(FrameRate * FootOffTime_L) + 1
    FootOffFrame_R <- round(FrameRate * FootOffTime_R) + 1
  
    # Store in list
    GaitCycleEvents <- list(
      Labels = EventNames,
      Descriptions = EventDescriptions,
      Contexts = EventContexts,
      Times = EventTimes,
      Subjects = EventSubjects,
      FootStrikeTime_L = FootStrikeTime_L,
      FootStrikeTime_R = FootStrikeTime_R,
      FootOffTime_L = FootOffTime_L,
      FootOffTime_R = FootOffTime_R,
      FootStrikeFrame_L = FootStrikeFrame_L,
      FootStrikeFrame_R = FootStrikeFrame_R,
      FootOffFrame_L = FootOffFrame_L,
      FootOffFrame_R = FootOffFrame_R
    )
  } # GaitCycleEventsUsed
  

  #### ---- Create C3D Output Variables ----
  # Follow general format and variables from MLS C3D reader
  # Header Parameters
  DataFormat <- case_when(Real ~ 'Real', TRUE ~ 'Integer')
  Header <- list(
    'C3D_File_Name' = C3DFileName,
    'First_Parameter_Record' = FirstParameterRecordNumber,
    'Number_of_Trajectories' = NumberOfMarkers,
    'Analog_Channels' = NumberOfAnalogChannels,
    'First_Frame' = FirstC3DFrameNumber,
    'Last_Frame' = LastC3DFrameNumber,
    'Video_Sampling_Rate' = FrameRate,
    'Analog_Sampling_Rate' = AnalogSampleRate,
    'Scale_Factor' = VideoScaleFactor,
    'Start_Record_Num' = FirstDataRecordNumber,
    'Max_Interpolation_Gap' =  MaxInterpolationGap,
    'C3D_File_Format' = ProcessorType,
    'C3D_Data_Format' = DataFormat,
    'Number_of_Events' = NumberOfEvents)
  
  # Events
  if (NumberOfEvents > 0) {
  Events <- tibble(
    Labels = HeaderEventLabels,
    Times = HeaderEventTimes,
    Switches = HeaderEventSwitches)
  } else {
    Events <- 'No Events in header'
  }
  
  #### ---- Assemble C3D output list ----
  # Again follow the general order of the MLS viewer display
  # but return Parameters in a single data frame
  
  # All C3D should have these elements
  C3D <- list(
    'Header' = Header,
    'Events' = Events,
    'Parameters' = Parameters,
    'GaitCycleEvents' = GaitCycleEvents
  )
  
  # Add optional elements
  if (GetMarkerData) {C3D$VideoData <- MarkerData}
  if (GetAnalogData) {C3D$AnalogData <- ADCData}
  if (GetForcePlateData) {C3D$ForcePlateData <- ForcePlateData}
            
  close(con)
  return(C3D)

} # End Function

# Test function call ...
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/TD038AS_16.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/TD038AS_16.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/TD039A_A18.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/Workstation 2007.c3d' 
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/TYPE-4.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/666090 21.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/Nexus 2008.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/Workstation 1997.C3D'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/180040 7.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/507168 24.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/TYPE-3.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/C3DTestSuite/Eb015pr.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/C3DPointFrames/18124framesf.c3d' 
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/C3DPointFrames/36220framesi.c3d' 
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/C3DPointFrames/72610framesf.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/C3DCompanies/arthuman-sample.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/147068 53.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/C3DCompanies/Vicon pyCGM2 lower limb CGM24 Walking01.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/648385 22.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/999978 15.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/999977 18.c3d'
# C3DFN <- 'C:/Users/bmacwilliams/Box/Projects/Read C3D/Sample C3Ds/PyOut16_13.c3d'
# 
# C3D <- ReadC3D(C3DFileName = C3DFN)

# p <- C3D$Parameters
# m <- C3D$VideoData
# a <- C3D$AnalogData
# fp1 <- C3D$ForcePlateData$ForcePlate1

