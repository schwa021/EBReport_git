
# library(tidyverse)
# rm(list = ls())
# close(con)
# 
# # setwd("C:/Users/bmacwilliams/Box/Projects/Read C3D")
# 
# source("C:/Users/bmacwilliams/Box/Projects/Read C3D/ConversionFunctions.R")

# Function only needs to pass "con"

# #setwd("C:/Users/bamac/Box/Projects/Read C3D")
# ### SLC
# con = file("Sample C3Ds/TD038AS_16.c3d", "rb") # Works
# con = file("Sample C3Ds/Nexus 2008.c3d", "rb") # Works
# con = file("Sample C3Ds/Workstation 2007.c3d", "rb") # Works
# con = file("Sample C3Ds/Workstation 1997.c3d", "rb") # DEC Works
# ### Gillette
# con = file("Sample C3Ds/496202 14.c3d", "rb") # PC Works
# con = file("Sample C3Ds/507168 24.c3d", "rb") # PC, big one, Works
# con = file("Sample C3Ds/635804 14.c3d", "rb") # PC, Works
# con = file("Sample C3Ds/655294 31.c3d", "rb") # PC, Works
# con = file("Sample C3Ds/664485 15.c3d", "rb") # PC, Works
# con = file("Sample C3Ds/900045 32.c3d", "rb") # PC, Works
# con = file('Sample C3Ds/666090 21.c3d', "rb")
# con = file('Sample C3Ds/C3DCompatibility/TESTBPI.c3d', "rb")


#### ---- Get Parameter Data ---- ####
# Return Parameter Data (unlisted) given group and parameter name to search
# Removes trailing white space in character vectors
# Returns NA if not found
GetParameterData <- function(P, GName, PName, Warn = TRUE) {
  # Test data ...
  # P <- Parameters
  # GName <- 'POINT'
  # PName <- 'LONG_FRAMES'
  # PName <- 'FRAMES'
  # browser()
  
  P <- P %>% dplyr::filter(GroupName == GName)
  PR <- which(P$ParameterName == PName)
  if (is_empty(PR)) {
    if (Warn){
      warning('ReadC3DParamters:GetParameterData: Parameter ', GName, ':', PName,' not found \n')
    }
    pdata <- NA
  } else {
  
    dims <- unlist(P$Dimensions[PR])
    pdata <- unlist(P$Data[PR])
    # If a matrix or 3D of numbers reformat
    
    # if string
    if (P$DataType[PR] == -1) {
      if (P$NumDimensions[PR] > 1) {
        # Reformat strings using white space
        # Has to be a better way ...
        t <- as.character(dims[2])
        for (i in 1: dims[2]){
          t[i] <- trimws(substr(pdata, (i-1)*dims[1] + 1, i*dims[1]))
        }
        pdata <- t
        #pdata <- trimws(str_split_fixed(pdata, ' +', n=dims[2]), 'right')
      } else {
        pdata <- trimws(pdata, 'right')
      }
    # Special conditions
    # This was a brilliant idea but it screws up FP:Zero
    #} else if (P$DataType[PR] == 2 & P$NumDimensions[PR] == 1 & dims[1] == 1) {
    #  # If there is a 1D integer (not a scalar) then convert two integer bytes to int()
    #  pdata <- as.integer((pdata[2] * 256) + pdata[1])
    # if numeric  
    } else {
      if (P$NumDimensions[PR] > 1) {
        # Re-dimension data
        pdata <- array(pdata, dim = dims)
        }
    }
  }
  return(pdata)
}

# Float from 4 bytes of DEC format ('Single')
# Pass the 4 bytes in the DEC order (bLo1, bHi1, bLo2, bHi2), float returned
DECBytes2Single <-function(dec_bytes) {
  # swap words, keep lo hi order 
  pc_bytes <- c(dec_bytes[3], dec_bytes[4], dec_bytes[1], dec_bytes[2])
  # Use readBin to do the byte manipulation to create floating point type
  # Divide by 4, same as subtracting 1 from last byte
  return(readBin(pc_bytes, numeric(), n=1, size=4)/4)
}


# ReadC3DParameters evaluates parameters contained in the opened C3D file 
# passed in "con" from readBin(). This function works on both PC and DEC 
# processor types. Parameters are returned in a data frame with data as type 
# list and dimensions given also in a list.
# 
# Parameters <- tibble(
#   Group = integer(),            # Parameter Group Number (1:NG)
#   Parameter = integer(),        # Parameter Number for Group (1:NP)
#   GroupName = character(),      # Group Name
#   ParameterName = character(),  # Parameter Name
#   NumDimensions = integer(),    # Parameter Number of Dimensions (0 = scalar)
#   Dimensions = list(),          # Number of values for each dimension
#   DataType = integer(),         # Parameter data type (-1 = Char, 1 = Byte, 2 = Int, 4 = Float)
#   Data= list()                  # Parameter data
# )

ReadC3DParameters <- function(con) {

  # con passed with pointer not at start so reset
  cbo <- seek(con, where = 0, origin = 'start')
  # Determine starting byte for parameter records
  FirstParameterRecordNumber <- readBin(con, integer(), n = 1, size = 1)
  FirstParameterByteNumber <- (FirstParameterRecordNumber - 1) * 512

  # Parameter typically block starts at byte 512
  # ignore first two bytes (512, 513)
  cbo <- seek(con, where = FirstParameterByteNumber + 2, origin = 'start')
  # 3rd byte (byte 514) is the number of 512 byte blocks that the parameter section uses 
  Parameter_Blocks <- readBin(con, integer(), n = 1, size = 1, signed = FALSE)
  
  # Apparently some files may not record the number of parameter blocks, so calculate
  # by using the data start record
  if (Parameter_Blocks == 0) {
    # Store current position
    cbo <- seek(con, where = NA)
    # Skip to byte 17 (Word 9 = bytes 16, 17)
    temp_cbo <- seek(con, where = 16, origin = 'start')
    # Word 9: Starting record number for 3D point and analog data, unsigned 2 byte integer
    FirstDataRecordNumber <- readBin(con, integer(), n = 1, size = 2, signed = FALSE)
    Parameter_Blocks <- FirstDataRecordNumber - FirstParameterRecordNumber
    # Set pointer back to previous
    cbo <- seek(con, where = cbo, origin = 'start')
  }
  
  # Could pass processor type, but just read it again 
  # 4th byte (515) is the Processor type with:
  # 85 = Intel/PC
  # 86 = DEC
  # 87 = SGI/MIPS
  # Already been screened so type is either PC or DEC
  # Already here in file 
  #cbo <- seek(con, where = FirstParameterByteNumber + 3, origin = 'start')
  byte <- readBin(con, integer(), n = 1, size = 1)
  Processor_Type <- case_when(byte == 84 ~ 'PC',
                              byte == 85 ~ 'DEC')
  
  # Knowing how many blocks, read the entire parameter section into a byte vector
  # Go back and start with the first block byte
  cbo <- seek(con, where = FirstParameterByteNumber, origin = 'start')
  NBytes <- 512 * Parameter_Blocks
  pb <- readBin(con, raw(), n = NBytes)
  
  # ............................................................................
  # From the C3D User Manual (C3D.org):
  
  # The parameters are stored starting at byte 5 of the parameter section. 
  # The parameters are stored in random order providing flexibility when 
  # parameters need to be edited, deleted or added. 
  # Each parameter has a data type, optional dimensions, a name, a description, 
  # and belongs to a group. Each group defined in the parameter section also 
  # has a name and a description.
  
  # Read the GROUP parameters
  # Byte  1:                       # of characters n in group name
  # Byte  2:                       Group ID number (-1 to -127, always negative)
  # Bytes 3:3 + n - 1:             Group name character(1: n = #characters)
  # Byte  3 + n:                   Signed integer offset in bytes pointing to start of next group/parameter
  # Byte  3 + n + 2:               # characters m in the Group description
  # Byte  3 + n + 3:               Group description
  
  # Read the PARAMETERS
  # Byte 1:                       # of characters n in parameter name
  # Byte 2:                       Group ID number (1 to 127, always positive)
  # Byte 3:3 + n:                 Parameter name character(1: n = #characters)
  # Byte 3 + n:                   Signed integer offset in bytes pointing to start of next group/parameter
  # Byte 3 + n + 2:               Length of each data element
  #                                 -1 for character data
  #                                  1 for byte data
  #                                  2 for integer data
  #                                  4 for floating point data
  # Byte 3 + n + 3:               Number of dimensions d of the parameter, 0 if scalar
  # Byte 3 + n + 4:               Parameter dimensions, length d bytes
  # Byte 3 + n + 4 + d:           Parameter data length t bytes
  # Byte 3 + n + 4 + d + t:       # of characters m in parameter description
  # Byte 3 + n + 4 + d + t + 1:   Parameter description length m
  
  # There is no count stored for the number of parameters in each group and all 
  # group and parameter records can appear in any order. This means that it is 
  # permissible for a parameter to appear in the parameter section before the
  # group information and software accessing a C3D file should be prepared to deal 
  # with this situation.
  
  # Parameters are connected to groups by use of the group ID number. Group 
  # records have unique ID numbers within the file, which are stored as a negative 
  # value in byte 2. All parameters belonging to a group will store the same ID as 
  # a positive value, also in byte 2.
  
  # Always use the absolute value of the first byte to determine the length of the 
  # parameter name. This value may be set to a negative value to indicate that the 
  # data item has been marked as locked and should not be edited.
  # ............................................................................
  
  # Number of Groups and Parameters are not known, so have to search through all blocks
  
  # Initialize temporary arrays to max as dimensions are unknown
  GroupNumber <- 1
  GroupName <- character(128)
  ParamNumber <- integer(128)
  DimSize <- integer(7)
  
  # Main tibble for storage
  Parameters <- tibble(
    Group = integer(), 
    Parameter = integer(), 
    GroupName = character(), 
    ParameterName = character(), 
    NumDimensions = integer(), 
    Dimensions = list(), 
    DataType = integer(),
    Data = list()
  )
  
  # New method, use readBin() or rawToChar() with pb[] instead of conversion functions
  # potentially faster, For unsigned integers from a single byte use as.integer() 
  
  # byte = Counter for parameter block pb[]
  byte <- 5
  # PR is row counter for Parameter tibble
  PR <- 0
  
  while (GroupNumber != 0) {
    # cat('Starting Byte: ', byte, '\n')
    # Byte 1: # characters in GROUP or PARAMETER name
    # This is actually signed because a negative number indicates "LOCKED"
    # as in do not change (value from 1-127)
    NameLength <- abs(readBin(pb[byte], integer(), n = 1, size = 1, signed = TRUE))
    # If the length is 0 stop searching
    if (NameLength == 0) {
      GroupNumber <- 0
      GN <- 0
    } else {
      # Byte 2: # Group ID number, also signed 
      # Negative if group, positive if parameter
      byte <- byte + 1
      # Group Number for indexing
      GN <- readBin(pb[byte], integer(), n = 1, size = 1, signed = TRUE)
    }
    # Is this a group or a parameter?
    if (GN < 0) {
      # GROUP
      # Read the parameter name of length NameLength, store in array for use in parameter tibble
      byte <- byte + 1
      GroupName[abs(GN)] <-rawToChar(pb[(byte):(byte + NameLength - 1)])
      # cat('Group Number: ', GN, ' Group Name: ', GroupName[abs(GN)], '\n')
      # 2 Bytes following name are the byte number of next group
      byte <- byte + NameLength
      Offset2Next <- readBin(c(pb[byte],pb[byte + 1]), integer(), n = 1, size = 2)
      if (Offset2Next == 0){
        # The last parameter in the parameter section always has a pointer value 
        # of 0x0000h to indicate that there are no more parameters
        GroupNumber <- 0
        break
      }
      NextParamByte <- byte +  Offset2Next
      # Next byte is # characters in GROUP description, unsigned integer
      byte <- byte + 1
      DescLength <- as.integer(pb[byte])
      # Do we care about these or just skip? I vote for skip ...
      # GroupDesc[GroupNumber, pn] <- rawToChar(pb[(byte):(byte + DescLength - 1)])
      
      # Advance to next group/parameter by number of bytes + offset
      byte <- NextParamByte
      
    } else {
      # PARAMETER
      # Read the parameter name of length NameLength
      byte <- byte + 1 
      ParamName <- rawToChar(pb[(byte):(byte + NameLength - 1)])
      # Keep count of the number of parameters in each group, use PN as counter 
      PN <- ParamNumber[GN] + 1
      ParamNumber[GN] <- PN
      # cat('Group Number: ', GN, ' Parameter Number: ', PN, ' Parameter Name: ', ParamName, '\n')
      
      # 2 Bytes following name are the byte number of next group
      byte <- byte + NameLength 
      Offset2Next <- readBin(c(pb[byte],pb[byte + 1]), integer(), n = 1, size = 2)
      # This Offset points to start of next parameter FROM THIS BytE
      NextParamByte <- byte +  Offset2Next
      if (Offset2Next == 0) {
        # The last parameter in the parameter section always has a pointer value 
        # of 0x0000h to indicate that there are no more parameters
        GroupNumber <- 0
        break
      }
  
      # Update the Parameters row after validating parameter exists
      PR <- PR + 1
      Parameters <- Parameters %>% add_row()
      Parameters$Group[PR] <- GN
      Parameters$Parameter[PR] <- PN
      Parameters$GroupName[PR] <- GroupName[GN]
      Parameters$ParameterName[PR] <- ParamName
      
      # Type/length of data element
      byte <- byte + 2
      PT <- readBin(pb[byte], integer(), n = 1, size = 1, signed = TRUE)
      if (!PT %in% c(-1, 1, 2, 4)) {
        stop('ReadC3DParameters:Main: Type not read at parameter byte ', byte, '\n')
      }  else { 
        Parameters$DataType[PR] <- PT
      }

      # Number of dimensions in parameter
      byte <- byte + 1 
      numdim <- readBin(pb[byte], integer(), n = 1, size = 1, signed = TRUE)
      Parameters$NumDimensions[PR] <- numdim
      # Parameter dimensions, length is given by numdim
      # First get the number of values by the dimensions
      # One dimensional
      # Data Length byte gives value in bytes (variables * byte size), not # variables
      byte <- byte + 1
      # Keep count of number of values stored in each parameter
      NumVal <- 0
      # If scalar don't read next byte
      if (numdim == 0) {
        Parameters$Dimensions[PR] <- 0
        NumVal <- 1
        DataLength <- abs(Parameters$DataType[PR])
      }
      # If vector
      else if (numdim == 1) {
        Parameters$Dimensions[PR] <- 1
        NumVal <- as.integer(pb[byte])
        DataLength <-  NumVal * abs(Parameters$DataType[PR])
        byte <- byte + 1
      # If 3D or more
      } else {
        NumVal <- 1
        DimSize <- integer(numdim)
        for(dim in 1:numdim){
          DimSize[dim] <- as.integer(pb[byte])
          NumVal <- NumVal * DimSize[dim]
          byte <- byte + 1
        }
        DataLength <- NumVal * abs(Parameters$DataType[PR])
        Parameters$Dimensions[PR] <- list(DimSize)
      }

      # Read all parameter data into a vector regardless of dimensions, then re-dimension later
      EndByte <- byte + DataLength - 1 
      # If characters
      if (Parameters$DataType[PR] == -1) {
        #browser()
        # New approach, store strings exactly as in C3D, deal with white space and dimensions as needed
        Parameters$Data[PR] <- list(rawToChar(pb[byte:EndByte]))
      # If bytes
      } else if (Parameters$DataType[PR] == 1) {
        Parameters$Data[PR] <- list(readBin(pb[byte:EndByte], what = raw(), n = DataLength))
      # If integers 
      } else if (Parameters$DataType[PR] == 2) {
        Parameters$Data[PR] <- list(readBin(pb[byte:EndByte], what = integer(), n = DataLength, size = 2))
      # If float/real
      } else if (Parameters$DataType[PR] == 4) {
        if (Processor_Type == 'DEC') {
          # DEC conversion, need to take one at time dimension temp variable
          paramdat <- numeric(NumVal)
          for(i in 1:NumVal) {
             paramdat[i] <- DECBytes2Single(pb[byte:(byte + 3)])
             byte <- byte + 4
          }
          Parameters$Data[PR] <- list(paramdat)
          # PC Real
        } else {
          Parameters$Data[PR] <- list(readBin(pb[byte:EndByte], what = numeric(), n = NumVal, size = 4))
        }
      # Else we're in trouble ...
      } else {
        stop('ReadC3DParameters:Main: Parameter type not recognized \n')
      }
      # Advance to next by number of bytes + offset
      byte <- EndByte + 1
      # Number of characters in the parameter description, unsigned
      DescLength <- as.integer(pb[byte])
      # Do we care about these or just skip? I vote for skip ...
      # ParamDesc <- rawToChar(pb[(byte):(byte + DescLength - 1)])
      
      # Advance to next group/parameter by number of bytes + offset
      byte <- NextParamByte 
    } # Read next parameter
  } # End while()
  
  # Clean up
  rm(pb, GroupName, ParamNumber)
  Parameters <- Parameters %>%
    arrange(Group, Parameter)
  
  return(Parameters)
  
}
