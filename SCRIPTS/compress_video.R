
# This function compresses videos using ffmpeg -----
compress_video <- function(temp, vidlist, vidlabs){
  # Remove old videos.....
  unlink(temp)
  # Compress all videos.....
  vidname <- vector()
  for (kk in 1:length(vidlist)) {
    curr.vid <- vidfile[kk]
    vidname[kk] <- glue("{vidlabs[[kk]]}.mp4")
    ffmpeg_command <-
      paste("ffmpeg -i", curr.vid, "-c:v libx264 -crf 20", vidname[kk])
    system(ffmpeg_command)
  }  
}
