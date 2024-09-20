# This function builds a tabbed interface for a set of videos with chapters
# The function takes a list of video files and a list of marker files (chapter info) as input
# The function generates an HTML file with the tabbed interface
# The function also generates a JavaScript function to set chapters in the videos
# The resulting HTML is written to a temporary file called "video_tab_code.txt"
# This file is then included in the R Markdown document to display the tabbed interface

build_video_tabs <- function(vidlist, vidfile){

  # Define the number of videos and their names -----
  N <- length(vidlist)
  
  # Define the tab names -----
  tabnames <- str_replace(vidlist, ".mp4", "")
  video_files <- vidlist
  marker_files <- str_replace(vidfile, "mp4", "mkr")
  
  # Initialize HTML content -----
  html_content <- "<style>
/* Button styling */
button {
  font-size: 11pt;            /* Set font size to 11 points */
  border-radius: 8px;        /* Rounded edges */
  margin: 3px;                /* 5-pixel margin */
  padding: 5px 7px;          /* Add some padding inside the button */
  cursor: pointer;            /* Cursor changes to pointer on hover */
  background-color: #e5f2ff;  /* Background color */
  color: black;               /* Text color */
  border: none;               /* Remove border */
}

button:hover {
  background-color: #0056b3;  /* Darker background on hover */
}
</style>
<div>
  <ul class=\"nav nav-tabs\" role=\"tablist\">"

# Add tabs for each video -----
for (i in 1:N) {
  html_content <- paste0(html_content, sprintf(
    "<li class=\"nav-item\">
      <a class=\"nav-link%s\" id=\"tab%d-tab\" data-bs-toggle=\"tab\" href=\"#tab%d\" role=\"tab\" aria-controls=\"tab%d\" aria-selected=\"%s\">%s</a>
    </li>",
    ifelse(i == 1, " active", ""),
    i, i, i,
    ifelse(i == 1, "true", "false"),
    tabnames[i]
  ))
}

html_content <- paste0(html_content, "</ul>
  <div class=\"tab-content\">")

# Function to parse .mkr file using regular expressions
parse_mkr <- function(file) {
  content <- readLines(file)
  content <- paste(content, collapse = "\n")
  
  # Regular expression to extract chapter data
  pattern <- "<MARKER LABEL=\"([^\"]+)\" START=\"([^\"]+)\" END=\"([^\"]+)\""
  matches <- str_match_all(content, pattern)[[1]]
  
  data.frame(
    label = matches[, 2],
    start = as.numeric(matches[, 3]),
    end = as.numeric(matches[, 4])
  )
}

# Add content for each tab -----
for (i in 1:N) {
  video_file <- video_files[i]
  marker_file <- marker_files[i]
  chapters <- parse_mkr(marker_file)
  
  html_content <- paste0(html_content, sprintf(
    "<!-- Tab %d -->
    <div class=\"tab-pane fade%s\" id=\"tab%d\" role=\"tabpanel\" aria-labelledby=\"tab%d-tab\">
      <video id=\"video%d\" controls width=\"100%%\">
        <source src=\"%s\" type=\"video/mp4\">
        Your browser does not support the video tag.
      </video>
      <div style=\"margin-top: 10px;\">",
    i, ifelse(i == 1, " show active", ""),
    i, i, i, video_file
  ))
  
  for (j in 1:nrow(chapters)) {
    html_content <- paste0(html_content, sprintf(
      "<button onclick=\"setChapter('video%d', %f, %f)\">%s</button>",
      i,
      chapters$start[j],
      chapters$end[j],
      chapters$label[j]
    ))
  }
  
  html_content <- paste0(html_content, "</div></div>")
}

html_content <- paste0(html_content, "</div></div>

<script>
function setChapter(videoId, startTime, endTime) {
  var video = document.getElementById(videoId);
  
  // Pause the video and reset current time
  video.pause();
  video.currentTime = startTime;
  
  // Ensure the video has time to update the currentTime before playing
  setTimeout(function() {
    video.play();

    // Function to stop at the end time
    var stopChapter = function() {
      if (video.currentTime >= endTime) {
        video.pause();
        video.removeEventListener('timeupdate', stopChapter);
      }
    };
    
    // Add the event listener for stopping the chapter
    video.addEventListener('timeupdate', stopChapter);
  }, 50); // Adjust this timeout value if necessary
}
</script>")

# Write the HTML content to a file -----
writeLines(html_content, "video_tab_code.txt")
}