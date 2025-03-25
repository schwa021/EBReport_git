# Packages -----
options(java.parameters = "-Xmx32g")
library(bartMachine, verbose = F)
library(tidyverse, verbose = F)
library(lubridate, verbose = F)
library(patchwork, verbose = F)
library(ggtext, verbose = F)
library(RColorBrewer, verbose = F)
library(glue, verbose = F)
library(gt, verbose = F)
library(gtsummary, verbose = F)
library(ggpmisc, verbose = F)
library(MatchIt, verbose = F)
library(ggh4x, verbose = F)
library(extraDistr, verbose = F)
library(iml, verbose = F)
library(Hmisc, verbose = F)
library(colorspace, verbose = F)
library(MatchIt, verbose = F)
library(DBI)

# User Functions -----
margin <- ggplot2::margin
summarize <- dplyr::summarize
select <- dplyr::select
formals(table)$useNA <- "ifany"

source("SCRIPTS/read_and_organize.R")
source("SCRIPTS/addnoise.R")
source("SCRIPTS/get_TD_NEW.R")
source("SCRIPTS/build_propensity_mod.R")
source("SCRIPTS/get_prop_model_vars.R")
source("SCRIPTS/prop_shap_plot.R")
source("SCRIPTS/prop_detail_plot.R")
source("SCRIPTS/pmatch_plot.R")
source("SCRIPTS/get_outcome_vars.R")
source("SCRIPTS/pmatch_data.R")
source("SCRIPTS/outcome_plots.R")
source("SCRIPTS/p_match_qual.R")
source("SCRIPTS/chkmatchlims.R")
source("SCRIPTS/test_pred_perf.R")
source("SCRIPTS/noisify_side.R")
source("SCRIPTS/getnoisevars.R")
source("SCRIPTS/prop_shap_table.R")
source("SCRIPTS/fmt_shap.R")
source("SCRIPTS/fmt_shapLR.R")
source("SCRIPTS/ptableLR.R")
source("SCRIPTS/getq.R")
source("SCRIPTS/fmt_chk.R")
source("SCRIPTS/get_varlabs.R")
source("SCRIPTS/plot_development.R")
source("SCRIPTS/get_meanscores.R")
source("SCRIPTS/pred_outcome_by_bart.R")
source("SCRIPTS/outcome_plots_BART.R")
source("SCRIPTS/get_pred_model_vars.R")
source("SCRIPTS/get_O2_Z.R")
source("SCRIPTS/get_outcome_thresh.R")
source("SCRIPTS/read_c3d_one.R")
source("SCRIPTS/make_FD_one.R")
source("SCRIPTS/get_file_list.R")
source("SCRIPTS/build_ptsumm.R")
source("SCRIPTS/get_patient_info.R")
source("SCRIPTS/build_goaltbl.R")
source("SCRIPTS/get_goal.R")
source("SCRIPTS/build_importanttbl.R")
source("SCRIPTS/compress_video.R")
source("SCRIPTS/build_video_tabs.R")
source("SCRIPTS/get_prop.R")
source("SCRIPTS/build_pred_outcome_BART.R")
source("SCRIPTS/build_checklist.R")
source("SCRIPTS/propgraph.R")
source("SCRIPTS/petbl.R")
source("SCRIPTS/orthoseverity.R")
source("SCRIPTS/build_orthotbl.R")
source("SCRIPTS/build_spatbl.R")
source("SCRIPTS/build_strseltbl.R")
source("SCRIPTS/build_foottbl.R")
source("SCRIPTS/plot_patient_knm.R")
source("SCRIPTS/plot_patient_knm_history.R")
source("SCRIPTS/get_LLD.R")
source("SCRIPTS/plot_patient_knmafo.R")
source("SCRIPTS/remove_missing_outcome.R")
source("SCRIPTS/compute_FAQT.R")

# Theme for ggplot2 -----
theme_mhs <- theme_minimal(base_size = 7) 
theme_mhs <- function(bs){
  theme_minimal(base_size = bs) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = .1, color = "grey85")
    )
}
theme_set(theme_mhs(7))

# List of surgeries -----
surglist <- c("Femoral_Derotation_Osteotomy", "Tibial_Derotation_Osteotomy", "Psoas_Release", "Hams_Lengthening", "Adductor_Release", "Gastroc_Soleus_Lengthening", "DFEO_Patellar_Advance", "Patellar_Advance", "Foot_and_Ankle_Bone", "Foot_and_Ankle_Soft_Tissue","Neural_Rhizotomy", "Rectus_Transfer")

# For trimming white space around figures -----
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)

# For controlling evaluation -----
usevideo <- params$usevideo

