
#setwd("H:/nhs_ptd_power_bi")

# Import all required NHS R Plot the Dots scripts
source('./r_files/flatten_HTML.r')
source("./R/ptd_spc.R")
source("./R/ptd_rebase.R")
source("./R/ptd_target.R")
source("./R/ptd_spc_options.R")
source("./R/ptd_validate_spc_options.R")
source("./R/ptd_validate_plot_options.R")
source("./R/to_datetime.R")
source("./R/ZZZ.R")
source("./R/ptd_add_rebase_column.R")
source("./R/ptd_calculate_point_type.R")
source("./R/ptd_add_short_group_warnings.R")
source("./R/ptd_add_target_column.R")
source("./R/ptd_calculate_assurance_type.R")
source("./R/ptd_spc_standard.R")

############### Library Declarations ###############
libraryRequireInstall("plotly")
libraryRequireInstall("dplyr")
libraryRequireInstall("DT")
####################################################

# If developing for use on Microsoft PowerBI service, you need to be using 
# R 3.4.4 and some rather old versions of packages specified here:
# https://learn.microsoft.com/en-us/power-bi/connect-data/service-r-packages-support
# The difficulty with such old versions of R and packages as of June 2023 is that 
# many of the options for installing older versions of packages in themselves
# do not work. devtools fails to install due to a range of dependencies. 
# Various versions of the groundhog package also fail to work. 
# The following approach of specifying a range of key packages and installing 
# specified versions directly from CRAN seems to be the most robust approach. 
# Order of packages is very important as some are dependencies of others and the 
# dependencies are not installed automatically. 
# Rtools will also need to be installed - version 34 is the one that works with R3.4.4
# https://cran.r-project.org/bin/windows/Rtools/history.html

packageurls <- c(
  "http://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_1.0.1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/ellipsis/ellipsis_0.3.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/backports/backports_1.1.2.tar.gz",
  "https://cran.r-project.org/src/contrib/zeallot_0.1.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/vctrs/vctrs_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/pillar/pillar_1.4.2.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/purrr/purrr_0.3.3.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/assertthat/assertthat_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/tibble/tibble_2.1.1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/tidyselect/tidyselect_0.2.5.tar.gz",
  "https://cran.r-project.org/src/contrib/plogr_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/BH/BH_1.66.0-1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_0.8.3.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/lifecycle/lifecycle_0.1.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/tidyr/tidyr_1.0.0.tar.gz",
  
  "http://cran.r-project.org/src/contrib/Archive/stringr/stringr_1.3.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/lubridate/lubridate_1.7.2.tar.gz",

  "http://cran.r-project.org/src/contrib/Archive/htmltools/htmltools_0.3.6.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/htmlwidgets/htmlwidgets_1.3.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/lazyeval/lazyeval_0.2.1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/httpuv/httpuv_1.3.6.2.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.0.5.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/colorspace/colorspace_1.3-2.tar.gz",
  "https://cran.r-project.org/src/contrib/munsell_0.5.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/scales/scales_1.0.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/gtable/gtable_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/testthat/testthat_2.0.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/isoband/isoband_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.3.3.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/crosstalk/crosstalk_1.0.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/DT/DT_0.4.tar.gz",
  
  "http://cran.r-project.org/src/contrib/Archive/data.table/data.table_1.12.6.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/later/later_1.0.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/promises/promises_1.1.0.tar.gz",
    
    "http://cran.r-project.org/src/contrib/Archive/openssl/openssl_1.0.1.tar.gz",
    "http://cran.r-project.org/src/contrib/Archive/httr/httr_1.3.1.tar.gz",
    "http://cran.r-project.org/src/contrib/Archive/hexbin/hexbin_1.27.2.tar.gz",
    "http://cran.r-project.org/src/contrib/Archive/plotly/plotly_4.9.2.2.tar.gz"
)

for (packageurl in packageurls) {
  install.packages(packageurl, repos=NULL, type="source")
}

################### Actual code ####################
# 'Values' is the input received from PowerBI
# Note that it seems a bit funny about the format dates are passed through in
# TODO: Make date parsing more flexible - at the moment it will *only* work with yyyy-mm-dd 
# (or possibly a very comprehensive mm-dd-yyyy, and that might depend on locale settings...) 

# If testing, a sample dataset can be loaded from here (changing the path below the 
# repository if necessary)
Values <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_decrease_good_trending_lower_no_target.csv") %>%
          mutate(date = lubridate::dmy(date))

dataset <- Values %>% 
    mutate(date = as.Date(date))

# Extract option for baseline length
# Slightly odd layout required to get true null as return value - this pattern is reused throughout as seems very reliable
# We need the true null as then this means we will get the default behaviour of the ptd function
# if nothing is passed by the user
if(is.na(unique(dataset$baseline_duration))) baseline <- NULL else baseline <- unique(dataset$baseline_duration)

# Extract rebase points (if included) - if none passed, return NULL so that we get default ptd behaviour 
if((dataset %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% nrow()) > 1) rebase_points <- NULL else rebase_points <- dataset %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% select(date) %>% pull() %>% ptd_rebase()

# Get any target values (if included)
# If present, pass through to ptd target function
if(is.na(unique(dataset$target))) target <- NULL else target <- unique(dataset$target) %>% ptd_target()

# Take improvement direction from where it is specified in original dataframe
# TO BE DECIDED - is this best provided in the dataframe, or should this be an option in the PBI dataframe?
# My current thinking is that while dataframe is inefficient for storage, it's far more efficient for creating 
# a lot of visuals at once 
improvement_direction <- dataset %>% 
  tail(1) %>% 
  select(improvement_direction) %>% 
  distinct() %>% 
  pull() %>% 
  # Force as character to appease the PBI service
  as.character()


# Generate NHS R making data count object
ptd_object <- ptd_spc(dataset, 
        value_field = "value",
        date_field="date", 
        improvement_direction = improvement_direction,
        fix_after_n_points = baseline,
        rebase = rebase_points,
        target = target
        ) %>% 
  # We want the underlying dataframe rather than the resulting plot
  # so convert to tibble
  tibble() %>% 
  # Tweak point type text for nicer display
  mutate(point_type = case_when(
    point_type == "special_cause_concern" ~ "Special Cause - Concern",
    point_type == "special_cause_improvement" ~ "Special Cause - Improvement",
    point_type == "common_cause" ~ "Common Cause",
    TRUE ~ "ERROR - CHECK"
  )) 
  
p <- ptd_object %>%
  DT::datatable()

# # Set up palette using NHS identity colours for point types
# # TODO: add in support for neutral variation
# palette <- c("Special Cause - Concern" = "#ED8B00",
#              "Special Cause - Improvement" = "#41B6E6",
#              "Common Cause" = "#768692")

# # Initialise the plotly figure
# fig <- plot_ly(ptd_object, 
#                x = ~x, 
#                colors = palette)

# fig <- fig %>%
#   # Add the main line for the data
#   add_trace(y = ~y, name = 'trace 0',
#             type="scatter",
#             mode = 'lines', line=list(color='#768692'),
#             showlegend=FALSE) %>% 
#   # Add in markers for the data, colouring by the point types
#   # and using the palette we passed when initialising the figure
#   add_trace(y = ~y, 
#             type="scatter",
#             mode = 'markers', color = ~point_type
# ) %>% 
#   # Add in line for lower process limit
#   add_trace(y = ~lpl, name = 'Lower Process Limit',
#             type="scatter",
#             mode = 'lines', line=list(color='#231f20', dash="dot"),
#             showlegend=FALSE) %>% 
#   # Add in line for upper process limit
#   add_trace(y = ~upl, name = 'Upper Process Limit',
#             type="scatter",
#             mode = 'lines', line=list(color='#231f20', dash="dot"),
#             showlegend=FALSE) %>% 
#   # Add in line for mean
#   # TODO: Investigate whether this should be median. Median doesn't appear in plot
#   # but I thought that was MDC methodology - I'm probably misremembering. 
#   add_trace(y = ~mean, name = 'Mean',
#             type="scatter",
#             mode = 'lines', line=list(color='#231f20'),
#             showlegend=FALSE)  

# # If a target is provided, add in a line for the target  
# if (!is.null(target)) {
#   fig <- fig %>% 
#     add_trace(y = ~target, name = 'Target',
#               type="scatter",
#               mode = 'lines', line=list(color='#DA291C', dash="dot"),
#               showlegend=FALSE)  
  
# }

# # Calculate variation type by looking at final point in ptd object
# variation_type <- ptd_object %>% 
#   tail(1) %>% 
#   select(point_type) %>% 
#   pull()

# # Get variation image paths
# # Variation image relies on both the value of the most recent point 
# # and the direction that is counted as improvement
# # Improvement direction was calculated earlier to pass to ptd arguments
# # TODO: Add in support for 'neutral' improvement direction
# if(variation_type == "Special Cause - Concern" & improvement_direction == "decrease") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/concern_high.svg"
# if(variation_type == "Special Cause - Concern" & improvement_direction == "increase") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/concern_low.svg"
# if(variation_type == "Special Cause - Improvement" & improvement_direction == "decrease") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/improvement_low.svg"
# if(variation_type == "Special Cause - Improvement" & improvement_direction == "increase") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/improvement_high.svg"
# if(variation_type == "Common Cause") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/common_cause.svg"


# # Get assurance image paths
# # NHS R PTD package provides a helper function for calculating this from the PTD object
# if (!is.null(target)) {

# assurance_type <- ptd_calculate_assurance_type(ptd_object) %>% select(assurance_type) %>% pull()

# if(assurance_type == "inconsistent") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/inconsistent.svg"
# if(assurance_type =="consistent_pass") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/pass.svg"
# if(assurance_type == "consistent_fail") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/fail.svg"

# }

# if (is.null(target)) assurance_image <- ""

# # Get settings from power bi visual formatting options
# if(exists("titlesettings_ChartTitle")) titlesettings_ChartTitle <- titlesettings_ChartTitle else titlesettings_ChartTitle <- ""
# if(exists("titlesettings_TitleSize")) titlesettings_TitleSize<- titlesettings_TitleSize else titlesettings_TitleSize <- 10

# if(exists("xaxissettings_XAxisTitle")) xaxissettings_XAxisTitle <- xaxissettings_XAxisTitle else xaxissettings_XAxisTitle <- ""

# if(exists("yaxissettings_YAxisTitle")) yaxissettings_YAxisTitle <- yaxissettings_YAxisTitle else yaxissettings_YAxisTitle <- ""

# # Update fig to include variation icon and, if present, assurance icon
# # Also pass in user parameters from the PBI visual formatting options for titles
#   fig <- fig %>% 
#     layout( 
      
#     xaxis = list(title = xaxissettings_XAxisTitle),
#     yaxis = list(title = yaxissettings_YAxisTitle),


#       title=list(text=titlesettings_ChartTitle, 
#                  font=list(size=titlesettings_TitleSize), 
#                  automargin=TRUE, 
#                  yref='container',
#                  yanchor =  'top'
#                  ),

#       # Add in icons for variation and, if target present, assurance
#       # Note that assurance will not always be present, so place variation icon 
#       # in the far left top hand corner and assurance to the right of it if present
#       # Try to get these as far out of the way as possible
#       # TODO: add in user options for icon placement

#       # Useful reference for images:
#       # https://plotly.com/r/reference/layout/images/
#       # https://plotly.com/r/images/

#       # TODO: Work out how to add a tooltip explaining the meaning of the icons on hover. 
#       # From docs, doesn't appear to be something we can add directly to the images
#       # Think we will need an invisible point where the images are 
#       # but this could be tricky to achieve because of the way the image locations 
#       # and sizes are set. 

#       # TODO: Have not yet verified whether the images work when visual is running on
#       # PBI service rather than PBI desktop. Plotly seems to only accept images from web source, 
#       # but I worry that PBI service will block these requests. Note to self - would base64
#       # encoding of the images work if required? Or look into plotly source code at what
#       # exactly is happening at this step - what aspect of it being 'on the web' is crucial?
#       # Because we can include additional assets in the pbi visual package so I don't think 
#       # that's an issue. 

#       images = list(  
        
#         list(  
#           source =  assurance_image,
#           xref="paper", 
#           yref="paper", 
#           x=0.22, 
#           y=1.05, 
#          xanchor="right", 
#          yanchor="top",
#          sizex=0.1, 
#          sizey=0.1
#         ) ,

#          list(  
          
#           source =  variation_image,
#           xref="paper", 
#           yref="paper", 
#           x=0.1, 
#           y=1.05, 
#           xanchor="right", 
#           yanchor="top",
#           sizex=0.1, 
#           sizey=0.1
          
#         )   
        
#       )) 
  
# #fig

# ####################################################

############# Create and save widget ###############
#p <- fig
internalSaveWidget(p, 'out.html');
####################################################

################ Reduce paddings ###################
ReadFullFileReplaceString('out.html', 'out.html', ',"padding":[0-9]*,', ',"padding":0,')
####################################################
