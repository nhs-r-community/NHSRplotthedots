source('./r_files/flatten_HTML.r')
source("./r_files/ptd_spc.R")
source("./r_files/ptd_rebase.R")
source("./r_files/ptd_target.R")
source("./r_files/ptd_spc_options.R")
source("./r_files/ptd_validate_spc_options.R")
source("./r_files/ptd_validate_plot_options.R")
source("./r_files/to_datetime.R")
source("./r_files/ZZZ.R")
source("./r_files/ptd_add_rebase_column.R")
source("./r_files/ptd_calculate_point_type.R")
source("./r_files/ptd_add_short_group_warnings.R")
source("./r_files/ptd_add_target_column.R")
source("./r_files/ptd_calculate_assurance_type.R")
source("./r_files/ptd_spc_standard.R")

############### Library Declarations ###############
libraryRequireInstall("plotly")
libraryRequireInstall("dplyr")
####################################################

################### Actual code ####################
dataset <- Values %>% 
    mutate(date = as.Date(date))


# Extract option for baseline length
# Slightly odd layout required to get true null as return value
if(is.na(unique(dataset$baseline_duration))) baseline <- NULL else baseline <- unique(dataset$baseline_duration)

# Extract rebase points
if((dataset %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% nrow()) > 1) rebase_points <- NULL else rebase_points <- dataset %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% select(date) %>% pull() %>% ptd_rebase()

# Slightly odd layout required to get true null as return value
if(is.na(unique(dataset$target))) target <- NULL else target <- unique(dataset$target) %>% ptd_target()


ptd_object <- ptd_spc(dataset, value_field = "value",
        date_field="date", 
        fix_after_n_points = baseline,
        rebase = rebase_points,
        target = target
        ) %>% 
  tibble() %>% 
  mutate(point_type = case_when(
    point_type == "special_cause_concern" ~ "Special Cause - Concern",
    point_type == "special_cause_improvement" ~ "Special Cause - Improvement",
    point_type == "common_cause" ~ "Common Cause",
    TRUE ~ "ERROR - CHECK"
  ))



palette <- c("Special Cause - Concern" = "#ED8B00",
             "Special Cause - Improvement" = "#41B6E6",
             "Common Cause" = "#768692")


fig <- plot_ly(ptd_object, x = ~x, 
               colors = palette)

fig <- fig %>% 
  add_trace(y = ~y, name = 'trace 0',
            type="scatter",
            mode = 'lines', line=list(color='#768692'),
            showlegend=FALSE) %>% 
  add_trace(y = ~y, 
            type="scatter",
            mode = 'markers', color = ~point_type
) %>% 
  add_trace(y = ~lpl, name = 'Lower Process Limit',
            type="scatter",
            mode = 'lines', line=list(color='#231f20', dash="dot"),
            showlegend=FALSE) %>% 
  add_trace(y = ~upl, name = 'Upper Process Limit',
            type="scatter",
            mode = 'lines', line=list(color='#231f20', dash="dot"),
            showlegend=FALSE) %>% 
  add_trace(y = ~mean, name = 'Mean',
            type="scatter",
            mode = 'lines', line=list(color='#231f20'),
            showlegend=FALSE)  
  
if (!is.null(target)) {
  fig <- fig %>% 
    add_trace(y = ~target, name = 'Target',
              type="scatter",
              mode = 'lines', line=list(color='#DA291C', dash="dot"),
              showlegend=FALSE)  
  
}


variation_type <- ptd_calculate_assurance_type(ptd_object) %>% select(assurance_type) %>% pull()

if(variation_type == "inconsistent") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/inconsistent.svg"
if(variation_type =="consistent_pass") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/pass.svg"
if(variation_type == "consistent_fail") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/fail.svg"

#if (!is.null(variation_image)) {
  fig <- fig %>% 
    layout( 
      
      images = list(  
        
        list(  
          
          # sources of images
          
          source =  assurance_image,
          
          xref="paper", 
          
          yref="paper", 
          
          x=0.1, 
          
          y=1, 
          
         xanchor="right", 
          
         yanchor="top",
          
         sizex=0.1, 
          
         sizey=0.1
          
        )  
        
      )) 
  
#}

fig
####################################################

############# Create and save widget ###############
p <- fig
internalSaveWidget(p, 'out.html');
####################################################

################ Reduce paddings ###################
ReadFullFileReplaceString('out.html', 'out.html', ',"padding":[0-9]*,', ',"padding":0,')
####################################################
