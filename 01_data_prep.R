# Author: Silvan Hofer
# Date: 17.11.2024
# Project: Bocconi Course "Welfare & Politics", Group Project for an Intergenerational Justice Index
# Purpose: 
# Load in the data and metadata for creating the intergenerational opportunity index. 


rm(list = ls())

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)     
library(RColorBrewer) 
library(ggthemes)  
library(grid)       
library(stringr)
library(forcats)

# 0. Read all data in ----------------------------------------------------------
# Set the path to the folder containing the Excel files
path_to_excel_files <- "data/Updated Data/"

# List all Excel files in the folder
excel_files <- list.files(path = path_to_excel_files, pattern = "\\.xlsx?$", full.names = TRUE)

# Initialize an empty list to store data frames
data <- data.frame("Year" = 1950:2024)

# Define the regex pattern to match age group columns
age_group_pattern <- "\\(\\d{2}-\\d{2}\\)"
readin = F
if(readin){
    # Loop over each Excel file
  for (file in 1:length(excel_files)){
    print(file)
    col_names <- names(read_excel(excel_files[file], n_max = 0))
    col_types <- c("numeric", rep("guess", length(col_names)-1))
    
    df <- read_excel(excel_files[file], col_types = col_types)
    names(df)[names(df) == "Age Group"] <- "Age_Group"
    
    # Check column names
    age_group_cols <- names(df)[grepl(age_group_pattern, names(df))]
    other_cols <- setdiff(names(df), age_group_cols)
    
    # Pivot wider for Age groups
    if (any(other_cols == "Age_Group")){
     df <- df %>%
       filter(!is.na(Age_Group))
     df <- df %>%
        pivot_wider(id_cols = "Year", 
                    names_from = "Age_Group", 
                    values_from = other_cols[!other_cols %in% c("Year", "Age_Group")],
                    names_glue = "{.value}_{Age_Group}")
    } 
    
    data <- data%>%
        left_join(df, by = "Year")
  }
  
  writexl::write_xlsx(data, path = "Combined_Data.xlsx")
  rm(file, age_group_cols, age_group_pattern, col_names, col_types, excel_files, file, other_cols, df)

}

# 1. Make it pretty ------------------------------------------------------------
# Decide what to include, create frame with metadata, make decisions there
# load it back in
data <- readxl::read_xlsx(path = "Combined_Data.xlsx")

# Remove data that is in absolute values, is already implicitly defied in other wars (Rent, Inflation) or concerns the wrong age category. 
data <- data %>%
  select(-c(
            `Number of convicted criminals, less 18 years old`,
            `Time spent commuting to work greater than 31 minutes, per 100 pp of same charct, Men_(15-19)`, 
            `Time spent commuting to work greater than 31 minutes, per 100 pp of same charct, Women_(15-19)`, 
            `Time spent commuting to work greater than 31 minutes, per 100 pp of same charct, Total_(15-19)`,
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Men_(14-17)`,
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Men_(0-14 )`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Men_(15-17)`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Men_(18-19)`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Total_(0-14 )`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Total_(14-17)`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Total_(15-17)`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Total_(18-19)`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Women_(0-14 )`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Women_(15-17)`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Women_(18-19)`, 
            `Children who take more than 31 minutes to get to school, per 100pp with same charact, Women_(14-17)`, 
            `Unemployed, 15-24, Abs number, Men`, 
            `Unemployed, 15-24, Abs number, Women`, 
            `Unemployed, 15-24, Abs number, Women`, 
            `Persons (share per 100 pp) they practice sports: never, Men_(3-5)`, 
            `Persons (share per 100 pp) they practice sports: never, Men_(6-10)`, 
            `Persons (share per 100 pp) they practice sports: never, Men_(11-14)`, 
            `Persons (share per 100 pp) they practice sports: never, Men_(15-17)`, 
            `Persons (share per 100 pp) they practice sports: never, Men_(18-19)`, 
            `Persons (share per 100 pp) they practice sports: never, Women_(3-5)`, 
            `Persons (share per 100 pp) they practice sports: never, Women_(6-10)`, 
            `Persons (share per 100 pp) they practice sports: never, Women_(11-14)`, 
            `Persons (share per 100 pp) they practice sports: never, Women_(15-17)`, 
            `Persons (share per 100 pp) they practice sports: never, Women_(18-19)`, 
            `Persons (share per 100 pp) they practice sports: never, Total_(3-5)`, 
            `Persons (share per 100 pp) they practice sports: never, Total_(6-10)`,
            `Persons (share per 100 pp) they practice sports: never, Total_(11-14)`,
            `Persons (share per 100 pp) they practice sports: never, Total_(15-17)`,
            `Persons (share per 100 pp) they practice sports: never, Total_(18-19)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Men_(3-5)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Men_(6-10)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Men_(11-14)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Men_(15-17)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Men_(18-19)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Total_(3-5)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Total_(6-10)`,
            `Persons (share per 100 pp) they practice sports: continuosly, Total_(11-14)`,
            `Persons (share per 100 pp) they practice sports: continuosly, Total_(15-17)`,
            `Persons (share per 100 pp) they practice sports: continuosly, Total_(18-19)`,
            `Persons (share per 100 pp) they practice sports: continuosly, Women_(3-5)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Women_(6-10)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Women_(11-14)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Women_(15-17)`, 
            `Persons (share per 100 pp) they practice sports: continuosly, Women_(18-19)`,
            `Annual Change`, 
            `Employment, 15-24, Absolute Number, Men`,
            `Employment, 15-24, Absolute Number, Total`,
            `Employment, 15-24, Absolute Number, Women`,
            `Italians who emigrated abroad, until 17 years, Absoulute Value, Men`,
            `Italians who emigrated abroad, until 17 years, Absoulute Value, Women`, 
            `Italians who emigrated abroad, until 17 years, Absoulute Value, Total`
            )
         )


write.csv(names(data), file = "variable_names.csv")

# Initially create the Metadata frame. Then change it to adjust parameters. 
execute_metadata = F #DO NOT execute. Otherwise you might erase Metadata decision. 
if(execute_metadata){
    
  # Define the old variable names
  old_names <- c(
    "Year",
    "Central government debt, total (% of GDP)",
    "Employment to population ratio, ages (15-24), total (%) (national estimate)",
    "Incidence of HIV, ages (15-24) (per 1,000 uninfected population ages(15-24))",
    "Labor force participation rate for ages (15-24), total (%) (national estimate)",
    "Population, total",
    "Rural population (% of total population)",
    "Early leavers from education and training, 18-24, percentage values, Men",
    "Early leavers from education and training, 18-24, percentage values, Women",
    "Early leavers from education and training, 18-24, percentage values, Total",
    "Primary Educ, Participation Rate, Men",
    "Primary Educ, Participation Rate, Women",
    "Primary Educ, Participation Rate, Total",
    "Lower Seconday Educ, Participation Rate, Men",
    "Lower Seconday Educ, Participation Rate, Women",
    "Lower Seconday Educ, Participation Rate, Total",
    "Upper Secondary Educ, Participation Rate, Men",
    "Upper Secondary Educ, Participation Rate, Women",
    "Upper Secondary Educ, Participation Rate, Total",
    "Employment Rate, 15-24, Men",
    "Employment Rate, 15-24, Women",
    "Employment Rate, 15-24, Total",
    "Employment Rate",
    "Households that can't afford to save, until 35, percent of households",
    "Households that can't face unexpected expenses, until 35, percent of households",
    "Tenure Status, until 35, Rent",
    "Tenure Status, until 35, Property",
    "Inflation Rate (%)",
    "Tertiary Education, 14-24, per 100pp, Frequency in which talks about politics: everyday, Men",
    "Tertiary Education, 14-24,  per 100pp, Frequency in which talks about politics: never, Men",
    "Tertiary Education, 14-24, per 100pp, Listen to political debates, Men",
    "Tertiary Education, 14-24, per 100pp, Financially Support a Party, Men",
    "Tertiary Education, 14-24, per 100pp, Frequency in which talks about politics: everyday, Women",
    "Tertiary Education, 14-24, per 100pp, Frequency in which talks about politics: never, Women",
    "Tertiary Education, 14-24, per 100pp, Listen to political debates, Women",
    "Tertiary Education, 14-24, per 100pp, Financially Support a Party, Women",
    "Tertiary Education, 14-24, per 100pp, Frequency in which talks about politics: everyday, Total",
    "Tertiary Education, 14-24, per 100pp, Frequency in which talks about politics: never, Total",
    "Tertiary Education, 14-24, per 100pp, Listen to political debates, Total",
    "Tertiary Education, 14-24, per 100pp, Financially Support a Party, Total",
    "Real GDP per capita (EUR)",
    "Household relative poverty incidence (% of households in relative poverty, Italy, 18-34)",
    "Rent price (2015=100)",
    "Share of General Govt Expenditure to Primary-Terciary Educ",
    "Persons (share per 100 pp) they practice sports: continuosly, Men_(20-24)",
    "Persons (share per 100 pp) they practice sports: never, Men_(20-24)",
    "Persons (share per 100 pp) they practice sports: continuosly, Women_(20-24)",
    "Persons (share per 100 pp) they practice sports: never, Women_(20-24)",
    "Persons (share per 100 pp) they practice sports: continuosly, Total_(20-24)",
    "Persons (share per 100 pp) they practice sports: never, Total_(20-24)",
    "Total Expenditure of Public Works, Absolute Value",
    "Unemployment rate",
    "Unemployed, 15-24, Abs number, Total",
    "Uni Graduates, per 100, Men",
    "Uni Graduates, per 100, Women",
    "Uni Graduates, absolute value",
    "Children who take more than 31 minutes to get to school, per 100pp with same charact, Men_(20-24)",
    "Children who take more than 31 minutes to get to school, per 100pp with same charact, Women_(20-24)",
    "Children who take more than 31 minutes to get to school, per 100pp with same charact, Total_(20-24)",
    "Time spent commuting to work greater than 31 minutes, per 100 pp of same charct, Men_(20-24)",
    "Time spent commuting to work greater than 31 minutes, per 100 pp of same charct, Women_(20-24)",
    "Time spent commuting to work greater than 31 minutes, per 100 pp of same charct, Total_(20-24)"
  )
  
  # Create metadata dynamically
  metadata <- tibble(
    Old_Name = old_names,
    Short_Name = c(
      "Year", "Govt_Debt", "Emp_Pop_15_24", "HIV_Incidence_15_24", "Labor_Part_15_24", "Population",
      "Rural_Pop", "Early_Leavers_Men", "Early_Leavers_Women", "Early_Leavers_Total",
      "Prim_Part_Men", "Prim_Part_Women", "Prim_Part_Total",
      "LowerSec_Part_Men", "LowerSec_Part_Women", "LowerSec_Part_Total",
      "UpperSec_Part_Men", "UpperSec_Part_Women", "UpperSec_Part_Total",
      "Emp_Rate_Men_15_24", "Emp_Rate_Women_15_24", "Emp_Rate_Total_15_24",
      "Emp_Rate", "Cant_Save", "Cant_Handle_Expenses", "Rent", "Property",
      "Inflation", "Politics_Everyday_Men", "Politics_Never_Men",
      "Politics_Debates_Men", "Politics_Financial_Men",
      "Politics_Everyday_Women", "Politics_Never_Women",
      "Politics_Debates_Women", "Politics_Financial_Women",
      "Politics_Everyday_Total", "Politics_Never_Total",
      "Politics_Debates_Total", "Politics_Financial_Total",
      "GDP", "Poverty_18_34", "Rent_Index", "Govt_Educ_Spend",
      "Sports_Cont_Men", "Sports_Never_Men", "Sports_Cont_Women",
      "Sports_Never_Women", "Sports_Cont_Total", "Sports_Never_Total",
      "Public_Works", "Unemp_Rate", "Unemp_Abs_15_24", "Uni_Grad_Men",
      "Uni_Grad_Women", "Uni_Grad_Total", "Commute_31_Men",
      "Commute_31_Women", "Commute_31_Total", "Work_Commute_Men",
      "Work_Commute_Women", "Work_Commute_Total"
    ),
    Age_Range = ifelse(grepl("\\d+-\\d+", old_names), gsub(".*?(\\d+-\\d+).*", "\\1", old_names), NA),
    Unit = ifelse(grepl("\\(%|per|100|absolute)", old_names), "Percentage", "Value"),
    Category = ifelse(
      grepl("educ|politics", old_names, ignore.case = TRUE), "Education and Culture",
      ifelse(grepl("health|hiv", old_names, ignore.case = TRUE), "Health and Wellbeing",
             ifelse(grepl("population|poverty|inflation", old_names, ignore.case = TRUE),
                    "Macroeconomic Environment", "Access to Resources"))
    ),
    Higher_Is_Better = !grepl("poverty|unemployment", old_names, ignore.case = TRUE),
    Gender = ifelse(grepl("Men", old_names), "Male",
                    ifelse(grepl("Women", old_names), "Female", "Neutral")),
    Alig = ifelse(
      !is.na(Age_Range),
      ceiling(mean(as.numeric(unlist(strsplit(Age_Range, "-"))))) - 25,
      NA
    )
  )
  writexl::write_xlsx(metadata, "Metadata1.xslx")
}

# Get Metadata 
metadata <- readxl::read_xlsx("Metadata1.xlsx")

# Rename variables and exclude Variables we have decided against
name_map <- setNames(metadata$Short_Name, metadata$Old_Name)
data <- data %>%
  rename_with(~ name_map[.x], .cols = intersect(names(data),names(name_map)))

#remove the columns we decided not to include. 
leave_out <- metadata$Short_Name[metadata$Include != "TRUE"]
data <- data %>%
  select(-leave_out)

rm(leave_out, name_map, execute_metadata)
# 2. Data Prep -----------------------------------------------------------------

data <- data.frame(apply(data, 2, as.numeric))
metadata <- metadata %>%
  rename("Indicator" = "Short_Name")

# Change format and join the metadata for processing. 
data <- data %>%
  mutate(Public_Works = Public_Works/Population)%>%
  mutate(Emigration_Total = Emigration_Total/Population)%>%
  mutate(Uni_Grad_Total = Uni_Grad_Total/Population)%>%
  select(-Population)%>%
  pivot_longer(cols = 2:(ncol(data)-1), names_to = "Indicator", values_to = "Value")%>%
  left_join(metadata, by = "Indicator")


writexl::write_xlsx(data, path = "Combined_Data_Clean.xlsx")
data <- readxl::read_xlsx(path = "Combined_Data_Clean.xlsx")


## 2.1 Smooth -----------------------------------------------------------------


interpolate_values <- function(values) {
  # Only interpolate values bounded by non-NA values on both sides
  interpolated <- zoo::na.approx(values, na.rm = FALSE)
  
  # Ensure leading and trailing NAs remain untouched
  interpolated[is.na(values) & cumsum(!is.na(values)) == 0] <- NA
  interpolated[is.na(values) & rev(cumsum(rev(!is.na(values)))) == 0] <- NA
  
  return(interpolated)
}

# Apply smoothing 
data <- data %>%
  group_by(Indicator) %>% 
  mutate(Value = interpolate_values(Value)) %>%
  ungroup()


rm(interpolate_values)
## 2.2 Lagging ------------------------------------------------------------------


reference_age <- 25

# Get the age related variables and lag them by birth year
age_vars <- metadata$Indicator[!is.na(metadata$Alig)]
data_lagged <- data %>%
  filter(Indicator %in% age_vars)%>%
  mutate(Birth_Year = Year - reference_age - Alig) #decided to shift it up to 25 as "average" youth age when you kind of"hit the road"

  
# Get the non-age related variables
non_age_vars <- metadata$Indicator[!metadata$Indicator %in% age_vars]
data_year <- data %>%
  filter(Indicator %in% non_age_vars)%>%
  mutate(Birth_Year = Year - reference_age) #decided to shift it up to 25 as "average" youth age when you kind of"hit the road"


data <- rbind(data_lagged, data_year)


rm(age_vars, data_lagged, non_age_vars, data_year, reference_age)
##  2.3 Normalise and invert ---------------------------------------------------------

# Invert indicators where appropriate by Max_Value-Vale
# Normalise by turning into percentage of Max-Min. 
data <- data %>%
  group_by(Indicator) %>% # Group by Indicator for calculating Max
  mutate(
    Max_Value = if_else(!Higher_Is_Better, max(Value, na.rm = TRUE), NA_real_),
    Transformed_Value = if_else(!Higher_Is_Better, Max_Value - Value, Value)
  ) %>%
  mutate(
    Min_Value = min(Transformed_Value, na.rm = TRUE),
    Max_Value = max(Transformed_Value, na.rm = TRUE),
    Percentage_Value = (Transformed_Value - Min_Value) / (Max_Value - Min_Value)
  ) %>%
  ungroup() %>%
  select(-Max_Value,-Min_Value, -Higher_Is_Better) # Remove intermediate columns if not needed




# 3. Create Index --------------------------------------------------------------

writexl::write_xlsx(data, path = "Final_Data_Clean.xlsx")
data <- readxl::read_xlsx(path = "Final_Data_Clean.xlsx")

data <- data %>%
  filter(Birth_Year %in% 1950:1997)

data <- data %>%
  mutate(Category = ifelse(Category == "Access to Resources", "Living Standards", Category))




genders <- c("Both","Male", "Female")
weights <- c("Weights", "Weights_new")

for(weight in weights){
  for(gender in genders){
    
    data_loop <- data %>%
      filter(Gender == "Neutral" | Gender == gender)
    
    eval(parse(text = paste0("data_loop$Weights <- data_loop$", weight)))
    
    
    data_loop <- data_loop %>% #better overview
      select(c(Birth_Year, Indicator, Category, Percentage_Value, Weights))
    
    
    # rescale Weights: not all variables are available for all years. 
    # Check how many variables are available and rescale if necessary. 
    Index_Categories <- data_loop %>%
      group_by(Birth_Year, Category) %>%
      mutate(
        # Assign baseline weights: Equal weights within each category
        Baseline_Weight = 1 / n(),  # Equal weights for all indicators within a category
        
        # Adjust weights based on available data
        Total_Weight_Available = sum(Weights[!is.na(Percentage_Value)], na.rm = TRUE),  # Sum weights of available indicators
        Rescaled_Weight = if_else(!is.na(Percentage_Value), Weights / Total_Weight_Available, NA_real_),  # Rescale weights
        Weighted_Value = if_else(!is.na(Percentage_Value), Percentage_Value * Rescaled_Weight, 0),  # Calculate weighted values
        
        # Adjust weights for baseline weights
        Baseline_Total_Weight_Available = sum(Baseline_Weight[!is.na(Percentage_Value)], na.rm = TRUE),
        Rescaled_Baseline_Weight = if_else(!is.na(Percentage_Value), Baseline_Weight / Baseline_Total_Weight_Available, NA_real_),
        Weighted_Baseline_Value = if_else(!is.na(Percentage_Value), Percentage_Value * Rescaled_Baseline_Weight, 0)
      )%>%
    filter(Total_Weight_Available != 0)%>%
      # Summarize the weighted values for each category and year
      summarise(
        Aggregated_Value = sum(Weighted_Value, na.rm = TRUE),  # Aggregated with actual weights
        Aggregated_Baseline_Value = sum(Weighted_Baseline_Value, na.rm = TRUE)  # Aggregated with baseline weights
      )
    
    
    # Aggregating categories to one index
    Index <- Index_Categories %>%
      group_by(Birth_Year) %>%
      mutate(
        num_categories = length(unique(Category)),  # Count the number of unique categories
        Index = sum(Aggregated_Value) / num_categories,  # Calculate the index for Aggregated_Value
        Baseline_Index = sum(Aggregated_Baseline_Value) / num_categories  # Calculate the index for Aggregated_Baseline_Value
      ) %>%
      select(-Category, -Aggregated_Value, -Aggregated_Baseline_Value) %>%  # Remove unnecessary columns
      unique()  # Remove duplicate rows
    
    eval(parse(text = paste0("datasets_",gender,"_", weight ," <- list(Index, Index_Categories)")))
    
  }
}





# 4. Plot ----------------------------------------------------------------------
weights <- c(weights,"Baseline")



for(weight in weights){
  for(gender in genders){
    
    if(weight == "Baseline"){
      eval(parse(text = paste0("data_loop <- datasets_",gender,"_Weights")))
      data_loop[[2]]$Aggregated_Value <- data_loop[[2]]$Aggregated_Baseline_Value
      data_loop[[1]]$Index <- data_loop[[1]]$Baseline_Index
      
    } else{
        
      eval(parse(text = paste0("data_loop <- datasets_",gender,"_", weight)))
      
    }

    p_categories <- ggplot(data_loop[[2]], aes(x = Birth_Year, y = Aggregated_Value, color = Category)) +
      geom_line(size = 2) + # Thicker lines for better visibility
      scale_color_brewer(palette = "Set1") + # Better color palette for distinction
      theme_minimal() + # Clean, minimalistic theme
      labs(
        #title = paste0("Trends by Birth Year for ", gender, " weighted by ", weight),
        x = "Birth Year",
        y = "Index",
        color = "Category"
      ) + # Add labels and title
      theme(
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), # Center and style title
        axis.title = element_text(size = 20, face = "bold"), # Style axis titles
        axis.text = element_text(size = 20), # Adjust axis text size
        legend.position = "top", # Place legend at the top
        legend.text = element_text(size = 22), # Increase legend text size
        legend.title = element_text(size = 20, face = "bold"), # Place legend at the top
        plot.margin = margin(10, 10, 10, 10)
      ) 
      
    
    
    
    p_index <- ggplot(data_loop[[1]], aes(x = Birth_Year, y = Index)) +
      geom_line(color = "steelblue", size = 2) + # Thicker, colored line
      theme_minimal() + # Clean, minimalistic theme
      ylim(0,1)+
      labs(
        #title = paste0("Index Over Birth Years for ", gender, " weighted by ", weight),
        x = "Birth Year",
        y = "Index"
      ) + # Add descriptive title and axis labels
      theme(
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), # Center and style title
        axis.title = element_text(size = 20, face = "bold"), # Style axis titles
        axis.text = element_text(size = 20), # Adjust axis text size
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        plot.margin = margin(10, 10, 10, 10)
        )
    
    ggsave(plot = p_categories, 
           width = 20, height = 10, units = "in",
           file = paste0("charts/Index_Categories_",gender,"_",weight ,".pdf"))
    ggsave(plot = p_index,
           width = 20, height = 10, units = "in",
           file = paste0("charts/Index_",gender,"_",weight ,".pdf"))
    
    
  }
}


## 4.3 Plot the data availability ----------------------------------------------



years <- sort(unique(data$Birth_Year))
years_to_show <- years[seq(1, length(years), by = 5)]

data_display <- data %>%
  filter(Birth_Year %in% 1950:2023)


areas <- unique(data_display$Category)
for(area in areas){
  
  test <- data_display %>%
    filter(Category == area)
  
  heatmap_plot <- ggplot(test, aes(x = factor(Birth_Year), y = fct_rev(Indicator), fill = Percentage_Value)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradientn(
      colors = c("red", "yellow", "green"),  # Red for lower values, Yellow for mid, Green for higher
      na.value = "grey80",
      #limits = c(0, 1),
      name = "Score (Higher = Better)",
      guide = guide_colorbar(barwidth = 15, barheight = 1, title.position = "top")
    )+
    scale_x_discrete(breaks = years_to_show) +
    labs(title = "Intergenerational Opportunities by Year of Birth at Age 25 with Lags",
         x = NULL,
         y = NULL) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(b = 15)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, margin = margin(t = 5)),
      axis.text.y = element_text(angle = 0, size = 10),
      axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15)),
      legend.position = "bottom",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.key.width = unit(2.5, "cm"),
      legend.key.height = unit(1.5, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    #coord_fixed() +
    scale_y_discrete(labels = function(labels) str_wrap(labels, width = 15))+
    facet_wrap(~ Category, scales = "free_y", ncol = 1)  # Create facets by Category


  ggsave(heatmap_plot, filename = paste0("charts/Opportunity_Heatmap_",area, ".pdf"),width = 16,height = 9, units = "in")
  ggsave(heatmap_plot, filename = paste0("charts/Opportunity_Heatmap_",area, ".png"),width = 16,height = 9, units = "in")
  
}




heatmap_plot <- ggplot(data_display, aes(x = factor(Birth_Year), y = fct_rev(Indicator), fill = Percentage_Value)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradientn(
    colors = c("red", "yellow", "green"),  # Red for lower values, Yellow for mid, Green for higher
    na.value = "grey80",
    #limits = c(0, 1),
    name = "Score (Higher = Better)",
    guide = guide_colorbar(barwidth = 15, barheight = 1, title.position = "top")
  )+
  scale_x_discrete(breaks = years_to_show) +
  labs(title = "Intergenerational Opportunities by Year of Birth at Age 25 with Lags",
       x = NULL,
       y = NULL) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, margin = margin(t = 5)),
    axis.text.y = element_text(angle = 0, size = 10),
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15)),
    legend.position = "bottom",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  #coord_fixed() +
  scale_y_discrete(labels = function(labels) str_wrap(labels, width = 15))+
  facet_wrap(~ Category, scales = "free_y", ncol = 1)  # Create facets by Category


ggsave(heatmap_plot, filename = paste0("charts/Opportunity_Heatmap.pdf"),width = 16,height = 9, units = "in")
ggsave(heatmap_plot, filename = paste0("charts/Opportunity_Heatmap.png"),width = 16,height = 9, units = "in")

