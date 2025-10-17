# Load the necessary libraries
library("tidyverse")
library("readxl")

#---------Create a custom theme (theme_minimal) for presentation slides---------

custom_theme_slide <- theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    # Titles
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Centered
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    
    # Axis titles and text
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    
    # Legend
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "bottom",  # Better for horizontal slides
    legend.key.size = unit(0.7, "cm"),
    
    # Panel & grid
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.major.x = element_blank(),  # Optional: Hide vertical grid
    panel.grid.minor = element_blank(),
    
    # Margins and spacing
    plot.margin = margin(10, 15, 10, 15)
  )

#------------------------SECTION 1: Demographic Tables--------------------------

# A. Age

# Import the data
age_df <- read_excel("demographic_tables.xlsx", sheet = 1)
dim(age_df)

# Reshape data from wide to long format for grouped bar chart
library(tidyr)
age_df2 <- pivot_longer(
  age_df,
  cols = c(Safeguarding_Concerns, Section42_Enquiries),
  names_to = "Type",
  values_to = "Count"
)


# Plot grouped bar chart
ggplot(age_df2, aes(x = AgeBand, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = "Safeguarding: Counts of Individuals by Age Band",
    x = "Age Band",
    y = "Number of Individuals",
    fill = "Key"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00"),
                    labels = c("Concerns raised", "Enquiries commenced")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), vjust = -0.4) 

#-------------------------------------------------------------------------------

# B. Gender

# Import the data
gender_df <- read_excel("demographic_tables.xlsx", sheet = 2)
dim(gender_df)

# Reshape data to long format for grouped bar chart
library(tidyr)
gender_df2 <- pivot_longer(
  gender_df,
  cols = c(Safeguarding_Concerns, Section42_Enquiries),
  names_to = "Type",
  values_to = "Count"
)


# Plot grouped bar chart
ggplot(gender_df2, aes(x = Gender, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = "Safeguarding: Counts of Individuals by Gender",
    x = "Gender",
    y = "Number of Individuals",
    fill = "Key"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00"),
                    labels = c("Concerns raised", "Enquiries commenced")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), vjust = -0.4) 

#-------------------------------------------------------------------------------

# C. Ethnicity

# Import the data
ethnicity_df <- read_excel("demographic_tables.xlsx", sheet = 3)
dim(ethnicity_df)

# Reshape data to long format for grouped bar chart
library(tidyr)
ethnicity_df2 <- pivot_longer(
  ethnicity_df,
  cols = c(Safeguarding_Concerns, Section42_Enquiries),
  names_to = "Type",
  values_to = "Count"
)


# Plot grouped bar chart
ggplot(ethnicity_df2, aes(x = reorder(Ethnicity, Count), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  coord_flip() +
  labs(
    title = "Safeguarding: Counts of Individuals by Ethnicity",
    x = "Ethnicity",
    y = "Number of Individuals",
    fill = "Key"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00"),
                    labels = c("Concerns raised", "Enquiries commenced")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), hjust = -0.4) 
        

#-------------------------------------------------------------------------------

# D. Primary Support Reason

# Import the data
psr_df <- read_excel("demographic_tables.xlsx", sheet = 4)
dim(psr_df)

# Reshape data to long format for grouped bar chart
library(tidyr)
psr_df2 <- pivot_longer(
  psr_df,
  cols = c(Safeguarding_Concerns, Section42_Enquiries),
  names_to = "Type",
  values_to = "Count"
)


# Plot grouped bar chart
ggplot(psr_df2, aes(x = reorder(Primary_Support_Reasons, Count), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  coord_flip() +
  labs(
    title = "Safeguarding: Counts of Individual by Primary Support Reasons",
    x = "Primary Support Reason",
    y = "Number of Individuals",
    fill = "Key"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00"),
                    labels = c("Concerns raised", "Enquiries commenced")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), hjust = -0.4)


#-------------------------------------------------------------------------------

# E. Primary Support Type

# Import the data
pst_df <- read_excel("demographic_tables.xlsx", sheet = 5)
dim(pst_df)

# Reshape data to long format for grouped bar chart
library(tidyr)
pst_df2 <- pivot_longer(
  pst_df,
  cols = c(Safeguarding_Concerns, Section42_Enquiries),
  names_to = "Type",
  values_to = "Count"
)


# Plot grouped bar chart
ggplot(pst_df2, aes(x = reorder(Primary_Support_Type, Count), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  coord_flip() +
  labs(
    title = "Safeguarding: Types of Individual by Primary Support Reasons",
    x = "Type of Support",
    y = "Number of Individuals",
    fill = "Key"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00"),
                    labels = c("Concerns raised", "Enquiries commenced"))


#-------------------------------------------------------------------------------

# F. Safeguarding Activity

# Import the data
sa_df <- read_excel("demographic_tables.xlsx", sheet = 6)
dim(sa_df)

# Create a blank theme for the pie chart
blank_theme <- theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )
# The coord_polar() function is used to produce a pie chart, 
# which is just a stacked bar chart in polar coordinates. 
sa_df %>% 
  ggplot(aes(x = "", y = Count, fill = Safeguarding_Activity))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  blank_theme +
  labs(title = "Overall Summary of Safeguarding Activity",
       fill = "Safeguarding Activity") +
  scale_fill_manual(values = c("#0072B2", "#D55E00"),
                    labels = c("Concerns raised", "Enquiries commenced")) +
  theme(axis.text.x=element_blank()) +
  geom_label(aes(label = paste0(round((Count / sum(Count)) * 100, digits = 2), "%")),
             position = position_stack(vjust = 0.5), show.legend = FALSE) # If show.legend is set to TRUE, it returns "a"

#-------------------------------------------------------------------------------


#------------------------SECTION 2: Case Detail Tables--------------------------

# A. Case Type and Sources of Risk

# Import the data
abuse_df <- read_excel("case_detail_tables.xlsx", sheet = 1)
dim(abuse_df)

# Reshape data to long format for grouped bar chart
abuse_df2 <- pivot_longer(
  abuse_df,
  cols = c(Service_provider, Other_known_to_individual, Other_not_known_to_individual),
  names_to = "Source_of_risk",
  values_to = "Count"
)

# Remove rows where Count is zero and pre-calculate total count per abuse type.
abuse_df2_clean <- abuse_df2 %>%
  filter(Count > 0) %>%
  group_by(Type_of_abuse) %>%
  mutate(Total = sum(Count)) %>%
  ungroup() %>%
  mutate(Type_of_abuse = reorder(Type_of_abuse, Total))

# Plot grouped bar chart
ggplot(abuse_df2_clean, aes(x = Type_of_abuse, y = Count, fill = Source_of_risk)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  coord_flip() +
  labs(
    title = "Enquiries by Types of Abuse and Sources of Risk",
    x = "Type of Abuse",
    y = "Number of Individuals",
    fill = "Source of Risk"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "green"),
                    labels = c("Others, known to individual", "Others, not known to individual", "Service Provider")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), hjust = -0.4) 

#-------------------------------------------------------------------------------

# B. Case Location and Sources of Risk

# Import the data
loc.abuse_df <- read_excel("case_detail_tables.xlsx", sheet = 2)
dim(loc.abuse_df)

# Reshape data to long format for grouped bar chart
loc.abuse_df2 <- pivot_longer(
  loc.abuse_df,
  cols = c(Service_provider, Other_known_to_individual, Other_not_known_to_individual),
  names_to = "Source_of_risk",
  values_to = "Count"
)

# Remove rows where Count is zero and pre-calculate total count per location type.
loc.abuse_df2_clean <- loc.abuse_df2 %>%
  filter(Count > 0) %>%
  group_by(Location_of_abuse) %>%
  mutate(Total = sum(Count)) %>%
  ungroup() %>%
  mutate(Location_of_abuse = reorder(Location_of_abuse, Total))

# Plot grouped bar chart
ggplot(loc.abuse_df2_clean, aes(x = Location_of_abuse, y = Count, fill = Source_of_risk)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  coord_flip() +
  labs(
    title = "Enquiries by Location of Abuse and Sources of Risk",
    x = "Location of Abuse",
    y = "Number of Individuals",
    fill = "Source of Risk"
  ) +
  custom_theme_slide + 
  scale_fill_manual(values = c("#0072B2", "#D55E00", "green"),
                    labels = c("Others, known to individual", "Others, not known to individual", "Service Provider")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), hjust = -0.4) 

#-------------------------------------------------------------------------------

# C. Case Type and Locations of Risk

# Import the data
type.loc_df <- read_excel("case_detail_tables.xlsx", sheet = 3)
dim(type.loc_df)

# Reshape data to long format for grouped bar chart
type.loc_df2 <- pivot_longer(
  type.loc_df,
  cols = c(Own_home, In_the_community_excluding_community_services, In_a_community_service, Care_home_nursing, Care_home_residential,
           Hospital_acute, Hospital_mental_health, Other),
  names_to = "Location_of_risk",
  values_to = "Count"
)

# Remove rows where Count is zero and pre-calculate total count per location of risk.
type.loc_df2_clean <- type.loc_df2 %>%
  filter(Count > 0) %>%
  group_by(Type_of_abuse) %>%
  mutate(Total = sum(Count)) %>%
  ungroup() %>%
  mutate(Type_of_abuse = reorder(Type_of_abuse, Total))


# Plot grouped bar chart
ggplot(type.loc_df2_clean, aes(x = Type_of_abuse, y = Count, fill = Location_of_risk)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  coord_flip() +
  labs(
    title = "Enquiries by Type of Abuse and Location of Risk",
    x = "Type of Abuse",
    y = "Number of Individuals",
    fill = "Location of Risk"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "green", "red", "yellow", "violet", "tan", "pink", "darkgreen", "orange", "darkblue"),
                    labels = c("Care home - nursing", "Care home - residential" , "Hospital - acute", "Hospital - mental health", "In a community service",
                               "In the community excluding community services", "Other", "Own home")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), hjust = -0.4, size = 3) 

#--------------------------------------------------------------------------------

# D. Risk Assessment and Sources of Risk

# Import the data
risk.assessment_df <- read_excel("case_detail_tables.xlsx", sheet = 4)
dim(risk.assessment_df)

# Reshape data to long format for grouped bar chart
risk.assessment_df2 <- pivot_longer(
  risk.assessment_df,
  cols = c(Service_provider, Other_known_to_individual, Other_not_known_to_individual),
  names_to = "Source_of_risk",
  values_to = "Count"
)

# Remove rows where Count is zero and pre-calculate total count per risk assessment type.
risk.assessment_df2_clean <- risk.assessment_df2 %>%
  filter(Count > 0) %>%
  group_by(Risk_assessment) %>%
  mutate(Total = sum(Count)) %>%
  ungroup() %>%
  mutate(Risk_assessment = reorder(Risk_assessment, Total))

# Plot grouped bar chart
ggplot(risk.assessment_df2_clean, aes(x = Risk_assessment, y = Count, fill = Source_of_risk)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  coord_flip() +
  labs(
    title = "Risk Assessment Outcomes of the Sources of Risk",
    x = "Risk Assessment Outcome",
    y = "Number of Individuals",
    fill = "Source of Risk"
  ) +
  custom_theme_slide + 
  scale_fill_manual(values = c("#0072B2", "#D55E00", "green"),
                    labels = c("Others, known to individual", "Others, not known to individual", "Service Provider")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), hjust = -0.4) 

#-------------------------------------------------------------------------------

# E. Risk Outcomes and Sources of Risk

# Import the data
risk.outcome_df <- read_excel("case_detail_tables.xlsx", sheet = 5)
dim(risk.outcome_df)

# Reshape data to long format for grouped bar chart
risk.outcome_df2 <- pivot_longer(
  risk.outcome_df,
  cols = c(Service_provider, Other_known_to_individual, Other_not_known_to_individual),
  names_to = "Source_of_risk",
  values_to = "Count"
)

# Plot A. grouped bar chart
ggplot(risk.outcome_df2, aes(x = reorder(Risk_outcome, -Count), y = Count, fill = Source_of_risk)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = "Concluded Risk Outcome of the Sources of Risk",
    x = "Risk Outcome",
    y = "Number of Individuals",
    fill = "Source of Risk"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "green"),
                    labels = c("Others, known to individual", "Others, not known to individual", "Service Provider")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), vjust = -0.4) 

# Plot B - Pie chart
risk.outcome_df %>% 
  ggplot(aes(x = "", y = Total, fill = Risk_outcome))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  blank_theme +
  labs(title = "Risk Assessment Outcome",
       fill = "Key") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "green"),
                    labels = c("Risk reduced", "Risk remained", "Risk removed")) +
  theme(axis.text.x=element_blank(),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom",  # Better for horizontal slides
        legend.key.size = unit(0.7, "cm"),
        axis.text = element_text(size = 12)) +
  geom_label(aes(label = paste0(round((Total / sum(Total)) * 100, digits = 2), "%")),
             position = position_stack(vjust = 0.5), show.legend = FALSE)


#--------------------SECTION 3: Mental Capacity Table---------------------------

# A. Mental Capacity Ability

# Import the data
mc_df <- read_excel("mental_capacity_tables.xlsx", sheet = 1)
dim(mc_df)

# Reshape data to long format for grouped bar chart
mc_df2 <- pivot_longer(
  mc_df,
  cols = c(Yes_they_lacked_capacity, No_they_did_not_lack_capacity, Do_not_know, Support_provided_when_they_lacked_capacity),
  names_to = "Risk_of_lacking_capacity",
  values_to = "Count"
)


# Plot grouped bar chart
ggplot(mc_df2, aes(x = AgeBand, y = Count, fill = Risk_of_lacking_capacity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = "Mental Capacity to Make Decisions Related to the Safeguarding Enquiry",
    x = "Age Band",
    y = "Number of Individuals",
    fill = "Key"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "purple", "darkgreen"),
                    labels = c("Not known", "No, they did not lack capacity", "Support provided when they lacked capacity", 
                               "Yes, they lacked capacity")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), vjust = -0.4) 

#-------------------------------------------------------------------------------

# B. Support Provided for Mental Capacity

# Import the data
mc2_df <- read_excel("mental_capacity_tables.xlsx", sheet = 2)
dim(mc2_df)

# Reshape data to long format for grouped bar chart
mc2_df2 <- pivot_longer(
  mc2_df,
  cols = c(Yes_they_lacked_capacity, Support_provided_when_they_lacked_capacity),
  names_to = "Risk_of_lacking_capacity",
  values_to = "Count") %>% 
  mutate(
    Label = ifelse(Risk_of_lacking_capacity == "Support_provided_when_they_lacked_capacity",
                   paste0(round(Percentage, 0), "%"), "") # show percentage only on purple bars
  )


# Plot grouped bar chart
ggplot(mc2_df2, aes(x = AgeBand, y = Count, fill = Risk_of_lacking_capacity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = "Support Involvement in 'Yes' Enquiries: Advocate, Family, or Friend Participation",
    x = "Age Band",
    y = "Number of Individuals",
    fill = "Key"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("purple", "darkgreen"),
                    labels = c("Support provided when they lacked capacity", 
                               "Yes, they lacked capacity")) +
  geom_text(aes(label = Label),
            position = position_dodge(width = 0.6), vjust = -0.4) 


#---------------SECTION 4: Making Safeguarding Personal Tables------------------

# A. Desired Outcome Enquiry

# Import the data
msp_df <- read_excel("msp_tables.xlsx", sheet = 1)
dim(msp_df)

# Reshape data to long format for grouped bar chart
msp_df2 <- pivot_longer(
  msp_df,
  cols = c(Yes_they_were_asked_and_outcomes_were_expressed, Yes_they_were_asked_but_no_outcomes_were_expressed, No, Do_not_know),
  names_to = "Enquiry_desired_outcomes",
  values_to = "Count"
)

# Remove rows where Count is zero and pre-calculate total count per age group.
msp_df2_clean <- msp_df2 %>%
  filter(Count > 0) %>%
  group_by(AgeBand) %>%
  mutate(Total = sum(Count)) %>%
  ungroup() 

# Plot grouped bar chart
ggplot(msp_df2_clean, aes(x = AgeBand, y = Count, fill = Enquiry_desired_outcomes)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = "Were People or Their Representatives Asked What They Wanted to Happen?",
    x = "Age Band",
    y = "Number of Individuals",
    fill = "Key"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "green", "purple"),
                    labels = c("Don't know", "No",
                               "Yes, they were asked and outcomes were expressed", "Yes, they were asked but no outcomes were expressed")) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.6), vjust = -0.4) 

#-------------------------------------------------------------------------------

# B. Desired Outcomes

# Import the data
msp2_df <- read_excel("msp_tables.xlsx", sheet = 2)
dim(msp2_df)

# Reshape data to long format for grouped bar chart
msp2_df2 <- pivot_longer(
  msp2_df,
  cols = c(Fully_achieved, Partially_achieved, Not_achieved),
  names_to = "Desired_outcome_success",
  values_to = "Count"
)

# Remove rows where Count is zero and pre-calculate total count per age group.
msp2_df2_clean <- msp2_df2 %>%
  filter(Count > 0) %>%
  group_by(AgeBand) %>%
  mutate(Total = sum(Count)) %>%
  ungroup()

# Calculate percentages per age band and create label column
msp2_df2_percent <- msp2_df2_clean %>%
  mutate(
    Percent = (Count / Total) * 100,
    Label = paste0(round(Percent, 0), "%")  # Format as whole number percentage
  )

# Plot grouped bar chart
ggplot(msp2_df2_percent, aes(x = AgeBand, y = Percent, fill = Desired_outcome_success)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = "Success Rate of Meeting Desired Outcomes When Views Were Sought",
    x = "Age Band",
    y = "Success Rate (%)",
    fill = "Key"
  ) +
  custom_theme_slide +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "green"),
                    labels = c("Fully achieved", "Not achieved", "Partially achieved")) +
  geom_text(aes(label = Label),
            position = position_dodge(width = 0.6), vjust = -0.4) 

#-------------------------------------------------------------------------------






#ggsave("plot.png", plot = my_plot, width = 1920, height = 1080, units = "px", dpi = 150)

