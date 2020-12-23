# BUSINESS UNDERSTANDING ----

# Libraries:

library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)


# Load Data:

path_train <- "00_Data/telco_train.xlsx"
train_raw_tbl <- read_excel(path_train, sheet = 1)


# Data Subset:

dpt_job_role_tbl <- train_raw_tbl %>% 
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)


# 1. Business Science Problem Framework ----

# 1.A View Business As Machine ----


# BSU'S Department and Job Role:
# Define objectives: Retrain High Performers:
# Assess Outcomes:

  dpt_job_role_tbl %>% 
    group_by(Attrition) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(pct = n / sum(n))
  

# 1.B Understand The Drivers ----

# Investigate Objectives: 16 PCT Attrition
# Synthesize Outcomes:
# Hypothesize Drivers: Job Role and  Departments

# Department
dpt_job_role_tbl %>% 
  group_by(Department, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(Department) %>% 
  mutate(pct = n / sum(n))

# Job Role
dpt_job_role_tbl %>% 
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  filter(Attrition %in% "Yes") %>% 
  arrange(desc(pct))


# 1C. Measure The Drivers ----

# Collect Information on Employee Attrition: 
# Develop KPI's: Industry KPIs: 8.8% 

dpt_job_role_tbl %>% 
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  filter(Attrition %in% "Yes") %>% 
  arrange(desc(pct)) %>% 
  mutate(above_industry_avg = case_when(
  pct > 0.088 ~ "Yes",
  TRUE ~ "No"
    )
  )

# 1D. Uncover Problems and Opportunities ----

# Function to calculate attrition cost within a mutate() function
calculate_attrition_cost <- function(
  
  # Employee
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
  
) {
  
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year * 
    (workdays_position_open + workdays_onboarding * onboarding_efficiency) 
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
}


# Calculate cost by job role ----

dpt_job_role_tbl %>% 
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  filter(Attrition %in% "Yes") %>% 
  arrange(desc(pct)) %>% 
  mutate(above_industry_avg = case_when(
    pct > 0.088 ~ "Yes",
    TRUE ~ "No"
  )
  ) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n= n, salary = 80000)
  )

# Workfllow of Attrition ----


count_to_pct <- function(data, ..., col = n) {
  
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  ret <- data %>% 
    group_by(!!!grouping_vars_expr) %>% 
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>% 
    ungroup()
  
  return(ret)
}


dpt_job_role_tbl %>% 
  count(JobRole, Attrition) %>% 
  count_to_pct(JobRole) %>% 
  filter(Attrition %in% "Yes") %>% 
  arrange(desc(pct)) %>% 
  mutate(above_industry_avg = case_when(
    pct > 0.088 ~ "Yes",
    TRUE ~ "No"
  )
  ) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n= n, salary = 80000)
  )


assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
  
  attrition_col_expr <- enquo(attrition_col)
  
  data %>% 
    filter((!! attrition_col_expr) %in% attrition_value) %>% 
    arrange(desc(pct)) %>% 
    mutate(above_industry_avg = case_when(
      pct > baseline_pct ~ "Yes",
      TRUE ~ "No"
    )
    ) %>%
    mutate(
      cost_of_attrition = calculate_attrition_cost(n= n, salary = 80000)
    )
  
  
}


dpt_job_role_tbl %>% 
  count(JobRole, Attrition) %>% 
  count_to_pct(JobRole) %>% 
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088)

  