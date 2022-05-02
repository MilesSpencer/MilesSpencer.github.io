library(tidyverse)
library(janitor)
library(lmerTest)


df_student <- read_csv("./uvu_full_student_anon.csv") %>% 
  clean_names() %>% 
  select(c("student_warehouse_entity_uid","academic_year","age","gender","primary_ethnicity_desc",
           "term_cumulative_gpa","state_residency","academic_period_desc"))

df_student <- df_student[!df_student$term_cumulative_gpa %>% is.na(),]

df_student %>% names

c("student_warehouse_entity_uid","academic_year","age","gender","primary_ethnicity_desc",
  "term_cumulative_gpa","state_residency","academic_period_desc")

df_course <- read_csv("../Data_Course_SPENCER/Final_Project/math_full_course_anon.csv") %>% 
  filter(SUBJECT == "MATH" | SUBJECT == "MAT" | SUBJECT == "STAT") %>% 
  clean_names() %>% 
  filter(freeze_type == "EOT") %>% 
  select(c("student_warehouse_entity_uid","academic_year","academic_period","course_identification","subject",
           "instr_warehouse_entity_uid","instruction_method","final_grade","sub_academic_period_desc",
           "repeat_course_ind","course_credits","course_campus_desc","schedule_type_desc","residency","credits_earned"))

c("student_warehouse_entity_uid","academic_year","academic_period","course_identification","subject",
  "instr_warehouse_entity_uid","instruction_method","final_grade","sub_academic_period_desc",
  "repeat_course_ind","course_credits","course_campus_desc","schedule_type_desc","residency")

table(df_course$student_crs_type)

a <- df_course %>% names
b <- df_student %>% names

join_by <- a[a %in% b] #column names in both data frames
rm(a,b)


df_full <- left_join(df_course,df_student,by=join_by) %>% 
  mutate(grade_score = case_when(  #change grades to number values
    final_grade == "A" ~ 4,
    final_grade == "A-" ~ 3.7,
    final_grade == "B+" ~ 3.4,
    final_grade == "B" ~ 3,
    final_grade == "B-" ~ 2.7,
    final_grade == "C+" ~ 2.4,
    final_grade == "C" ~ 2,
    final_grade == "C-" ~ 1.7,
    final_grade == "D+" ~ 1.4,
    final_grade == "D" ~ 1,
    final_grade == "D-" ~ 0.7,
    TRUE ~ 0))

rm(df_course,df_student)


# diff_values <- names(df_full)[which(df_full[2,] != df_full[3,])]
# df_student %>% head(n=10) %>% view()
# df_full %>% head %>% view()

#interesting proportion
sum(df_full$credits_earned,na.rm = TRUE)/sum(df_full$course_credits)

#how many students got the credits they were after
table(df_course$credits_earned == df_course$course_credits)

df_full %>% names
df_full %>% head %>% view

table(df_full$final_grade)
table(df_full$credits_earned)

df_full %>% 
  ggplot(aes(x = final_grade,
             fill = gender))+
  geom_bar()

df_full %>% mutate(pass = case_when(
  credits_earned > 0 ~ "PASSED",
  TRUE ~ "FAILED")) %>% 
  ggplot(aes(x = pass,
             fill = gender)) +
  geom_bar() +
  facet_wrap(~primary_ethnicity_desc,
             scales = "free")

df_full %>% 
  ggplot(aes(x = grade_score)) +
  geom_density() +
  facet_wrap(~state_residency)

df_full %>% mutate(pass = case_when(
  credits_earned > 0 ~ TRUE,
  TRUE ~ FALSE)) %>% 
  ggplot(aes(x = pass,
             fill = gender)) +
  geom_bar() +
  facet_wrap(~academic_year)

df_full %>% 
  ggplot(aes(x = age,
             y = grade_score,
             color = gender)) +
  geom_jitter()

library(easystats)

mod_class <- df_full %>% 
  lmer(data = .,
       formula = grade_score ~ academic_year + gender * age + (1|subject/course_identification))
#nested subject and course_ID - doesn't look for variation between the subject and course_ID

mod_subject <- df_full %>% 
  lmer(data = .,
       formula = grade_score ~ academic_year + age + (1|subject))

summary(mod_class)
summary(mod_subject)

compare_performance(mod_class,mod_subject) 

df_full %>% ggplot(aes(x = age,
                       y = grade_score)) +
  geom_count()

df_full %>% ggplot(aes(x = grade_score)) +
  geom_density()

#lmer(score~year+gender+(1|CourseID)) 
#year & gender are fixed
#courseid is a randomized as the intercept
#also check out subject 


cor(df_full$inst_gpa,df_full$quality_points,use = "pairwise.complete.obs")

df_full %>% names