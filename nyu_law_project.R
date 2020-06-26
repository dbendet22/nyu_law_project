# Initial project description here:
# https://docs.google.com/document/d/19ie7XA0pikhsqEQk-HdsaRfT-a366SBCINFDd0r_YQo/edit

# CSVs originally pulled from below:
# https://docs.google.com/spreadsheets/d/1DEgul0Q-eH9lOat1O9Krn8KLqI57Jf_BZfWJnJfP3Mw/edit#gid=0
# https://docs.google.com/spreadsheets/d/1x90XNE8JtrpVVXzYJzBb7-PY2eLAUcr8xQTJl1kZd2U/edit#gid=663445164  

library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)
library(scales)
library(lubridate)
library(ggpubr)  
library(reshape2)
library(readr)
library(sentimentr)
library(knitr)
library(formattable)
library(kableExtra)
library(tidytext)
library(textdata)
library(stringr)

fall_survey_result <- read_csv("~/nyu_law_project/data/fall_survey_result.csv")
winter_survey_result <- read_csv("~/nyu_law_project/data/winter_survey_result.csv")
spring_survey_result <- read_csv("~/nyu_law_project/data/spring_survey_result.csv")


# fall survey
# length of thoughts
# sentiment rating of thoughts 
# keywords included in thoughts 

# spring survey
# numerical equivalents of letter grades 

nyuPalette = c("#8900e1", "#FFC107", "#28619e", "#330662", "#3dbbdb", "#f2f2f2", "#6d6d6d",  "#000000", "#220337")

fall_survey_result <- fall_survey_result %>%
                        mutate(response_id = row_number(),
                               # hour_of_day = hour(ymd(Timestamp)),
                               thoughts_length = str_length(`Share your thoughts`), 
                               stress = str_detect(`Share your thoughts`, 'stress'),
                               pressure = str_detect(`Share your thoughts`, 'pressure'),
                               anxiety = str_detect(`Share your thoughts`, 'anxiety'),
                               improve_mental_health = ifelse(str_detect(`Would changing NYU’s grading system improve your mental health?`, 'IDK'), 'IDK', `Would changing NYU’s grading system improve your mental health?`))


# library(formattable)
# df <- data.frame(
#   id = 1:10,
#   name = c("Bob", "Ashley", "James", "David", "Jenny", 
#            "Hans", "Leo", "John", "Emily", "Lee"), 
#   age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
#   grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
#   Test.number.1.score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
#   test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.9, 9.3, 9.1, 8.6),
#   final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
#   registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
#   stringsAsFactors = FALSE)
# 
# formattable(df, list(
#   age = color_tile("white", "orange"),
#   grade = formatter("span", style = x ~ ifelse(x == "A", 
#                                                style(color = "green", font.weight = "bold"), NA)),
#   area(col = c(Test.number.1.score, test2_score)) ~ normalize_bar("pink", 0.2),
#   final_score = formatter("span",
#                           style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
#                           x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
#   registered = formatter("span",
#                          style = x ~ style(color = ifelse(x, "green", "red")),
#                          x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
# ))
# https://stackoverflow.com/questions/40162306/equal-column-widths-on-r-formattable


# 80% of fall respondents said changing the grading system would improve their mental health
# people were most likely to give thoughts at all if they said no, followed closely behind yes, and then IDK)

# fixedWidth = 500

# formattable(fall_survey_result %>% 
#               group_by(ImproveMentalHealth = improve_mental_health) %>% 
#               summarise("Responses" = n()), 
#             align =c("l","r","c","c","c", "c", "c", "c", "r"), 
#             # list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold", width = "500px"))
#             list(
#               `Indicator Name` = formatter(.tag = "span", style = function(x) style(display = "inline-block",
#                                                                                        direction = "rtl", 
#                                                                                        `border-radius` = "4px", 
#                                                                                        `padding-right` = "2px", 
#                                                                                        `background-color` = csscolor("pink"), 
#                                                                                         width = paste(fixedWidth*proportion(x), "px", sep="")))
#               
#             ))

fall_survey_result %>% 
  group_by("Would changing NYU’s grading system improve your mental health?" = improve_mental_health) %>% 
  summarise("Responses" = n()) %>%
  arrange(desc(Responses)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("stripedd", "hover"), full_width = F, position = "left", font_size = 15, fixed_thead = T) %>%
  column_spec(1, bold = T, width = "35em", border_right = F) %>%
  column_spec(2, bold = T, width = "20em") %>% 
  row_spec(1:1, background = "#f2f2f2") %>% 
  footnote(general = "",
           general_title = "Responses from Fall 2019 NYU Law Survey",
           footnote_as_chunk = T, 
           title_format = c("italic"))

# http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
  

fall_survey_result %>% 
  group_by(improve_mental_health) %>% 
  summarise(responses = n(), 
            gave_thoughts_pct = sum(ifelse(is.na(`Share your thoughts`), 0, 1)) / n(),
            thoughts_length = mean(thoughts_length, na.rm = TRUE)) %>% 
  ggplot(aes(improve_mental_health, round(responses,digits = 0), label = responses)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, fill='#8900e1') +
  # geom_col(fill="black") +  
  # scale_x_discrete(labels=labs) +
  scale_x_discrete() +
  scale_y_continuous("Responses", expand = c(0,0), limits = c(0,102)) +
  ggtitle("Would changing NYU's grading system improve your mental health?") +   
  scale_fill_manual(values=nyuPalette) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  geom_text(aes(label = responses, y = responses + 1.1), position = position_dodge(0.9), vjust = 0, size = 5) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 17),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  # facet_wrap(~sentiment, scales = "free_y") +
  # facet_wrap(~sentiment) +
  # labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()


# a couple interesting responses from those who said no:
# "I've heard anecdotally that, in law schools without letter grading systems, there becomes significant pressure to network with faculty members. In those schools, because students are less able to distinguish themselves through grades, they resort to other ways of trying to distinguish themselves, such as through recommendations. To me, this suggests that the specific grading system employed by NYU is not the issue; the issue is that legal opportunities are inherently limited and law students are strongly motivated to compete for them."
# "The pressure on grades comes from external sources: jobs and other professional opportunities. Changing the grading only would have those external forces adapt and to expect whatever the equivalent of an A is. And, the broader a grade band is (ie. like High Pass at Harvard), the more opportunity there is for traditionally powerful communities (white men) to advocate for themselves and stand out over minority students. The problem is law school, not NYU grades. Do not change the system."

winter_survey_result <- winter_survey_result %>% mutate(response_id = row_number())

winter_survey_result_agree_disagree <- winter_survey_result %>%
  mutate(feel_confident_in_course_material = ifelse(str_detect(`I feel confident in the course material`, 'Agree'), 'Agree', 
                                           ifelse(str_detect(`I feel confident in the course material`, 'Disagree'), 'Disagree', 
                                                  'Neutral')),
         rewarding_effort_participation = ifelse(str_detect(`System of Grading is fair in terms of rewarding effort and participation`, 'Agree'), 'Agree', 
                                            ifelse(str_detect(`System of Grading is fair in terms of rewarding effort and participation`, 'Disagree'), 'Disagree', 
                                                   'Neutral')),
         grades_reflect_knowledge_of_material = ifelse(str_detect(`in terms of accurately reflecting knowledge of course material`, 'Agree'), 'Agree', 
                                     ifelse(str_detect(`in terms of accurately reflecting knowledge of course material`, 'Disagree'), 'Disagree', 
                                            'Neutral')),         
         equalize_playing_field = ifelse(str_detect(`in terms of equalizing the playing field`, 'Agree'), 'Agree', 
                                       ifelse(str_detect(`in terms of equalizing the playing field`, 'Disagree'), 'Disagree', 
                                              'Neutral')),
         important_for_career_path = ifelse(str_detect(`Grades are important for your career path`, 'Agree'), 'Agree', 
                                                   ifelse(str_detect(`Grades are important for your career path`, 'Disagree'), 'Disagree', 
                                                          'Neutral')),         
         career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate?`)) %>% 
  select(response_id, 
         feel_confident_in_course_material, 
         rewarding_effort_participation, 
         grades_reflect_knowledge_of_material, 
         equalize_playing_field, 
         important_for_career_path,
         career_plans
  )

keycol <- "response_id"
valuecol <- "count"
gathercols <- c('feel_confident_in_course_material', 
                'rewarding_effort_participation', 
                'grades_reflect_knowledge_of_material', 
                'equalize_playing_field', 
                'important_for_career_path')

winter_by_answer_long <- gather(winter_survey_result_agree_disagree, keycol, valuecol, gathercols, factor_key=TRUE)

nyuPalette = c("#8900e1", "#FFC107", "#28619e", "#330662", "#3dbbdb", "#f2f2f2", "#6d6d6d",  "#000000", "#220337")

winter_by_answer_long %>%
  group_by(keycol) %>%
  summarise(pct_agree = sum(ifelse(valuecol == 'Agree', 1, 0), na.rm = TRUE) / n()) %>%
  ggplot(aes(keycol, pct_agree)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, fill='#8900e1') +
  scale_x_discrete(labels = c("I feel confident in the course material",
                              "Grading system is fair in terms of rewarding effort and participation", 
                              "Grading system accurately reflects knowledge of course material",
                              "Grading system equalizes the playing field",
                              "Grades are important for your career path")) +
  scale_y_continuous("% Agree", labels = percent, expand = c(0,0), limits = c(0,1)) +
  ggtitle("Most students in the winter survey agree that grades are important to their careers but they disagree \nstrongly that grades equalize the playing feel, reflect knowledge of course material, or rewaerd effort/participation.") +   
  # scale_fill_manual(values=nyuPalette) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  geom_text(aes(label = percent(pct_agree, digits = 0), y = pct_agree - 0.04), vjust = 0, size = 5) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm"),
    legend.key = element_rect(colour = "white")) +
  # facet_wrap(~sentiment, scales = "free_y") +
  # facet_wrap(~sentiment) +
  # labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()



















letter_grades <- c('A+', 'A', 'A-', 'B+', 'B', 'B-', 'C', 'D', 'F')
civ_pro_grades <- c(4.333, 4.000, 3.667, 3.333, 3.000, 2.667, 2.000, 1.000, 0.000)
grade_mapping <- data.frame(letter_grades, civ_pro_grades)
spring_survey_result <- spring_survey_result %>% 
  left_join(grade_mapping, by = c("What grade did you receive in Civil Procedure?" = "letter_grades"))

contracts_grades <- c(4.333, 4.000, 3.667, 3.333, 3.000, 2.667, 2.000, 1.000, 0.000)
grade_mapping <- data.frame(letter_grades, contracts_grades)
spring_survey_result <- spring_survey_result %>% 
  left_join(grade_mapping, by = c("What grade did you receive in Contracts?" = "letter_grades"))

crimlaw_grades <- c(4.333, 4.000, 3.667, 3.333, 3.000, 2.667, 2.000, 1.000, 0.000)
grade_mapping <- data.frame(letter_grades, crimlaw_grades)
spring_survey_result <- spring_survey_result %>% 
  left_join(grade_mapping, by = c("What grade did you receive in Criminal Law/Torts?" = "letter_grades"))


spring_survey_result <- spring_survey_result %>% mutate(response_id = row_number())

spring_survey_result_agree_disagree <- spring_survey_result %>%
  mutate(grade_expectations_match = ifelse(str_detect(`Did your expectations match up with the grades you received?`, 'Agree'), 'Agree', 
                                           ifelse(str_detect(`Did your expectations match up with the grades you received?`, 'Disagree'), 'Disagree', 
                                                  'Neutral')),
         grade_understanding_match = ifelse(str_detect(`Do you feel the grades received match your understanding of the course material?`, 'Agree'), 'Agree', 
                                           ifelse(str_detect(`Do you feel the grades received match your understanding of the course material?`, 'Disagree'), 'Disagree', 
                                                  'Neutral')),
         grade_effort_match = ifelse(str_detect(`Do you feel the grades received reflect the level of effort you put in last semester?`, 'Agree'), 'Agree', 
                                            ifelse(str_detect(`Do you feel the grades received reflect the level of effort you put in last semester?`, 'Disagree'), 'Disagree', 
                                                   'Neutral')),         
         grade_arbitrary_feel = ifelse(str_detect(`Do the grades you received feel arbitrary?`, 'Agree'), 'Agree', 
                                     ifelse(str_detect(`Do the grades you received feel arbitrary?`, 'Disagree'), 'Disagree', 
                                            'Neutral')),
         grade_different_system_warranted = ifelse(str_detect(`Do you feel a different grading system is warranted?`, 'Agree'), 'Agree', 
                                       ifelse(str_detect(`Do you feel a different grading system is warranted?`, 'Disagree'), 'Disagree', 
                                              'Neutral')),         
         prefer_pass_fail = ifelse(str_detect(`Would you prefer a Pass/Fail grading system for 1L?`, 'Agree'), 'Agree', 
                                                   ifelse(str_detect(`Would you prefer a Pass/Fail grading system for 1L?`, 'Disagree'), 'Disagree', 
                                                          'Neutral')),
         change_future_plans = ifelse(str_detect(`Do the grades you received change your future plans (internship, career etc.)`, 'Agree'), 'Agree', 
                                   ifelse(str_detect(`Do the grades you received change your future plans (internship, career etc.)`, 'Disagree'), 'Disagree', 
                                          'Neutral')), 
         career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`)) %>% 
  select(response_id, 
         grade_expectations_match, 
         grade_understanding_match, 
         grade_effort_match, 
         grade_arbitrary_feel, 
         grade_different_system_warranted,
         prefer_pass_fail,
         change_future_plans, 
         career_plans,
         class_year = `What class year are you?`,
         racial_ethnic_identity = `What is your racial/ethnic identity?`,
         gender_identity = `What is your gender identity?`,
         first_generations_professional = `Are you a first-generation professional?`,
         lgbtq_plus_identity = `Do you identify as LGBTQ+`,
         disability_identify = `Do you identify as a person with a disability?`
         )






keycol <- "response_id"
valuecol <- "count"
gathercols <- c("racial_ethnic_identity", "gender_identity","first_generations_professional", "lgbtq_plus_identity", "disability_identify") 
spring_by_answer_long <- gather(spring_survey_result_agree_disagree, keycol, valuecol, gathercols, factor_key=TRUE)


spring_by_answer_long %>%
  filter(keycol == 'racial_ethnic_identity') %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  spread(valuecol, count) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T, position = "left", font_size = 15, fixed_thead = T)

spring_by_answer_long %>%
  filter(keycol == 'gender_identity') %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  spread(valuecol, count) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T, position = "left", font_size = 15, fixed_thead = T)

spring_by_answer_long %>%
  filter(keycol == 'first_generations_professional') %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  spread(valuecol, count) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T, position = "left", font_size = 15, fixed_thead = T)

spring_by_answer_long %>%
  filter(keycol == 'lgbtq_plus_identity') %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  spread(valuecol, count) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T, position = "left", font_size = 15, fixed_thead = T)

spring_by_answer_long %>%
  filter(keycol == 'disability_identify') %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  spread(valuecol, count) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T, position = "left", font_size = 15, fixed_thead = T)


spring_by_answer_long %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  # filter(first_generations_professional %in% c('Yes', 'No')) %>%
  ggplot(aes(valuecol, count, group = valuecol, fill = valuecol)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  scale_x_discrete() +
  scale_y_continuous("# Students Responded", labels = comma ) +
  ggtitle("Student Responses by Identity") +   
  # scale_fill_manual(values=nyuPalette) +
  facet_wrap(~keycol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=15),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 15),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


keycol <- "response_id"
valuecol <- "count"
gathercols <- c('grade_expectations_match', 
                'grade_understanding_match', 
                'grade_effort_match', 
                'grade_arbitrary_feel', 
                'grade_different_system_warranted',
                'prefer_pass_fail',
                'change_future_plans',
                'career_plans',
                'class_year')

spring_by_answer_long <- gather(spring_survey_result_agree_disagree, keycol, valuecol, gathercols, factor_key=TRUE)

spring_by_answer_long %>%
  filter(keycol %in% c('grade_expectations_match', 'grade_understanding_match', 'grade_effort_match', 'grade_arbitrary_feel', 'grade_different_system_warranted', 'prefer_pass_fail', 'change_future_plans')) %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  spread(valuecol, count) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T, position = "left", font_size = 15, fixed_thead = T)

spring_by_answer_long %>%
  filter(keycol %in% c('career_plans')) %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  spread(valuecol, count) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T, position = "left", font_size = 15, fixed_thead = T)

spring_by_answer_long %>%
  filter(keycol %in% c('career_plans')) %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  spread(valuecol, count) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T, position = "left", font_size = 15, fixed_thead = T)


spring_by_answer_long %>%
  group_by(keycol, valuecol) %>%
  summarise(count = n()) %>%
  # filter(first_generations_professional %in% c('Yes', 'No')) %>%
  ggplot(aes(valuecol, count, group = valuecol, fill = valuecol)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  scale_x_discrete() +
  scale_y_continuous("# Students Responded", labels = comma ) +
  ggtitle("Student Responses by Other Characteristics") +   
  # scale_fill_manual(values=nyuPalette) +
  facet_wrap(~keycol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=15),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 15),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


keycol <- "response_id"
valuecol <- "count"
gathercols <- c('grade_expectations_match', 
                'grade_understanding_match', 
                'grade_effort_match', 
                'grade_arbitrary_feel', 
                'grade_different_system_warranted',
                'prefer_pass_fail'
                # 'change_future_plans'
                # 'career_plans'
                # 'What class year are you?'
                # 'What is your racial/ethnic identcity?',
                # 'What is your gender identity?',
                # 'Are you a first-generation professional?'
                # 'Do you identify as LGBTQ+',
                # 'Do you identify as a person with a disability?'
                ) 
  
# c('Did your expectations match up with the grades you received?',
#               'Do you feel the grades received match your understanding of the course material?',
#               'Do you feel the grades received reflect the level of effort you put in last semester?', 
#               'Do the grades you received feel arbitrary?', 
#               'Do you feel a different grading system is warranted?', 
#               'Do the grades you received change your future plans (internship, career etc.)', 
#               
#               'Would you prefer the proposed grading system to the grading system NYU Law currently has?', 
#               'Would you prefer a Pass/Fail grading system for 1L?'
#               
#               # 'What do you plan on doing after you graduate? (feel free to select multiple)', 
#               # 'How do you think the proposed grading system would affect your mental health?', 
#               # 'How does the grading system at NYU affect your mental health? Explain:', 
                # 'What class year are you?',
                # 'What is your racial/ethnic identcity?',
                # 'What is your gender identity?',
                # 'Are you a first-generation professional?'
                # 'Do you identify as LGBTQ+',
                # 'Do you identify as a person with a disability?',
#               # 'civ_pro_grades',
#               # 'crimlaw_grades', 
#               # 'contracts_grades')
#               )

spring_by_answer_long <- gather(spring_survey_result_agree_disagree, keycol, valuecol, gathercols, factor_key=TRUE)

# View(response_sentiment_by_answer_long %>% arrange(response_id))
nyuPalette = c("#8900e1", "#FFC107", "#28619e", "#330662", "#3dbbdb", "#f2f2f2", "#6d6d6d",  "#000000", "#220337")

spring_by_answer_long %>%
  group_by(keycol) %>%
  summarise(pct_agree = sum(ifelse(valuecol == 'Agree', 1, 0)) / n()) %>%
  ggplot(aes(keycol, pct_agree)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, fill='#8900e1') +
  scale_x_discrete(labels = c("Do your grades match your expectations?",
                              "Do your grades match your understanding of course material?", 
                              "Do your grades match your effort level?",
                              "Do your grades feel arbitrary?",
                              "Do you think a different grading system is warrented?",
                              "Would you prefer a pass fail grading system for 1L?")) +
  scale_y_continuous("% Agree", labels = percent, expand = c(0,0), limits = c(0,1)) +
  ggtitle("The majority of students surveyed feel that grades seem arbitrary and do not match their understanding or effort level.") +   
  # scale_fill_manual(values=nyuPalette) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  geom_text(aes(label = percent(pct_agree, digits = 0), y = pct_agree - 0.04), vjust = 0, size = 5) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm"),
    legend.key = element_rect(colour = "white")) +
  # facet_wrap(~sentiment, scales = "free_y") +
  # facet_wrap(~sentiment) +
  # labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()



gathercols <- c('grade_expectations_match', 
                'grade_understanding_match', 
                'grade_effort_match', 
                'grade_arbitrary_feel', 
                'grade_different_system_warranted',
                # 'prefer_pass_fail',
                'change_future_plans'
                # 'What is your racial/ethnic identcity?',
                # 'What is your gender identity?',
                # 'Are you a first-generation professional?'
                # 'Do you identify as LGBTQ+',
                # 'Do you identify as a person with a disability?'
) 

keycol <- c('response_id', 'class_year', 'racial_ethnic_identity', 'gender_identity', 'first_generations_professional', 'lgbtq_plus_identity', 'disability_identify', 'career_plans')
valuecol <- "count"


spring_by_answer_long <- gather(spring_survey_result_agree_disagree, keycol, valuecol, gathercols, factor_key=TRUE)

spring_by_answer_long %>%
    group_by(keycol, racial_ethnic_identity) %>%
    summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
    filter(racial_ethnic_identity %in% c('Asian', 'White', 'Black', 'Latinex/Hispanic')) %>%
    filter(keycol == 'grade_understanding_match') %>%
    ggplot(aes(racial_ethnic_identity, count, group = racial_ethnic_identity, fill = racial_ethnic_identity)) +
    geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
    scale_x_discrete(expand = c(0.2, 0.2), labels=c("Asian", "Black", "Latinx/Hispanic", "White")) +
    scale_y_continuous("% Agree", labels = percent, expand = c(0,0), limits = c(0,1)) +
    ggtitle("While all racial identities disagreed strongly that grades matched their understanding of material, black students were \ndisproportionately in disagreement with this statement.") +   
    scale_fill_manual(values=nyuPalette) +
    geom_text(aes(label = percent(count, digits = 0), y = count + 0.025), vjust = 0, size = 5) +
    labs(caption = "Note: Out of the 180 person survey, 17 students identified as black, 19 identified as Asian, 15 identified as Latinx/Hispanic, \n117 identified as white.") +
    # facet_wrap(~keycol) +
    theme_bw() +
    theme(
      plot.title.position = "plot",
      text = element_text(size=17),
      axis.line = element_line(size = .2),
      axis.ticks = element_blank(),
      plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
      panel.border = element_rect(colour = "white"),
      panel.grid = element_line(size=0.3),
      axis.title.y = element_text(size = rel(0)),
      axis.title.x = element_text(vjust=-.75, size = 17),
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.key = element_rect(colour = "white"),
      plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
      plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
      coord_flip()

spring_by_answer_long %>%
  group_by(keycol, racial_ethnic_identity) %>%
  summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
  filter(racial_ethnic_identity %in% c('Asian', 'White', 'Black', 'Latinex/Hispanic')) %>%
  filter(keycol == 'grade_different_system_warranted') %>% 
  ggplot(aes(racial_ethnic_identity, count, group = racial_ethnic_identity, fill = racial_ethnic_identity)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = percent(count, digits = 0), y = count + 0.025), vjust = 0, size = 5) +
  scale_x_discrete(labels=c("Asian", "Black", "Latinx/Hispanic", "White")) +
  scale_y_continuous("% Agree", labels = percent, expand = c(0,0), limits = c(0,1)) +
  ggtitle("Despite feeling like grades did not match their understanding of course material, students identifying as black were \nless likely to say a different grading system is warranted") +   
  scale_fill_manual(values=nyuPalette) +
  labs(caption = "Note: Out of the 180 person survey, 17 students identified as black, 19 identified as Asian, 15 identified as Latinx/Hispanic, \n117 identified as white.") +
  # facet_wrap(~keycol) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()

spring_by_answer_long %>%
  group_by(keycol, gender_identity) %>%
  summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
  filter(gender_identity %in% c('Female', 'Male')) %>%
  filter(keycol == 'grade_understanding_match') %>%
  ggplot(aes(gender_identity, count, group = gender_identity, fill = gender_identity)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  scale_x_discrete() +
  scale_y_continuous("% Agree", labels = percent, expand = c(0,0), limits = c(0,1)) +
  ggtitle("Students identifying as female were much less likekly to agree that grades matched their understanding of course material") +   
  geom_text(aes(label = percent(count, digits = 0), y = count + 0.025), vjust = 0, size = 5) +
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~keycol) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 14),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()

spring_by_answer_long %>%
  group_by(keycol, gender_identity) %>%
  summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
  filter(gender_identity %in% c('Female', 'Male')) %>%
  filter(keycol == 'grade_different_system_warranted') %>%
  ggplot(aes(gender_identity, count, group = gender_identity, fill = gender_identity)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  scale_x_discrete() +
  scale_y_continuous("% Agree", labels = percent, expand = c(0,0)) +
  ggtitle("Female students were slightly less likely than males to agree that a different grading system would be warranted") +   
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~keycol) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()

spring_by_answer_long %>%
  group_by(keycol, gender_identity) %>%
  summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
  filter(gender_identity %in% c('Female', 'Male', 'Non-binary', 'Prefer Not to Answer')) %>%
  ggplot(aes(gender_identity, count, group = gender_identity, fill = gender_identity)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  scale_x_discrete() +
  scale_y_continuous("% Agree", labels = percent, ) +
  ggtitle("Student Agreement With Each Survey Question, by Gender Identity") +   
  scale_fill_manual(values=nyuPalette) +
  facet_wrap(~keycol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


# spring_by_answer_long %>%
#   group_by(keycol, first_generations_professional) %>%
#   summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
#   filter(first_generations_professional %in% c('Yes', 'No')) %>%
#   ggplot(aes(first_generations_professional, count, group = first_generations_professional, fill = first_generations_professional)) +
#   geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
#   scale_x_discrete() +
#   scale_y_continuous("% Agree", labels = percent, ) +
#   ggtitle("Student Agreement With Each Survey Question, by First Generation Professional") +   
#   scale_fill_manual(values=nyuPalette) +
#   facet_wrap(~keycol) +
#   # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
#   theme_bw() +
#   theme(
#     plot.title.position = "plot",
#     text = element_text(size=12),
#     axis.line = element_line(size = .2),
#     axis.ticks = element_blank(),
#     plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
#     panel.border = element_rect(colour = "white"),
#     panel.grid = element_line(size=0.3),
#     axis.title.y = element_text(size = rel(0)),
#     axis.title.x = element_text(vjust=-.75, size = 10),
#     legend.position = "bottom",
#     legend.title=element_blank(),
#     legend.key = element_rect(colour = "white")) +
#   coord_flip()

spring_by_answer_long %>%
  group_by(keycol, first_generations_professional) %>%
  summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
  filter(first_generations_professional %in% c('Yes', 'No')) %>%
  filter(keycol == "grade_effort_match") %>%
  ggplot(aes(first_generations_professional, count, group = first_generations_professional, fill = first_generations_professional)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  scale_x_discrete() +
  scale_y_continuous("% Agree", labels = percent, limits = c(0,1)) +
  ggtitle("First Generation Professionals were more likely to agree that grades matched their effort level") +   
  geom_text(aes(label = percent(count, digits = 0), y = count + 0.025), vjust = 0, size = 5) +
  # labs(caption = "Note: Out of the 180 person survey, 11 students identified as having a disability.") +
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~keycol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"), 
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()


# spring_by_answer_long %>%
#   group_by(keycol, lgbtq_plus_identity) %>%
#   summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
#   filter(lgbtq_plus_identity %in% c('Yes', 'No')) %>%
#   ggplot(aes(lgbtq_plus_identity, count, group = lgbtq_plus_identity, fill = lgbtq_plus_identity)) +
#   geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
#   scale_x_discrete() +
#   scale_y_continuous("% Agree", labels = percent, ) +
#   ggtitle("Student Agreement With Each Survey Question, by LGBTQ+ Identity") +   
#   scale_fill_manual(values=nyuPalette) +
#   facet_wrap(~keycol) +
#   # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
#   theme_bw() +
#   theme(
#     plot.title.position = "plot",
#     text = element_text(size=12),
#     axis.line = element_line(size = .2),
#     axis.ticks = element_blank(),
#     plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
#     panel.border = element_rect(colour = "white"),
#     panel.grid = element_line(size=0.3),
#     axis.title.y = element_text(size = rel(0)),
#     axis.title.x = element_text(vjust=-.75, size = 10),
#     legend.position = "bottom",
#     legend.title=element_blank(),
#     legend.key = element_rect(colour = "white")) +
#   coord_flip()
  

spring_by_answer_long %>%
  group_by(keycol, lgbtq_plus_identity) %>%
  summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
  filter(lgbtq_plus_identity %in% c('Yes', 'No')) %>%
  filter(keycol == "grade_effort_match") %>%
  ggplot(aes(lgbtq_plus_identity, count, group = lgbtq_plus_identity, fill = lgbtq_plus_identity)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  scale_x_discrete() +
  scale_y_continuous("% Agree", labels = percent, limits = c(0,1)) +
  ggtitle("Students identifying as LGBTQ+ were slightly less likely to agree that grades matched their effort level") +   
  geom_text(aes(label = percent(count, digits = 0), y = count + 0.025), vjust = 0, size = 5) +
  # labs(caption = "Note: Out of the 180 person survey, 11 students identified as having a disability.") +
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~keycol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"), 
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()



spring_by_answer_long %>%
  group_by(keycol, disability_identify) %>%
  summarise(count = sum(ifelse(valuecol == 'Agree', 1, 0)) / n() ) %>%
  filter(disability_identify %in% c('No', 'Yes')) %>%
  filter(keycol == "grade_effort_match") %>%
  ggplot(aes(disability_identify, count, group = disability_identify, fill = disability_identify)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  scale_x_discrete() +
  scale_y_continuous("% Agree", labels = percent, limits = c(0,1)) +
  ggtitle("Students with a disability were much less likely to agree that grades matched their effort level") +   
  geom_text(aes(label = percent(count, digits = 0), y = count + 0.025), vjust = 0, size = 5) +
  labs(caption = "Note: Out of the 180 person survey, 11 students identified as having a disability.") +
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~keycol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"), 
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()


spring_survey_result_numerical_agreement <- spring_survey_result %>%
  mutate(grade_expectations_match = ifelse(gsub(",.*$", "", `Did your expectations match up with the grades you received?`) == 'Strongly Agree', 5, 
                                      ifelse(gsub(",.*$", "", `Did your expectations match up with the grades you received?`) == 'Agree', 4,
                                        ifelse(gsub(",.*$", "", `Did your expectations match up with the grades you received?`) == 'Neutral', 3,
                                          ifelse(gsub(",.*$", "", `Did your expectations match up with the grades you received?`) == 'Disagree', 2,
                                                  1)))),
         grade_understanding_match = ifelse(gsub(",.*$", "", `Do you feel the grades received match your understanding of the course material?`) == 'Strongly Agree', 5, 
                                           ifelse(gsub(",.*$", "", `Do you feel the grades received match your understanding of the course material?`) == 'Agree', 4,
                                                  ifelse(gsub(",.*$", "", `Do you feel the grades received match your understanding of the course material?`) == 'Neutral', 3,
                                                         ifelse(gsub(",.*$", "", `Do you feel the grades received match your understanding of the course material?`) == 'Disagree', 2,
                                                                1)))),

         grade_effort_match = ifelse(gsub(",.*$", "", `Do you feel the grades received reflect the level of effort you put in last semester?`) == 'Strongly Agree', 5, 
                                            ifelse(gsub(",.*$", "", `Do you feel the grades received reflect the level of effort you put in last semester?`) == 'Agree', 4,
                                                   ifelse(gsub(",.*$", "", `Do you feel the grades received reflect the level of effort you put in last semester?`) == 'Neutral', 3,
                                                          ifelse(gsub(",.*$", "", `Do you feel the grades received reflect the level of effort you put in last semester?`) == 'Disagree', 2,
                                                                 1)))),         

         grade_arbitrary_feel = ifelse(gsub(",.*$", "", `Do the grades you received feel arbitrary?`) == 'Strongly Agree', 5, 
                                     ifelse(gsub(",.*$", "", `Do the grades you received feel arbitrary?`) == 'Agree', 4,
                                            ifelse(gsub(",.*$", "", `Do the grades you received feel arbitrary?`) == 'Neutral', 3,
                                                   ifelse(gsub(",.*$", "", `Do the grades you received feel arbitrary?`) == 'Disagree', 2,
                                                          1)))),
         
         grade_different_system_warranted = ifelse(gsub(",.*$", "", `Do you feel a different grading system is warranted?`) == 'Strongly Agree', 5, 
                                       ifelse(gsub(",.*$", "", `Do you feel a different grading system is warranted?`) == 'Agree', 4,
                                              ifelse(gsub(",.*$", "", `Do you feel a different grading system is warranted?`) == 'Neutral', 3,
                                                     ifelse(gsub(",.*$", "", `Do you feel a different grading system is warranted?`) == 'Disagree', 2,
                                                            1)))),         
         
         prefer_pass_fail = ifelse(gsub(",.*$", "", `Would you prefer a Pass/Fail grading system for 1L?`) == 'Strongly Agree', 5, 
                                                   ifelse(gsub(",.*$", "", `Would you prefer a Pass/Fail grading system for 1L?`) == 'Agree', 4,
                                                          ifelse(gsub(",.*$", "", `Would you prefer a Pass/Fail grading system for 1L?`) == 'Neutral', 3,
                                                                 ifelse(gsub(",.*$", "", `Would you prefer a Pass/Fail grading system for 1L?`) == 'Disagree', 2,
                                                                        1)))),

         change_future_plans = ifelse(gsub(",.*$", "", `Do the grades you received change your future plans (internship, career etc.)`) == 'Strongly Agree', 5, 
                                   ifelse(gsub(",.*$", "", `Do the grades you received change your future plans (internship, career etc.)`) == 'Agree', 4,
                                          ifelse(gsub(",.*$", "", `Do the grades you received change your future plans (internship, career etc.)`) == 'Neutral', 3,
                                                 ifelse(gsub(",.*$", "", `Do the grades you received change your future plans (internship, career etc.)`) == 'Disagree', 2,
                                                        1))))) %>% 
  select(response_id, 
         grade_expectations_match, 
         grade_understanding_match, 
         grade_effort_match, 
         grade_arbitrary_feel, 
         grade_different_system_warranted,
         prefer_pass_fail,
         change_future_plans, 
         class_year = `What class year are you?`,
         racial_ethnic_identity = `What is your racial/ethnic identity?`,
         gender_identity = `What is your gender identity?`,
         first_generations_professional = `Are you a first-generation professional?`,
         lgbtq_plus_identity = `Do you identify as LGBTQ+`,
         disability_identify = `Do you identify as a person with a disability?`
  )


keycol <- "response_id"
valuecol <- "count"
gathercols <- c('grade_expectations_match', 
                'grade_understanding_match', 
                'grade_effort_match', 
                'grade_arbitrary_feel', 
                'grade_different_system_warranted',
                'prefer_pass_fail',
                'change_future_plans'
                # 'What class year are you?'
                # 'What is your racial/ethnic identcity?',
                # 'What is your gender identity?',
                # 'Are you a first-generation professional?'
                # 'Do you identify as LGBTQ+',
                # 'Do you identify as a person with a disability?'
) 

spring_by_answer_numerical_long <- gather(spring_survey_result_numerical_agreement, keycol, valuecol, gathercols, factor_key=TRUE)

# View(response_sentiment_by_answer_long %>% arrange(response_id))

spring_by_answer_numerical_long %>%
  group_by(keycol) %>%
  summarise(avg = mean(valuecol)) %>%
  ggplot(aes(keycol, avg, group = valuecol, fill = valuecol)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = FALSE) +
  # geom_col(fill="black") +  
  # scale_x_discrete(labels=labs) +
  scale_x_discrete() +
  scale_y_continuous("Avg Agreement Score", labels = comma, expand = c(0,0)) +
  ggtitle("Student Agreement With Each Survey Question") +   
  scale_fill_manual(values=nyuPalette) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  # facet_wrap(~sentiment, scales = "free_y") +
  # facet_wrap(~sentiment) +
  # labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

formattable(spring_by_answer_numerical_long %>%
              group_by(keycol) %>%
              summarise(agreement_score = digits(mean(valuecol), 2)),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))


gathercols <- c('What grade did you receive in Civil Procedure?', 
                'What grade did you receive in Contracts?', 
                'What grade did you receive in Criminal Law/Torts?'
                ) 
keycol <- c('response_id', 
            'class_year', 
            'racial_ethnic_identity', 
            'gender_identity', 
            'first_generations_professional', 
            'lgbtq_plus_identity', 
            'disability_identify',
            'grade_expectations_match', 
            'grade_understanding_match', 
            'grade_effort_match', 
            'grade_arbitrary_feel', 
            'grade_different_system_warranted',
            'prefer_pass_fail',
            'change_future_plans'
            )
valuecol <- "count"

spring_by_grades <- gather(spring_survey_result, keycol, valuecol, gathercols, factor_key=TRUE)

spring_by_grades %>%
  filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  filter(keycol == 'What grade did you receive in Civil Procedure?') %>%
  group_by(valuecol) %>%
  summarise(count = n() / 180) %>%
  ggplot(aes(reorder(valuecol, count), count , group = valuecol, fill = valuecol)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  # geom_col(fill="black") +  
  # scale_x_discrete(labels=labs) +
  scale_x_discrete() +
  scale_y_continuous("Responses", labels = percent, expand = c(0,0)) +
  ggtitle("What grade did you receive in Civil Procedure?") +   
  scale_fill_manual(values=nyuPalette) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  # facet_wrap(~sentiment, scales = "free_y") +
  # facet_wrap(~sentiment) +
  # labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()



formattable(spring_by_grades %>%
              filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              # filter(keycol == 'What grade did you receive in Civil Procedure?') %>%
              group_by(valuecol, keycol) %>%
              summarise(percent_students = digits(n() / 180*100, 1)) %>% 
              spread(keycol, percent_students),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))

target_curve = data.frame(Grade = c('A', 'A-', 'B+', 'B', 'B-'), "Target" = c(11, 20, 26, 37, 6))

spring_by_grades %>%
  filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  # filter(keycol == 'What grade did you receive in Civil Procedure?') %>%
  group_by(valuecol, keycol) %>%
  summarise(percent_students = digits(n() / 180*100, 1)) %>% 
  select("Grade" = valuecol, keycol, percent_students) %>% 
  spread(keycol, percent_students) %>% inner_join(target_curve, by = "Grade") %>%
  arrange(factor(Grade, levels = c("A", "A-", "B+", "B", "B-")) , desc(Grade)) %>% 
  # arrange(desc(Responses)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("stripedd", "hover"), full_width = F, position = "left", font_size = 14, fixed_thead = T) %>%
  column_spec(1, bold = T, border_right = F) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, bold = T) %>%
  column_spec(4, bold = T) %>% 
  column_spec(5, bold = T)
  # row_spec(1:1, background = "#f2f2f2") %>% 
  # footnote(general = "",
  #          general_title = "Responses from Fall 2019 NYU Law Survey",
  #          footnote_as_chunk = T, 
  #          title_format = c("italic"))







spring_by_grades %>%
  filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  filter(keycol == 'What grade did you receive in Contracts?') %>%
  group_by(valuecol) %>%
  summarise(count = n() / 180) %>%
  ggplot(aes(reorder(valuecol, count), count , group = valuecol, fill = valuecol)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  # geom_col(fill="black") +  
  # scale_x_discrete(labels=labs) +
  scale_x_discrete() +
  scale_y_continuous("Responses", labels = percent, expand = c(0,0)) +
  ggtitle("What grade did you receive in Contracts?") +   
  scale_fill_manual(values=nyuPalette) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  # facet_wrap(~sentiment, scales = "free_y") +
  # facet_wrap(~sentiment) +
  # labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

spring_by_grades %>%
  filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  filter(keycol == 'What grade did you receive in Criminal Law/Torts?') %>%
  group_by(valuecol) %>%
  summarise(count = n() / 180) %>%
  ggplot(aes(reorder(valuecol, count), count , group = valuecol, fill = valuecol)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  # geom_col(fill="black") +  
  # scale_x_discrete(labels=labs) +
  scale_x_discrete() +
  scale_y_continuous("Responses", labels = percent, expand = c(0,0)) +
  ggtitle("What grade did you receive in Criminal Law/Torts?") +   
  scale_fill_manual(values=nyuPalette) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  # facet_wrap(~sentiment, scales = "free_y") +
  # facet_wrap(~sentiment) +
  # labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()



spring_survey_result <- read_csv("nyu_law_project/data/spring_survey_result.csv")

letter_grades <- c('A+', 'A', 'A-', 'B+', 'B', 'B-', 'C', 'D', 'F')
civ_pro_grades <- c(4.333, 4.000, 3.667, 3.333, 3.000, 2.667, 2.000, 1.000, 0.000)
grade_mapping <- data.frame(letter_grades, civ_pro_grades)
spring_survey_result <- spring_survey_result %>% 
  left_join(grade_mapping, by = c("What grade did you receive in Civil Procedure?" = "letter_grades"))

contracts_grades <- c(4.333, 4.000, 3.667, 3.333, 3.000, 2.667, 2.000, 1.000, 0.000)
grade_mapping <- data.frame(letter_grades, contracts_grades)
spring_survey_result <- spring_survey_result %>% 
  left_join(grade_mapping, by = c("What grade did you receive in Contracts?" = "letter_grades"))

crimlaw_grades <- c(4.333, 4.000, 3.667, 3.333, 3.000, 2.667, 2.000, 1.000, 0.000)
grade_mapping <- data.frame(letter_grades, crimlaw_grades)
spring_survey_result <- spring_survey_result %>% 
  left_join(grade_mapping, by = c("What grade did you receive in Criminal Law/Torts?" = "letter_grades"))


spring_survey_result <- spring_survey_result %>% mutate(response_id = row_number())
# is there any overlap in respondents between the fall and spring surveys?

formattable(spring_survey_result %>%
              # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              mutate(
                gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
                class_year = gsub(",.*$", "", `What class year are you?`),
                racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
                gender_identity = gsub(",.*$", "", `What is your gender identity?`),
                first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
                lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
                disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
              group_by(racial_ethnic_identity) %>%
              summarise(gpa = digits(sum(gpa, na.rm=TRUE) / n(),2), 
                        count = n()) %>%
              filter(count > 5) %>% 
              select(racial_ethnic_identity, gpa),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))

medians <- spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  select(racial_ethnic_identity, gpa) %>% 
  group_by(racial_ethnic_identity) %>%
  summarise(gpa = median(gpa, na.rm = TRUE)) %>% 
  filter(racial_ethnic_identity %in% c("White", "Asian", "Latinex/Hispanic", "Black")) 

spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  select(racial_ethnic_identity, gpa) %>% 
  # group_by(racial_ethnic_identity) %>%
  # summarise(gpa = sum(gpa, na.rm=TRUE) / n(),
            # count = n()) %>%
  filter(racial_ethnic_identity %in% c("White", "Asian", "Latinex/Hispanic", "Black")) %>%
  ggplot(aes(reorder(racial_ethnic_identity, gpa), gpa , group = racial_ethnic_identity, colour = racial_ethnic_identity)) +
  # geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  geom_boxplot(aes(colour = racial_ethnic_identity)) +
  scale_x_discrete(labels=c("Black", "Latinx/Hispanic", "Asian", "White")) +
  scale_y_continuous("GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by Racial/Ethnic Identity for students surveyed in the spring semester") +   
  scale_fill_manual(values=nyuPalette) +
  geom_text(data = medians, aes(label = digits(gpa,2), y = gpa), vjust = -5, size = 5) +
  labs(caption = "Note: Out of the 180 person survey, 17 students identified as black, 19 identified as Asian, 15 identified as Latinx/Hispanic, \n117 identified as white.") +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()

medians <- spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  select(gender_identity, gpa) %>% 
  group_by(gender_identity) %>%
  summarise(gpa = median(gpa, na.rm = TRUE)) %>% 
  filter(gender_identity %in% c("Male", "Female", "Non-binary"))

spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  select(gender_identity, gpa) %>% 
  # group_by(racial_ethnic_identity) %>%
  # summarise(gpa = sum(gpa, na.rm=TRUE) / n(), 
  # count = n()) %>%
  filter(gender_identity %in% c("Male", "Female", "Non-binary")) %>%
  ggplot(aes(reorder(gender_identity, gpa), gpa , group = gender_identity, colour = gender_identity)) +
  # geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  geom_boxplot(aes(colour = gender_identity)) +
  scale_x_discrete() +
  scale_y_continuous("GPA", labels = comma, expand = c(0,0)) +
  geom_text(data = medians, aes(label = digits(gpa,2), y = gpa), vjust = -6, size = 5) +
  labs(caption = "Note: Out of the 180 person survey, 5 students identified as non-binary.") +
  ggtitle("GPA by Gender Identity for students surveyed in the spring semester") +   
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()

medians <- spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  filter(gender_identity %in% c("Male", "Female")) %>%
  filter(racial_ethnic_identity %in% c("White", "Asian", "Latinex/Hispanic", "Black")) %>%
  select(race_gender, gpa) %>% 
  group_by(race_gender) %>%
  summarise(gpa = median(gpa, na.rm = TRUE))
  

spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  filter(gender_identity %in% c("Male", "Female")) %>%
  filter(racial_ethnic_identity %in% c("White", "Asian", "Latinex/Hispanic", "Black")) %>%
  select(race_gender, gpa) %>%
  ggplot(aes(reorder(race_gender, gpa), gpa , group = race_gender, colour = race_gender)) +
  # geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  geom_boxplot(aes(colour = race_gender)) +
  scale_x_discrete(labels = c("Black--Female", "Black--Male", "Latinx/Hispanic--Female", "Latinx/Hispanic--Male", "Asian--Male", "Asian--Female", "White--Male", "White--Female")) +
  labs(caption = "Note: Out of the 180 person survey, 17 students identified as black, 19 identified as Asian, 15 identified as \nLatinx/Hispanic, 117 identified as white.") +
  scale_y_continuous("GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by Racial & Gender Identity for students surveyed in the spring semester") +   
  geom_text(data = medians, aes(label = digits(gpa,2), y = gpa), hjust = -0.21, size = 3) +
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()

medians <- spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  filter(disability_identify %in% c("Yes", "No")) %>%
  select(disability_identify, gpa) %>% 
  group_by(disability_identify) %>%
  summarise(gpa = median(gpa, na.rm = TRUE))

spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  filter(disability_identify %in% c("Yes", "No")) %>%
  select(disability_identify, gpa) %>%
  ggplot(aes(reorder(disability_identify, gpa), gpa , group = disability_identify, colour = disability_identify)) +
  # geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  geom_boxplot(aes(colour = disability_identify)) +
  scale_x_discrete() +
  scale_y_continuous("GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by having a disability for students surveyed in the spring semester") +   
  geom_text(data = medians, aes(label = digits(gpa,2), y = gpa), vjust = -9, size = 5) +
  labs(caption = "Note: Out of the 180 person survey, 11 students identified as having a disability.") +
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()



medians <- spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  filter(career_plans %in% c("Private Law", "Public Interest/Government")) %>%
  select(career_plans, gpa) %>% 
  group_by(career_plans) %>%
  summarise(gpa = median(gpa, na.rm = TRUE))


spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  filter(career_plans %in% c("Private Law", "Public Interest/Government")) %>%
  select(career_plans, gpa) %>%
  ggplot(aes(reorder(career_plans, gpa), gpa , group = career_plans, colour = career_plans)) +
  # geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  geom_boxplot(aes(colour = career_plans)) +
  scale_x_discrete() +
  scale_y_continuous("GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by Career Plans for students surveyed in the spring semester") +   
  scale_fill_manual(values=nyuPalette) +
  geom_text(data = medians, aes(label = digits(gpa,2), y = gpa), vjust = -9, size = 5) +
  # labs(caption = "Note: Out of the 180 person survey, 11 students identified as having a disability.") +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()


medians <- spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`),
    diff_system = gsub(",.*$", "", `Do you feel a different grading system is warranted?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  # filter(diff_system %in% c("Private Law", "Public Interest/Government")) %>%
  select(diff_system, gpa) %>% 
  group_by(diff_system) %>%
  summarise(gpa = median(gpa, na.rm = TRUE))

spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`),
    diff_system = gsub(",.*$", "", `Do you feel a different grading system is warranted?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  # filter(diff_system %in% c("Private Law", "Public Interest/Government")) %>%
  select(diff_system, gpa) %>%
  mutate(diff_system = factor(diff_system, levels = c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree"))) %>% 
  ggplot(aes(diff_system, gpa, group = diff_system, colour = diff_system)) +
  # geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  geom_boxplot(aes(colour = diff_system)) +
  scale_x_discrete() +
  scale_y_continuous("GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by desire for a different grading system for students surveyed in the spring semester") +   
  scale_fill_manual(values=nyuPalette) +
  geom_text(data = medians, aes(label = digits(gpa,2), y = gpa), vjust = -4, size = 5) +
  # labs(caption = "Note: Out of the 180 person survey, 11 students identified as having a disability.") +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()


medians <- spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`),
    diff_system = gsub(",.*$", "", `Do you feel a different grading system is warranted?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  # filter(diff_system %in% c("Private Law", "Public Interest/Government")) %>%
  select(first_generations_professional, gpa) %>% 
  group_by(first_generations_professional) %>%
  summarise(gpa = median(gpa, na.rm = TRUE)) %>% 
  filter(first_generations_professional %in% c("Yes", "No"))

spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`),
    diff_system = gsub(",.*$", "", `Do you feel a different grading system is warranted?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  filter(first_generations_professional %in% c("Yes", "No")) %>%
  select(first_generations_professional, gpa) %>%
  arrange(factor(first_generations_professional, levels = c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree"))) %>% 
  ggplot(aes(first_generations_professional, gpa , group = first_generations_professional, colour = first_generations_professional)) +
  # geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  geom_boxplot(aes(colour = first_generations_professional)) +
  scale_x_discrete() +
  scale_y_continuous("GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by First Generation Professional") +   
  scale_fill_manual(values=nyuPalette) +
  geom_text(data = medians, aes(label = digits(gpa,2), y = gpa), vjust = -9, size = 5) +
  # labs(caption = "Note: Out of the 180 person survey, 11 students identified as having a disability.") +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()


medians <- spring_survey_result %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`),
    diff_system = gsub(",.*$", "", `Do you feel a different grading system is warranted?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  # filter(diff_system %in% c("Private Law", "Public Interest/Government")) %>%
  select(lgbtq_plus_identity, gpa) %>% 
  group_by(lgbtq_plus_identity) %>%
  summarise(gpa = median(gpa, na.rm = TRUE)) %>% 
  filter(lgbtq_plus_identity %in% c("Yes", "No"))

spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    class_year = gsub(",.*$", "", `What class year are you?`),
    racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
    career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`),
    diff_system = gsub(",.*$", "", `Do you feel a different grading system is warranted?`),
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  filter(lgbtq_plus_identity %in% c("Yes", "No")) %>%
  select(lgbtq_plus_identity, gpa) %>%
  arrange(factor(lgbtq_plus_identity, levels = c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disgree"))) %>% 
  ggplot(aes(lgbtq_plus_identity, gpa , group = lgbtq_plus_identity, colour = lgbtq_plus_identity)) +
  # geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  geom_boxplot(aes(colour = lgbtq_plus_identity)) +
  scale_x_discrete() +
  scale_y_continuous("GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by LGBTQ+ Identity") +   
  scale_fill_manual(values=nyuPalette) +
  geom_text(data = medians, aes(label = digits(gpa,2), y = gpa), vjust = -9, size = 5) +
  # labs(caption = "Note: Out of the 180 person survey, 11 students identified as having a disability.") +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=17),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 15),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 17),
    legend.position = "none",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"),
    plot.caption = element_text(hjust = 0, vjust = -3, face="italic", colour="#696969"),
    plot.margin=unit(c(0.5, 1, 1, 0.5),"cm")) +
  coord_flip()


formattable(spring_survey_result %>%
              # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              mutate(
                gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
                class_year = gsub(",.*$", "", `What class year are you?`),
                racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
                gender_identity = gsub(",.*$", "", `What is your gender identity?`),
                first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
                lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
                disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
              group_by(gender_identity) %>%
              summarise(gpa = digits(sum(gpa, na.rm=TRUE) / n(),2), 
                        count = n()) %>%
              filter(count > 5) %>% 
              select(gender_identity, gpa),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))


spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    gender_identity = gsub(",.*$", "", `What is your gender identity?`),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  group_by(gender_identity) %>%
  summarise(gpa = sum(gpa, na.rm=TRUE) / n(), 
            count = n()) %>%
  filter(count > 5) %>% 
  ggplot(aes(reorder(gender_identity, gpa), gpa , group = gender_identity, fill = gender_identity)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  scale_x_discrete() +
  scale_y_continuous("Avg GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by Gender Identity") +   
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


formattable(spring_survey_result %>%
              # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              mutate(
                gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
                class_year = gsub(",.*$", "", `What class year are you?`),
                racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
                race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
                gender_identity = gsub(",.*$", "", `What is your gender identity?`),
                first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
                lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
                disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
              group_by(race_gender) %>%
              summarise(gpa = digits(sum(gpa, na.rm=TRUE) / n(),2), 
                        count = n()) %>%
              filter(count > 5) %>% 
              select(race_gender, gpa),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))

spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  group_by(race_gender) %>%
  summarise(gpa = sum(gpa, na.rm=TRUE) / n(), 
            count = n()) %>%
  filter(count > 5) %>% 
  ggplot(aes(reorder(race_gender, gpa), gpa , group = race_gender, fill = race_gender)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  scale_x_discrete() +
  scale_y_continuous("Avg GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by Gender & Race Identity") +   
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


formattable(spring_survey_result %>%
              # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              mutate(
                gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
                class_year = gsub(",.*$", "", `What class year are you?`),
                racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
                race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
                gender_identity = gsub(",.*$", "", `What is your gender identity?`),
                first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
                lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
                disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
              group_by(first_generations_professional) %>%
              summarise(gpa = digits(sum(gpa, na.rm=TRUE) / n(),2), 
                        count = n()) %>%
              filter(count > 5) %>% 
              select(first_generations_professional, gpa),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))

spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  group_by(first_generations_professional) %>%
  summarise(gpa = sum(gpa, na.rm=TRUE) / n(), 
            count = n()) %>%
  filter(count > 5) %>% 
  ggplot(aes(reorder(first_generations_professional, gpa), gpa , group = first_generations_professional, fill = first_generations_professional)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  scale_x_discrete() +
  scale_y_continuous("Avg GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by First Generation Professional") +   
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


formattable(spring_survey_result %>%
              # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              mutate(
                gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
                class_year = gsub(",.*$", "", `What class year are you?`),
                racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
                race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
                gender_identity = gsub(",.*$", "", `What is your gender identity?`),
                first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
                lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
                disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
              group_by(lgbtq_plus_identity) %>%
              summarise(gpa = digits(sum(gpa, na.rm=TRUE) / n(),2), 
                        count = n()) %>%
              filter(count > 5) %>% 
              select(lgbtq_plus_identity, gpa),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))


spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  group_by(lgbtq_plus_identity) %>%
  summarise(gpa = sum(gpa, na.rm=TRUE) / n(), 
            count = n()) %>%
  filter(count > 5) %>% 
  ggplot(aes(reorder(lgbtq_plus_identity, gpa), gpa , group = lgbtq_plus_identity, fill = lgbtq_plus_identity)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  scale_x_discrete() +
  scale_y_continuous("Avg GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by LBGTQ+ Identity") +   
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


formattable(spring_survey_result %>%
              # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              mutate(
                gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
                class_year = gsub(",.*$", "", `What class year are you?`),
                racial_ethnic_identity = gsub(",.*$", "", `What is your racial/ethnic identity?`),
                race_gender = paste(gsub(",.*$", "", `What is your racial/ethnic identity?`), gsub(",.*$", "", `What is your gender identity?`), sep = "--"),
                gender_identity = gsub(",.*$", "", `What is your gender identity?`),
                first_generations_professional = gsub(",.*$", "", `Are you a first-generation professional?`),
                lgbtq_plus_identity = gsub(",.*$", "", `Do you identify as LGBTQ+`),
                disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
              group_by(disability_identify) %>%
              summarise(gpa = digits(sum(gpa, na.rm=TRUE) / n(),2), 
                        count = n()) %>%
              filter(count > 5) %>% 
              select(disability_identify, gpa),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))



spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    disability_identify = gsub(",.*$", "", `Do you identify as a person with a disability?`)) %>% 
  group_by(disability_identify) %>%
  summarise(gpa = sum(gpa, na.rm=TRUE) / n(), 
            count = n()) %>%
  filter(count > 5) %>% 
  ggplot(aes(reorder(disability_identify, gpa), gpa , group = disability_identify, fill = disability_identify)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  scale_x_discrete() +
  scale_y_continuous("Avg GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by Disability Identify") +   
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()



formattable(spring_survey_result %>%
              # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              mutate(
                gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
                career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`)) %>% 
              group_by(career_plans) %>%
              summarise(gpa = digits(sum(gpa, na.rm=TRUE) / n(),2), 
                        count = n()) %>%
              filter(count > 5) %>% 
              select(career_plans, gpa),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))


spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`)) %>% 
  group_by(career_plans) %>%
  summarise(gpa = sum(gpa, na.rm=TRUE) / n(), 
            count = n()) %>%
  filter(count > 5) %>% 
  ggplot(aes(reorder(career_plans, gpa), gpa , group = career_plans, fill = career_plans)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  scale_x_discrete() +
  scale_y_continuous("Avg GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by Career Plans") +   
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


formattable(spring_survey_result %>%
              # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              mutate(
                gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
                career_plans = gsub(",.*$", "", `What do you plan on doing after you graduate? (feel free to select multiple)`)) %>% 
              group_by(career_plans) %>%
              summarise(gpa = digits(sum(gpa, na.rm=TRUE) / n(),2), 
                        count = n()) %>%
              filter(count > 5) %>% 
              select(career_plans, gpa),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))


formattable(spring_survey_result %>%
              # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
              mutate(
                gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
                diff_system = gsub(",.*$", "", `Do you feel a different grading system is warranted?`)) %>% 
              group_by(diff_system) %>%
              summarise(gpa = digits(sum(gpa, na.rm=TRUE) / n(),2), 
                        count = n()) %>%
              filter(count > 5) %>% 
              select(diff_system, gpa),
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey", font.weight = "bold")) 
            ))



spring_survey_result %>%
  # filter(valuecol %in% c('A', 'A-', 'B+', 'B', 'B-')) %>%
  mutate(
    gpa = (civ_pro_grades + contracts_grades + crimlaw_grades) / 3, 
    diff_system = gsub(",.*$", "", `Do you feel a different grading system is warranted?`)) %>% 
  group_by(diff_system) %>%
  summarise(gpa = sum(gpa, na.rm=TRUE) / n(), 
            count = n()) %>%
  filter(count > 5) %>% 
  ggplot(aes(reorder(diff_system, gpa), gpa , group = diff_system, fill = diff_system)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE, position = "dodge") +
  scale_x_discrete() +
  scale_y_continuous("Avg GPA", labels = comma, expand = c(0,0)) +
  ggtitle("GPA by Wanting Different System") +   
  scale_fill_manual(values=nyuPalette) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()









gathercols <- c('class_year',
                'racial_ethnic_identity',
                'gender_identity',
                'first_generations_professional',
                'lgbtq_plus_identity',
                'disability_identify') 

keycol <- c('response_id', 'career_plans')
valuecol <- "count"


spring_by_answer_long <- gather(spring_survey_result_agree_disagree, keycol, valuecol, gathercols, factor_key=TRUE)

spring_by_answer_long %>%
  filter(keycol == 'racial_ethnic_identity') %>%
  filter(valuecol %in% c('Asian', 'White', 'Black', 'Latinex/Hispanic', 'White, Latinex/Hispanic')) %>%
  group_by(valuecol, career_plans) %>%
  summarise(count = n()) %>%
  filter(career_plans %in% c('Public Interest/Government', 'Private Law')) %>%
  ggplot(aes(valuecol, count, group = career_plans, fill = career_plans)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE) +
  scale_x_discrete() +
  scale_y_continuous("Responses", labels = comma) +
  ggtitle("Career Plans by Racial Identity") +   
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~valuecol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


spring_by_answer_long %>%
  filter(keycol == 'gender_identity') %>%
  # filter(valuecol %in% c('Asian', 'White', 'Black', 'Latinex/Hispanic', 'White, Latinex/Hispanic')) %>%
  group_by(valuecol, career_plans) %>%
  summarise(count = n()) %>%
  filter(career_plans %in% c('Public Interest/Government', 'Private Law')) %>%
  ggplot(aes(valuecol, count, group = career_plans, fill = career_plans)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE) +
  scale_x_discrete() +
  scale_y_continuous("Responses", labels = comma) +
  ggtitle("Career Plans by Gender Identity") +   
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~valuecol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()


spring_by_answer_long %>%
  filter(keycol == 'first_generations_professional') %>%
  # filter(valuecol %in% c('Asian', 'White', 'Black', 'Latinex/Hispanic', 'White, Latinex/Hispanic')) %>%
  group_by(valuecol, career_plans) %>%
  summarise(count = n()) %>%
  filter(career_plans %in% c('Public Interest/Government', 'Private Law')) %>%
  ggplot(aes(valuecol, count, group = career_plans, fill = career_plans)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE) +
  scale_x_discrete() +
  scale_y_continuous("Responses", labels = comma) +
  ggtitle("Career Plans by First Generations Professional") +   
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~valuecol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()

spring_by_answer_long %>%
  filter(keycol == 'lgbtq_plus_identity') %>%
  # filter(valuecol %in% c('Asian', 'White', 'Black', 'Latinex/Hispanic', 'White, Latinex/Hispanic')) %>%
  group_by(valuecol, career_plans) %>%
  summarise(count = n()) %>%
  filter(career_plans %in% c('Public Interest/Government', 'Private Law')) %>%
  ggplot(aes(valuecol, count, group = career_plans, fill = career_plans)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE) +
  scale_x_discrete() +
  scale_y_continuous("Responses", labels = comma) +
  ggtitle("Career Plans by LGBTQ+ Identity") +   
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~valuecol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()

spring_by_answer_long %>%
  filter(keycol == 'disability_identify') %>%
  # filter(valuecol %in% c('Asian', 'White', 'Black', 'Latinex/Hispanic', 'White, Latinex/Hispanic')) %>%
  group_by(valuecol, career_plans) %>%
  summarise(count = n()) %>%
  filter(career_plans %in% c('Public Interest/Government', 'Private Law')) %>%
  ggplot(aes(valuecol, count, group = career_plans, fill = career_plans)) +
  geom_bar(alpha = 1.0, stat = "identity", show.legend = TRUE) +
  scale_x_discrete() +
  scale_y_continuous("Responses", labels = comma) +
  ggtitle("Career Plans by Disability Identity") +   
  scale_fill_manual(values=nyuPalette) +
  # facet_wrap(~valuecol) +
  # geom_text(size = 4, position = position_stack(vjust=1.05)) + 
  theme_bw() +
  theme(
    plot.title.position = "plot",
    text = element_text(size=12),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    plot.title=element_text(vjust=0, hjust=0, face="bold", size = 12),
    panel.border = element_rect(colour = "white"),
    panel.grid = element_line(size=0.3),
    axis.title.y = element_text(size = rel(0)),
    axis.title.x = element_text(vjust=-.75, size = 10),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white")) +
  coord_flip()










# sentiment analysis fwiw (not much)

# https://www.datacamp.com/community/tutorials/sentiment-analysis-R#lexiconsandlyrics

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

word_tb <- fall_survey_result %>% filter(thoughts_length > 0) %>%
  unnest_tokens(word, `Share your thoughts`) 

word_tb <- word_tb %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment))

response_sentiment <- word_tb %>%
  group_by(response_id) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(response_id, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         response_id = factor(response_id)) %>%
  drop_na(response_id)

response_sentiment_by_answer <- word_tb %>%
  group_by(`Would changing NYU’s grading system improve your mental health?`) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(`Would changing NYU’s grading system improve your mental health?`, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         `Would changing NYU’s grading system improve your mental health?` = factor(`Would changing NYU’s grading system improve your mental health?`)) %>%
  drop_na(`Would changing NYU’s grading system improve your mental health?`)

response_sentiment %>%
  ggplot(aes(response_id, sentiment, fill = response_id)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE)

keycol <- "`Would changing NYU’s grading system improve your mental health?`"
valuecol <- "words"
gathercols <- c("negative", "positive")
response_sentiment_by_answer_long <- gather(response_sentiment_by_answer, keycol, valuecol, gathercols, factor_key=TRUE)

response_sentiment_by_answer_long %>%
  ggplot(aes(`Would changing NYU’s grading system improve your mental health?`, valuecol, group = keycol, fill = keycol)) +
  geom_bar(alpha = 0.5, stat = "identity", position = "dodge", show.legend = TRUE)

bing_word_counts <- word_tb %>%
  drop_na(response_id) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  # facet_wrap(~sentiment, scales = "free_y") +
  facet_wrap(~sentiment) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()


sentence_tb <- fall_survey_result %>% filter(thoughts_length > 0) %>%
  unnest_tokens(sentence, `Share your thoughts`, token = "sentences") 

sentence_sentiment <- sentence_tb %>%
  group_by(response_id) %>%
  mutate(sentence_num = 1:n(),
         index = round(sentence_num / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(response_id, index) %>%
  summarise(sentiment = sum(value, na.rm = TRUE)) %>%
  arrange(desc(sentiment))

sentence_tb <- word_tb %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment))



