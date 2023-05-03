
library(janitor)

files <- dir("data/", pattern = "[^Ss]*$")
files <- grep('Sources', files, invert = TRUE, value = TRUE)

target_outcomes <- c("Financial skills, know-how & confidence",
                   "Financial skills, know-how & confidence",
                   "Managing expenses",
                   "Managing debt",
                   "Managing savings",
                   "Financial resilience")
names(target_outcomes) <- c("CB1", "CB2", "CB3", "CB4", "CB5", "RESILIENCE")

clean_data <- function(file) {

        # Extract substring between "-" and "."
        to <- str_remove_all(sub(".*-(.*)\\..*", "\\1", file), " ")

        data <- read_csv(paste0("data/", file)) %>%
        select(-`...1`)

        keep <- !is.na(data[[ncol(data)]])
        data <- data[keep, ]

        # Getting column names from first row and applying them to data frame
        colnames <- as.character(as.vector(data[1, ]))
        names(data) <- colnames
        # Dropping colnames from first row and cleaning
        data <- data %>% filter(row_number()>1) %>% clean_names() %>% mutate(target_outcome = target_outcomes[to])

        return(data)
}

questionnaire_data <- bind_rows(map(files, clean_data)) %>%
                    # Cleaning up a few things
                    mutate(
                      type_of_data = ifelse(is.na(type_of_data), "Consumer survey", type_of_data),
                      topic = ifelse(is.na(topic), topics_and_comments, topic),
                      topic = ifelse(is.na(topic), topic_and_comments, topic),
                      sector = ifelse(is.na(sector), "General", sector)
                    ) %>%
                    select(target_outcome, source, sector, objective_subjective = objective_vs_subjective, data_type = type_of_data, questionnaire_item = indicator_question, topic)



