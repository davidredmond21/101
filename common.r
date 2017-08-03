

machine_health_data_path <- function() {
    path <- "C:/MachineHealthData"
    if (!dir.exists(path)) {
        path <- "D:/Users/Tudor/Documents/Tudor/Work/MachineHealthData"
        if (!dir.exists(path)) {
            quit(1)
        }
    }
    
    path
}

dataset_path <- function(i) {
    dataset <- c('1st_test', '2nd_test', '4th_test/txt')
    
    file.path(machine_health_data_path(), 'datasets/nasa', dataset[i])
}

results_path <- function(i) {
    dataset <- c('dataset_1', 'dataset_2', 'dataset_3')
    
    file.path(machine_health_data_path(), 'results/data_mining_and_analysis_book_example', dataset[i])
}

dataset_properties <- function(i) {
    channels <- c(8, 4, 4)
    count <- c(2156, 984, 4448)
    
    c(channels[i], count[i])
}

