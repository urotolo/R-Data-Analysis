library(ggplot2) # Author - Ulisse Rotolo
library(dplyr)
sheets1_data <- read.csv("/root/RandomDocuments/mozilla_root0/Sheets1.csv")                            # Read in the data.
sheets2_data <- read.csv("/root/RandomDocuments/mozilla_root0/Sheets2.csv")       

# Part 1 - Merge the data from the two sheets (using the merge function) by route so that the
#          distance of each run is included with the data from Sheet 1.
                                                                                                       # Merges the data into a single data frame.                           
sheets3_combined_data <- merge(sheets1_data, sheets2_data, by.x = "route", by.y = "route name")
#sheets3_combined_data["route", drop=FALSE]
#sheets3_combined_data <- merge(sheets1_data, sheets2_data,by.x = c("route"),by.y = c("route name"))
#names(sheets3_combined_data)
#class(sheets3_combined_data$date)

# Part 2 - Convert the date column to R’s Date type using as.Date

sheets3_combined_data$date <- as.Date(sheets3_combined_data$date, format = "%m/%d/%y")                 # Converts the data type of the date column                                    
#class(sheets3_combined_data$date)                                                                     # into R's built in "Date" data type.
pre_fillin_dates <- sheets3_combined_data$date
pre_fillin_dates_chartype <- as.character.Date(pre_fillin_dates)                                       # <--- list of dates in character form pre-update
class(pre_fillin_dates_chartype)

# Part 3 - If any dates are missing, fill in the missing value with reasonable estimates.

for(i in 1:nrow(sheets3_combined_data)){                                                               # Fills in the missing date values with
  if(i >= 2)                                                                                           # reasonable estimates.
  {
    if(is.na(pre_fillin_dates_chartype[i]))  # Checks to see if date slot is empty, if so fill it up with estimated date.
    {
      previous_value = strsplit((pre_fillin_dates_chartype[i-1]), split = '-')  # Day before empty date slot
      next_value = strsplit((pre_fillin_dates_chartype[i+1]), split = '-')      # Day after empty date slot
      if(!is.na(next_value)){                         # if next date is not empty, use this date as a bench mark to estimate the new date
        specific_day = next_value[[1]][3]
        num_specific_day = as.integer(specific_day)
        num_specific_day_modify = num_specific_day - 1 # Subtract one from the successive date, and replace the NA in place of the empty date slot with "successive date minus one"
        estimated_date = paste(next_value[[1]][1],next_value[[1]][2],as.character(num_specific_day_modify),sep='-') # So this for loop uses the date before the NA and after the NA as
      }                                                                                                             # upper and lower bounds to estimate the new date.
      else if(!is.na(previous_value)){
        specific_day = next_value[[1]][3]
        num_specific_day = as.integer(specific_day)
        num_specific_day_modify = num_specific_day + 1
        estimated_date = paste(previous_value[[1]][1], previous_value[[1]][2],as.character(num_specific_day_modify),sep='-')
      }
      pre_fillin_dates_chartype[i] = estimated_date
    }
  }
}

# Part 4 - Create a histogram (using ggplot) of the number of days between successive runs.
# Note- Part of solution to problem 9 is in this for loop below. Pressed for time, so I 
# decided to just sort of re-use this for loop instead of building another more clear way.

total_days_between_successive_runs <- 0
numberof_days_between_dates <- vector()   # <--- (This variable for Part 9), this variable keeps track of amount of days between dates
for(i in 1:106){
  current_date = strsplit(pre_fillin_dates_chartype[i], split = '-')                                  # Calculates days between successive runs
  successive_date = strsplit(pre_fillin_dates_chartype[i+1], split = '-')
  if(successive_date[[1]][3] != "NA" && current_date[[1]][3] != "NA"){                                # This for loop takes the current date and successive date, then subtracts the
    if(successive_date[[1]][2] == current_date[[1]][2]){                                              # current date from the successive date, and saves the remainder in count variable
      successive_date_integer = as.numeric(successive_date[[1]][3])                                   # that tallys up all the iterations of successive date minus current date, until
      current_date_integer = as.numeric(current_date[[1]][3])                                         # the data exhaust and I am left with the total days between dates.
      sinlge_instance_of_daysbetweenruns = successive_date_integer - current_date_integer
      numberof_days_between_dates <- c(numberof_days_between_dates, sinlge_instance_of_daysbetweenruns)  # <--- (This variable for Part 9) I have how many days between each successive run date stored in this variable.
      total_days_between_successive_runs = total_days_between_successive_runs + sinlge_instance_of_daysbetweenruns
    }
    if(successive_date[[1]][2] != current_date[[1]][2]){
      successive_date_integer = as.numeric(successive_date[[1]][3])
      current_date_integer = as.numeric(current_date[[1]][3])
      successive_date_integer = successive_date_integer + 30
      sinlge_instance_of_daysbetweenruns = successive_date_integer - current_date_integer
      total_days_between_successive_runs = total_days_between_successive_runs + sinlge_instance_of_daysbetweenruns
    }
    print(total_days_between_successive_runs)                                                             # Days between successive runs
  }
}

days_ran = 106
days_between_runs = c(total_days_between_successive_runs)
data_ran_vs_not_ran = data.frame(days_ran, days_between_runs)
                                                                                                     # Prints a histogram, one column representing amount of days ran 
ggplot(data_ran_vs_not_ran) + geom_histogram(aes(x=data_ran_vs_not_ran$days_ran),                     # The next column representing amount of days between successive runs.
                                             col="red", alpha=0.3) +
  geom_histogram(aes(x=data_ran_vs_not_ran$days_between_runs),
                 col="blue", 
                 alpha=0.3) + 
  labs(title="Days ran vs Days Relaxed", x="Column of days run vs
                                           Column of days relaxed", y="Count")

# Part 5 - The time is recorded as "minutes.seconds" (i.e. 27.24 means 27 minutes and 24
#          seconds). Convert the data to minutes (as a decimal, so 27.24 should become 27.4
#          because 24 seconds is .4 minutes).

head(sheets3_combined_data)
convert_to_decimal <- function(x){                                                                      # Function that takes a minute.second time value and
  x_as_character = as.character.numeric_version(x)                                                      # converts it to a decimal time value. Below this function,there is a for loop,
  seconds_portion = strsplit(x_as_character,split='')                                                   # however, I wanted to use the apply or lapply function instead of a costly for loop
  seconds_whole = paste(seconds_portion[[1]][3],seconds_portion[[1]][4],seconds_portion[[1]][5],sep="") # to use with my function to convert every value in the time column list to the decimal
  seconds_as_numerical = as.numeric(seconds_whole)                                                      # time format, but ran into issues and due to time constraints had to settle for a
  seconds_in_decimal = seconds_as_numerical * 1.60                                                      # memory intensive for loop :(
  seconds_whole = as.character(seconds_in_decimal)
  split_seconds_whole = strsplit(seconds_whole,split = "")
  seconds_whole = paste(split_seconds_whole[[1]][3],split_seconds_whole[[1]][4],split_seconds_whole[[1]][5],sep="")
  minutes_whole = paste(seconds_portion[[1]][1],seconds_portion[[1]][2],sep="")
  minutes_whole_int= as.integer(minutes_whole)
  final_x_as_character = paste(minutes_whole,seconds_whole,sep=".")
  return(as.numeric(final_x_as_character))
}
unconverted_time_col = sheets3_combined_data$time
for(i in 1:nrow(sheets3_combined_data)){
  if(!is.na(unconverted_time_col[i])){
    unconverted_time_col[i] = convert_to_decimal(unconverted_time_col[i]) # Now converted
  }
}

print(unconverted_time_col)   # <--- To see converted times, from minutes.second to decimal, uncomment this.
                                               
                                                                                                        
# Part 6 - What is the total distance ran in the data set? (Exclude any treadmill runs.)

route_names_list = as.character(sheets3_combined_data$route)
distances_ran_list = sheets3_combined_data$distance..mi.
total_distance_ran = 0                                                        # In miles and excluding treadmill distance
for(i in 1:106){
  if(route_names_list[i] != "treadmill1"){
    total_distance_ran = total_distance_ran + distances_ran_list[i]
  }
}
print(total_distance_ran)                                                       # This line contains total distance ran in miles, excluding treadmill distance, uncomment to see

# Part 7 - Plot the pace per mile (i.e. the pace in minutes and seconds that it took to run a mile)
#  (with date on the x-axis) for each run in 3 different ways:
#   a. In one plot using different colors for different routes.
#   b. Using facet_grid to use separate plots for each route
#   c. One plot showing just the data for runs where the route is “standard”.

run_times_list = as.character(sheets3_combined_data$time)
mile_times_list = vector()
for(i in 1:nrow(sheets3_combined_data)){
  if(!is.na(run_times_list[i])){                                          # Finds pace per mile.
    distance_per_iteration = as.numeric(distances_ran_list[i])
    time_per_iteration = as.numeric(run_times_list[i])                    # Says on this day you ran this average time per mile
    average_mile_time_per_day = time_per_iteration/distance_per_iteration
    mile_times_list = c(mile_times_list, average_mile_time_per_day)
  } else{
    mile_times_list = c(mile_times_list, 7)
  }
}

#print(mile_times_list)                                                    # <---- variable list of mile times for all routes
routes_list = sheets3_combined_data$route
run_dates_list = sheets3_combined_data$date
times_and_distance_frame = data.frame(run_dates_list, mile_times_list, routes_list)

# Part 7A - a. In one plot using different colors for different routes.

ggplot(times_and_distance_frame, aes(x=run_dates_list,y=mile_times_list)) + geom_bar(stat="identity") + ylim(c(1, 20))
                                                                          + geom_point(aes(color = routes_list))
# Part 7B - b. Using facet_grid to use separate plots for each route

ggplot(times_and_distance_frame, aes(x=run_dates_list,y=mile_times_list)) + geom_point(shape=1) + facet_grid(routes_list)

# Part 7C -  c. One plot showing just the data for runs where the route is “standard”.

standard_route_mile_times = vector()
for(i in 1:nrow(sheets3_combined_data)){
  if(!is.na(mile_times_list[i] && routes_list[i] == "standard")){
    standard_route_mile_times = c(standard_route_mile_times, mile_times_list[i])
  } else{
    mile_times_list = c(mile_times_list, 0)
  }
}
times_and_distance_frame2 = data.frame(run_dates_list, mile_times_list, routes_list,standard_route_mile_times)
ggplot(times_and_distance_frame2, aes(x=run_dates_list,y=standard_route_mile_times)) + geom_bar(stat = "identity") + ylim(c(1,20))

# Part 8 - What is the average pace per mile in the data for all runs? How does that compare to the
#          “standard” route runs?

total_mile_times <- 0
for(i in 1:111){
  total_mile_times = total_mile_times + mile_times_list[i]
}
average_pace_per_mile_all_data <- total_mile_times / 111
print(standard_route_mile_times)
# Average mile time for standard route runs

standard_route_mile_times = vector()
for(i in 1:nrow(sheets3_combined_data)){
  if(routes_list[i] == "standard"){
    standard_route_mile_times = c(standard_route_mile_times, mile_times_list[i])
  } else{
    mile_times_list = c(mile_times_list, 0)
  }
}
total_mile_times <- 0
for(i in 1:111){
  total_mile_times = total_mile_times + standard_route_mile_times[i]
}
average_pace_per_mile_standard_data <- total_mile_times/111

print(average_pace_per_mile_all_data)        # <--- This is the average pace per mile for all runs (6.390417)
print(average_pace_per_mile_standard_data)   # <--- This is the average pace per mile for standard runs (10.76753)
                                             # It seems that the standard route comparatively to other routes 
                                             # is definitely on the slower side. I would need to run more test 
                                             # to know if it was the slowest route however. Likely
                                             # what this means is this is the longest route, the route
                                             # with the most terrain. Or it is possible the jogger slows down
                                             # along this route due to personality reasons. Maybe there is a beautiful
                                             # view the jogger routinely checks, or something along those lines that
                                             # slows the jogger down and is something other then the length of terrain.
                                             # However those ideas are just hypothesis, except the data, thats empirical.

# Part 9 - If one more row was included in the data, make your best estimate of what the date
#would be. Assuming that the route is “standard”, what is your best estimate of the time
#(in minutes and seconds) of the run?

#predict(lm, sheets3_combined_data$date)
numberof_days_between_dates # <--- Look to part 4 to see how I got this variable.
                            # (This variable for Part 9) Represents days between each successive date stored in this variable.
                            # I can now take the average of such, and within reason can predict the next date by adding the average
                            # amount of days between each date in the data, to the last date in the data, then the result is predicted
                            # date based on data. I know its not as cool as some advanced machine learning technique but still trying to get the hang of R.

average_days_between_dates <- sum(numberof_days_between_dates)/106  
last_date_given_bydata <- "2019-01-26"
last_day <- strsplit(last_date_given_bydata, split = "-")
new_day <- as.integer(last_day[[1]][3]) + as.integer(average_days_between_dates)
predicted_date <- paste(last_day[[1]][1],last_day[[1]][2],new_day,split="-")
print(predicted_date)      # Predicted next date to be hypothetically added to the date column in the main data frame.

predicted_time_ifstandard_route = sum(standard_route_mile_times) / 464  # Takes average of the mile times from the standard route
predicted_time_ifstandard_route                                         # and uses that average as the predicted next mile time for this
                                                                        # route.
