#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <tuple>

extern Rcpp::IntegerVector iso_week_and_year(int year, int month, int day);

// The main function to implement the logic similar to the R code.
// [[Rcpp::export]]
Rcpp::List group_by_frequency(Rcpp::List vardates, int freq) {
    std::vector<std::vector<int>> idxs(vardates.size());
    int j = 0;
    int date_prev = 0;
    int year_prev = 0;

    // Loop over each date in vardates
    for (size_t i = 0; i < vardates.size(); ++i) {
        Rcpp::S4 date_obj = vardates[i];  // Extract the S4 object
        int year = as<int>(date_obj.slot("Year"));
        int month = as<int>(date_obj.slot("Month"));
        int day = as<int>(date_obj.slot("Day"));
        int date = 0;

        // Compute date based on frequency
        if (freq == 1) {
            date = 1;  // Yearly frequency (1 for January)
        } else if (freq == 2) {
            date = (month <= 6) ? 1 : 2;  // Semi-annual frequency (1st half or 2nd half)
        } else if (freq == 4) {
            date = (month - 1) / 3 + 1;  // Quarterly frequency (1-4 for quarters)
        } else if (freq == 12) {
            date = month;  // Monthly frequency (1-12 for months)
        } else if (freq == 52) {
             // Weekly frequency - use iso_week_and_year to get the ISO week number
            Rcpp::IntegerVector year_day = iso_week_and_year(year, month, day);
            year = year_day[0];
            date = year_day[1];  // Get the week number
        }

        // Check if the year or date has changed to start a new group
        if (i > 0 && (date != date_prev || year != year_prev)) {
            ++j;  // Move to the next group
        }
        idxs[j].push_back(i + 1);  // Add the current index (1-based) to the group
        date_prev = date;
        year_prev = year;
    }

    // Create an Rcpp List to store the grouped indices
    Rcpp::List result(j + 1);
    for (int i = 0; i <= j; ++i) {
        result[i] = Rcpp::wrap(idxs[i]);  // Wrap the C++ vector into an R IntegerVector
    }

    return result;
}