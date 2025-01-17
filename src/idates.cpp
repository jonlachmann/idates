#include <Rcpp.h>
using namespace Rcpp;

// Helper function to calculate the day of the week (1 = Monday, ..., 7 = Sunday)
// [[Rcpp::export]]
int day_of_week(int year, int month, int day) {
    if (month < 3) {
        year -= 1;
        month += 12;
    }
    int k = year % 100;
    int j = year / 100;
    int weekday = (int) (day + floor((13 * (month + 1)) / 5.0) + k + floor(k / 4.0) + floor(j / 4.0) - 2 * j) % 7;
    return ((weekday + 6) % 7) + 1; // Adjust to ISO (1 = Monday, ..., 7 = Sunday)
}

// Helper function to compute if a year is a leap year
bool is_leap_year(int year) {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

// Precomputed cumulative days up to the start of each month for non-leap and leap years
std::vector<int> cumulative_days_normal = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
std::vector<int> cumulative_days_leap = {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};

// Inline calculation of day of the year
int day_of_year(int year, int month, int day) {
    const std::vector<int>& cumulative_days = is_leap_year(year) ? cumulative_days_leap : cumulative_days_normal;
    return cumulative_days[month - 1] + day;
}

// Main function to calculate ISO week and year
// [[Rcpp::export]]
IntegerVector iso_week_and_year(int year, int month, int day) {
    // Calculate the day of the week for the given date
    int weekday = day_of_week(year, month, day);

    // Find the day of year for the Thursday of the current week
    int days_to_thursday = 4 - weekday;
    int thursday_doy = day_of_year(year, month, day) + days_to_thursday;

    // Adjust for overflow/underflow of day of year
    if (thursday_doy < 1) {
        year -= 1;
        thursday_doy = 365 + is_leap_year(year) + thursday_doy;
    } else if (thursday_doy > 365 + is_leap_year(year)) {
        year += 1;
        thursday_doy = thursday_doy - (365 + is_leap_year(year - 1));
    }

    // Determine the ISO year
    int iso_year = year;

    // Compute the day of the year for January 4th of the ISO year
    int jan4_doy = day_of_year(iso_year, 1, 4);
    int jan4_weekday = day_of_week(iso_year, 1, 4);
    int iso_year_start_doy = jan4_doy - ((jan4_weekday - 1) % 7);

    // Calculate the ISO week number
    int week_number = (thursday_doy - iso_year_start_doy) / 7 + 1;

    // Handle edge cases for week 0 and week 53
    if (week_number == 0) {
        iso_year -= 1;
        jan4_doy = day_of_year(iso_year, 1, 4);
        jan4_weekday = day_of_week(iso_year, 1, 4);
        iso_year_start_doy = jan4_doy - ((jan4_weekday - 1) % 7);
        week_number = (thursday_doy - iso_year_start_doy) / 7 + 1;
    }

    // Return the ISO year and week number as an IntegerVector
    return IntegerVector::create(iso_year, week_number);
}
