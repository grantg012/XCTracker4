#include <Rcpp.h>
#include <string>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

/*
 * NOTE NAMESPACE MUST HAVE exportPattern("^[[:alpha:]]+") TO COMPILE
 * ROXYGEN2 MIGHT DELETE THIS
 */


std::string leadingZero(int value) {
    if(value < 10)
        return "0" + std::to_string(value);
    else
        return std::to_string(value);
}

std::string toTime(int prmSeconds) {
    return "" + std::to_string(prmSeconds / 60) + ":" + leadingZero(prmSeconds % 60);
}

//' Converts seconds to a formatted time, such as "16:26" (Vectorized).
//'
//' @param prmSeconds The amount of seconds to convert.
//' @return The value of prmSeconds in minutes and seconds.
//' @export
//' @useDynLib XCTrackerCpp
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
StringVector toTime(NumericVector prmSeconds) {
    StringVector result(prmSeconds.length());

    for(int i = 0; i < result.length(); i++) {
        result[i] = toTime((int)prmSeconds[i]);
    }

    return result;
}

int gradeToInt(std::string grade) {
    if(grade == "Sr")
        return 12;
    else if(grade == "Jr")
        return 11;
    else if(grade == "So")
        return 10;
    else // if(grade == "Fr")
        return 9;
}

//' Converts year abbreviations ("Sr", etc)to a grade number (12) (Vectorized).
//'
//' @param grades The grade abbreviations to convert
//' @return Integer(s) for each grade.
//' @export
// [[Rcpp::export]]
IntegerVector gradeToInt(StringVector grades) {
    IntegerVector numbers(grades.length());

    for(int i = 0; i < grades.length(); i++)
        numbers[i] = gradeToInt(as<std::string>(grades[i]));

    return numbers;
}

int inSeconds(std::string raceTime) {
    std::string minutes = "", seconds = "";
    unsigned int i = 0;
    for(; i < raceTime.length(); i++)
        if(raceTime[i] == '.' || raceTime[i] == ':')
            break;
        else
            minutes += raceTime[i];

    for(i++; i < raceTime.length(); i++)
        if(raceTime[i] == '.' || raceTime[i] == ':')
            break;
        else
            seconds += raceTime[i];

    return std::stoi(minutes) * 60 + std::stoi(seconds);
}

//' Converts a formatted time to its value in seconds (Vectorized).
//'
//' @param raceTimes The formatted times (ex: "16:26") to convert
//' @return The seconds value for each time
//' @export
// [[Rcpp::export]]
NumericVector inSeconds(StringVector raceTimes) {
    NumericVector numbers(raceTimes.length());

    for(int i = 0; i < numbers.length(); i++)
        numbers[i] = inSeconds(as<std::string>(raceTimes[i]));

    return numbers;
}

//' Calculates the milepace  (Vectorized).
//'
//' @param raceTime The formatted times (ex: "16:26") to convert.
//' @param distance The single race distance (default 3.1 miles).
//' @return A formatted string of each milepace.
//' @export
// [[Rcpp::export]]
StringVector milePace(StringVector raceTime, double distance = 3.1) {
    return toTime((inSeconds(raceTime) / distance));
}

//' Calculates and formats the difference between two times.
//'
//' @param lowerTime The shorter time.
//' @param higherTime The longer time.
//' @return A formatted string of the difference between the two times.
//' @export
// [[Rcpp::export]]
StringVector timeDifference(CharacterVector lowerTime, CharacterVector higherTime) {

    int shortest = lowerTime.length();
    if(higherTime.length() < shortest) {
        shortest = higherTime.length();
    }
    StringVector result(shortest);

    for(int i = 0; i < result.length(); i++) {
        // Rcout << lowerTime[i] << " " << higherTime[i] << "\n";
        if(CharacterVector::is_na(lowerTime[i]) || CharacterVector::is_na(higherTime[i])) {
            result[i] = NA_STRING;
        } else {
            result[i] = toTime(inSeconds(as<std::string>(higherTime[i])) - inSeconds(as<std::string>(lowerTime[i])));
        }
    }

    return result;
}

//' Sums the first 5 elements of the vector.
//'
//' @param places A vector to sum.
//' @return The sum of the first 5 elements or -1 if less than 5 are present.
//' @export
// [[Rcpp::export]]
int sumOfFive(IntegerVector places) {
    if(places.length() < 5)
        return -1;

    int total = 0;
    for(int i = 0; i < 5; i++) // 0-based
        total += places[i];
    return total;
}

//' Appends word to the prefix "Place."
//'
//' @param word A string of the word to append
//' @return The concatenation of "Place." and word.
//' @export
// [[Rcpp::export]]
std::string placeDot(std::string word) {
    return "Place." + word;
}

//' Appends the first character of word to the prefix "Place."
//'
//' @param word A string of which the first character will be used.
//' @return The concatenation of "Place." and word's first character.
//' @export
// [[Rcpp::export]]
std::string placeDotC(std::string word) {
    std::string result = "Place.";
    result.push_back(word[0]);
    return result;
}

//' Appends word to the prefix "Score."
//'
//' @param word A string of the word to append
//' @return The concatenation of "Score." and word.
//' @export
// [[Rcpp::export]]
std::string scoreDot(std::string word) {
    return "Score." + word;
}

//' Appends the first character of word to the prefix "Score."
//'
//' @param word A string of which the first character will be used.
//' @return The concatenation of "Score." and word's first character.
//' @export
// [[Rcpp::export]]
std::string scoreDotC(std::string word) {
    std::string result = "Score.";
    result.push_back(word[0]);
    return result;
}

//' Appends word to the prefix "Time."
//'
//' @param word A string of the word to append
//' @return The concatenation of "Time." and word.
//' @export
// [[Rcpp::export]]
std::string timeDot(std::string word) {
    return "Time." + word;
}

//' Appends the first character of word to the prefix "Time."
//'
//' @param word A string of which the first character will be used.
//' @return The concatenation of "Time." and word's first character.
//' @export
// [[Rcpp::export]]
std::string timeDotC(std::string word) {
    std::string result = "Time.";
    result.push_back(word[0]);
    return result;
}

