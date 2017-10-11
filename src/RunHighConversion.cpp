#include <Rcpp.h>
#include <string>
#include <list>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

int gradeToIntRH(std::string grade) {
    if(grade == "Sr")
        return 12;
    else if(grade == "Jr")
        return 11;
    else if(grade == "So")
        return 10;
    else // if(grade == "Fr")
        return 9;
}

//' Modifies the vector to remove any parenthesis numbers (eg "(3)").
//'
//' @param schoolNames The strings to removes numbers from.
//' @export
// [[Rcpp::export]]
void removeDistrictNumber(StringVector schoolNames) {
    // This method removes the district from strings in a vector.
    // Ex: Wilson (3) becomes Wilson

    for(int i = 0; i < schoolNames.size(); i++) {
        bool modified = false;
        std::string school = std::string(schoolNames[i]);

        // Remove the number, parenthesis and trailing spaces.
        while(school.back() == ' ' || school.back() == '('
                  || school.back() == ')' || isdigit(school.back())) {
            school.pop_back();
            modified = true;
        }

        // Update if the string was modified
        if(modified)
            schoolNames[i] = school;
    }
}

//' Interprets text and stores it in a data frame.
//'
//' @param lines The text of the results to convert.
//' @param dfResults A prebuilt data frame to store results into
//' @param hasRaceNumbers TRUE/FALSE the results have race numbers as the first column.
//' @export
// [[Rcpp::export]]
void convertRH(StringVector lines, List dfResults, bool hasRaceNumbers) {
    // Order: Place, Team Place, Name, Grade, MileSplit, Time, Pace, School
    NumericVector places = dfResults["Place"];
    StringVector names = dfResults["Name"];
    NumericVector grades = dfResults["Grade"];
    StringVector schools = dfResults["School"];
    StringVector times = dfResults["Time"];
    StringVector mileSplits = dfResults["MileSplit"];
    StringVector mile2Splits = dfResults["Mile2Split"];
    StringVector paces = dfResults["MilePace"];

    int row = 0;
    for(int i = 0; i < lines.size(); i++)
    {
        // Initialize the string for this runner's line
        std::string line = std::string(lines[i]);

        // Skip over the line if not a runner
        if(line.size() <= 1 || line[0] != ' ')
            continue;

        std::list<std::string> lineList {};
        std::string current = "";

        for(unsigned int posInLine = 0; posInLine < line.length(); posInLine++) {
            // If there are characters to add
            if(line[posInLine] == ' ') {
                if(!current.empty()) {
                    lineList.push_back(current);
                    current = "";
                }
            } else {
                current += line[posInLine];
            }
        }
        if(hasRaceNumbers) {
            lineList.pop_front();
        }
        // lineList now contains
        // [Place, (Team Place), First Name, Last Name, Grade, (MilePlace, MileSplit), Time, Pace, School]
        // The items in parenthesis might not be present.

    // Place
        // std::istringstream(lineList.front()) >> places[row];
        places[row] = std::stoi(lineList.front());
        lineList.pop_front();


        // Skip Team place if present
        if(isdigit(lineList.front()[0]))
            lineList.pop_front();


    // Name
        std::string name = lineList.front();
        lineList.pop_front();
        // until the next character is a number (for grade)
        std::string el = lineList.front();
        bool previousWasEnd = false;
        bool noGrade = false;
        while(!previousWasEnd)
        {
            name += ' ';
            name += el;
            lineList.pop_front();
            previousWasEnd = el[el.length() - 1] == ',';
            el = lineList.front();
            if(isdigit(lineList.front()[0])) { 
                noGrade = true;
                break;
            }
        }
        name.pop_back(); // Remove trailing comma
        names[row] = name;

    // Grade
        if(noGrade) {
            grades[row] = 9;
        } else {
            grades[row] = gradeToIntRH(lineList.front());
            lineList.pop_front();
        }


    // School
        std::string schoolName = lineList.back();
        lineList.pop_back();
        // Combine all words in the school name into one string
        while(!isdigit(lineList.back()[0])) {
            schoolName = lineList.back() + ' ' + schoolName;
            lineList.pop_back();
        }
        schools[row] = schoolName;


    // Pace
        paces[row] = lineList.back();
        lineList.pop_back();


    // Time
        times[row] = lineList.back();
        lineList.pop_back();


    // 1-MileSplit and place
        // Exit this runner if there is no milesplit
        if(lineList.empty()) {
            row++;
            continue;
        }

        // Skip the mile place if included.
        if(lineList.front().find(":") == std::string::npos) {
            lineList.pop_front();
        }
        // Add the mile split if present
        if(!lineList.empty() && lineList.front().find(":") != std::string::npos) {
            mileSplits[row] = lineList.front();
            lineList.pop_front();
        }

    // 2-MileSplit and place
        // Exit this runner if there is no 2-milesplit
        if(lineList.empty()) {
            row++;
            continue;
        }

        // Skip the 2-mile place if included.
        if(lineList.front().find(":") == std::string::npos) {
            lineList.pop_front();
        }
        // Add the 2-mile split if present
        if(!lineList.empty() && lineList.front().find(":") != std::string::npos) {
            mile2Splits[row] = lineList.front();
            lineList.pop_front();
        }

        row++;
    }
}
