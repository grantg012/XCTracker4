#include <Rcpp.h>
#include <string>
#include <list>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]


//' Interprets text and stores it in a data frame.
//'
//' @param lines The text of the results to convert. This should be
//' a string vector with each line as 1 element.
//' @param dfResults A prebuilt data frame to store results into.
//' Must have c("Place", "Name", "Grade", "School", "Time") as column names.
//' @export
// [[Rcpp::export]]
void convertPAMS(StringVector lines, List dfResults) {
    // Order: Place, Name, Grade, School, Time
    NumericVector places = dfResults["Place"];
    StringVector names = dfResults["Name"];
    NumericVector grades = dfResults["Grade"];
    StringVector schools = dfResults["School"];
    StringVector times = dfResults["Time"];

    int row = 0;
    for(int i = 0; i < lines.size(); ++i)
    {
        // Initialize the string for this runner's line
        std::string line{lines[i]};

        // Skip over the line if not a runner
        if(line.size() <= 1)
            continue;

        std::list<std::string> lineList {};
        std::string current = "";

        for(unsigned int posInLine = 0; posInLine < line.length(); ++posInLine) {
            if(line[posInLine] == ' ' || line[posInLine] == '\t') {
                // If there are characters to add
                if(!current.empty()) {
                    lineList.push_back(current);
                    current = "";
                }
            } else {
                current += line[posInLine];
            }
        }
        // Ensure that anything at the end w/o spaces after is added to the list.
        if(current.length() > 0) {
            lineList.push_back(current);
        }
        lineList.pop_back(); // Removes TeamPlace
        // lineList now contains
        // [Place, First Name, Last Name, Grade, School ..., Time]


    // Place
        // std::istringstream(lineList.front()) >> places[row];
        places[row] = std::stoi(lineList.front());
        lineList.pop_front();

    // Name
        std::string name = lineList.front();
        lineList.pop_front();
        // until the next character is a number (for grade)
        while(!isdigit(lineList.front()[0]))
        {
            name += ' ';
            name += lineList.front();
            lineList.pop_front();
        }
        names[row] = name;

    // Grade
        grades[row] = std::stoi(lineList.front());
        lineList.pop_front();

    // Time
        // Do time at the end because it's easy
        times[row] = lineList.back();
        lineList.pop_back();

    // School
        std::string schoolName = lineList.front();
        lineList.pop_front();
        // Combine all words in the school name into one string
        while(!lineList.empty()) {
            schoolName += ' ';
            schoolName += lineList.front();
            lineList.pop_front();
        }
        schools[row] = schoolName;

        ++row;
    }
}
