#include<iostream>
#include<fstream>
#include<vector>
#include<string>
#include<algorithm>

const char* test_input = "16,1,2,0,4,2,7,1,2,14";

std::vector<std::string> read_input_file(char *file_name) {
    std::vector<std::string> result;
    std::ifstream infile(file_name, std::ifstream::in);
    std::string line;
    while (getline(infile, line)) {
        result.push_back(line);
    }
    infile.close();
    return result;
}

std::vector<int> start_position;

void tokenize(std::string const &str, const char delim, std::vector<std::string> &out) {
    size_t start;
    size_t end = 0;
    while ((start = str.find_first_not_of(delim, end)) != std::string::npos) {
        end = str.find(delim, start);
        out.push_back(str.substr(start, end - start));
    }
}

void setup_start_from(std::string line) {
    std::vector<std::string> values;
    int ivalue = 0;
    tokenize(line, ',', values);
    for (auto value : values) {
        sscanf(value.c_str(), "%d", &ivalue);
        start_position.push_back(ivalue);
    }
    std::sort(start_position.begin(), start_position.end());
}

int find_middle(std::vector<int> pos) {
    int n = pos.size();
    int low = pos[0], high = pos[n-1];
    int mid = (low + high)/2;
    while () {
        for (v : pos) {
            sum += abs(mid - v);
        }
    }
}

int main() {
    char file_name[] = "input7.txt";
    std::cout << "Advent of Code 2021\nDay 7 - Part 1." << std::endl;
    std::vector<std::string> lines = read_input_file(file_name);
    std::cout << "Read " << lines.size() << " lines." << std::endl;
    setup_start_from(test_input);
    std::cout << "Test Input, read " << start_position.size() << " values." << std::endl;
    for (auto v : start_position) {
        std::cout << v << std::endl;
    }
    return 0;
}
