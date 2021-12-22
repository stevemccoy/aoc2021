#include<iostream>
#include<fstream>
#include<vector>
#include<string>
#include<algorithm>

typedef std::pair<int, int> limits_t;

const int reactor_bounds_limit = 50;
const int reactor_array_size = 2 * reactor_bounds_limit + 1;
const limits_t index_limits = { -1 * reactor_bounds_limit, reactor_bounds_limit };

bool reactor[reactor_array_size][reactor_array_size][reactor_array_size];

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

void clear_reactor() {
    for (int i = 0; i < reactor_array_size; i++) {
        for (int j = 0; j < reactor_array_size; j++) {
            for (int k = 0; k < reactor_array_size; k++) {
                reactor[i][j][k] = false;
            }
        }
    }
}

int count_reactor() {
    int count = 0;
    for (int i = 0; i < reactor_array_size; i++) {
        for (int j = 0; j < reactor_array_size; j++) {
            for (int k = 0; k < reactor_array_size; k++) {
                if (reactor[i][j][k]) {
                    count++;
                }
            }
        }
    }
    return count;
}

limits_t crop_limits(limits_t x) {
    x.first = std::max(x.first, index_limits.first);
    x.second = std::min(x.second, index_limits.second);
    return x;
}

bool limits_valid(limits_t x) {
    return (x.second > x.first);
}

bool set_reactor(limits_t xb, limits_t yb, limits_t zb, bool value) {
    crop_limits(xb);
    crop_limits(yb);
    crop_limits(zb);
    if (!limits_valid(xb) || !limits_valid(yb) || !limits_valid(zb)) {
        return false;
    }
    for (auto i = xb.first; i <= xb.second; i++) {
        for (auto j = xb.first; j <= xb.second; j++) {
            for (auto k = xb.first; k <= xb.second; k++) {
                reactor[reactor_bounds_limit + i][reactor_bounds_limit + j][reactor_bounds_limit + k] = value;
            }
        }
    }
    return true;
}

bool process_line(std::string line) {
    std::string op = "";
    int args[6];
    limits_t x, y, z;
    sscanf(line.c_str(), "%s x=%d..%d,y=%d..%d,z=%d..%d", &op, &x.first, &x.second, &y.first, &y.second, &z.first, &z.second);
    bool value = ((op == "on") ? true : false);
    return set_reactor(x, y, z, value);
}

int main() {
    char file_name[] = "input22.txt";
    std::cout << "Advent of Code 2021\nDay 22 - Part 1." << std::endl;
    std::vector<std::string> lines = read_input_file(file_name);
    std::cout << "Read " << lines.size() << " lines." << std::endl;
    clear_reactor();
    for (auto line : lines) {
        process_line(line);
    }
    auto count = count_reactor();
    std::cout << "Final reactor state: " << count << " cells are on." << std::endl;
    return 0;
}
