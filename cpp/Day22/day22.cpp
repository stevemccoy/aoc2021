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

void crop_limits(limits_t& x) {
    x.first = std::max(x.first, index_limits.first);
    x.second = std::min(x.second, index_limits.second);
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
        for (auto j = yb.first; j <= yb.second; j++) {
            for (auto k = zb.first; k <= zb.second; k++) {
                reactor[reactor_bounds_limit + i][reactor_bounds_limit + j][reactor_bounds_limit + k] = value;
            }
        }
    }
    return true;
}

bool process_line_part1(std::string line) {
    char str[6];
    int anint = 2;
    limits_t x, y, z;
    std::string op = line.substr(0, 2);
    (void)sscanf_s(line.substr(line.find(' ') + 1).c_str(), "x=%d..%d,y=%d..%d,z=%d..%d",
        &x.first, &x.second, &y.first, &y.second, &z.first, &z.second);
    bool value = (op == "on") ? true : false;
    return set_reactor(x, y, z, value);
}

static std::vector<std::vector<int>> instructions;
static limits_t xbounds, ybounds, zbounds;

void process_lines_part2(std::vector<std::string> lines) {
    limits_t x, y, z;
    instructions.clear();
    for (auto line : lines) {
        std::string op = line.substr(0, 2);
        (void)sscanf_s(line.substr(line.find(' ') + 1).c_str(), "x=%d..%d,y=%d..%d,z=%d..%d",
            &x.first, &x.second, &y.first, &y.second, &z.first, &z.second);
        int value = (op == "on") ? 1 : 0;
        std::vector<int> instruction = { value, x.first, x.second, y.first, y.second, z.first, z.second };
        instructions.push_back(instruction);
    }
}

void set_bounds() {
    xbounds.first = ybounds.first = zbounds.first = INT_MAX;
    xbounds.second = ybounds.second = zbounds.second = INT_MIN;
    for (auto i : instructions) {
        xbounds.first = std::min(xbounds.first, i[1]);
        ybounds.first = std::min(ybounds.first, i[2]);
        zbounds.first = std::min(zbounds.first, i[3]);
        xbounds.second = std::max(xbounds.second, i[4]);
        ybounds.second = std::max(ybounds.second, i[5]);
        zbounds.second = std::max(zbounds.second, i[6]);
    }
}

static int xbase, ybase, zbase;
static int xrange, yrange, zrange;
static bool* reactor2 = nullptr;
static int reactor2Bytes = 0;

void model_reactor() {
    xbase = xbounds.first;
    ybase = ybounds.first;
    zbase = zbounds.first;
    xrange = xbounds.second - xbounds.first + 1;
    yrange = ybounds.second - ybounds.first + 1;
    zrange = zbounds.second - zbounds.first + 1;
    reactor2Bytes = sizeof(bool) * xrange * yrange * zrange;
    std::cout << "Allocating " << reactor2Bytes << " bytes for reactor model." << std::endl;
    reactor2 = (bool *)std::malloc(reactor2Bytes);
    std::memset(reactor2, 0, reactor2Bytes);
    std::cout << "Done." << std::endl;
}

void set_indexed(int x, int y, int z, bool value) {
    reactor2[(zbase + z) + zrange * ((ybase + y) + xrange * (xbase + x))] = value;
}

void set_reactor2(const std::vector<int>& instruction) {
    bool value = instruction[0];
    for (auto i = instruction[1]; i <= instruction[2]; i++) {
        for (auto j = instruction[3]; j <= instruction[4]; j++) {
            for (auto k = instruction[5]; k <= instruction[6]; k++) {
                set_indexed(i, j, k, value);
            }
        }
    }
}

void recover_storage() {
    std::free(reactor2);
}

int count_reactor2() {
    int count = 0;
    for (int i = 0; i < reactor2Bytes; i++) {
        if (reactor2[i]) {
            count++;
        }
    }
    return count;
}

int main() {
    char file_name[] = "input22.txt";
    std::cout << "Advent of Code 2021\nDay 22 - Part 1." << std::endl;
    std::vector<std::string> lines = read_input_file(file_name);
    std::cout << "Read " << lines.size() << " lines." << std::endl;
    clear_reactor();
    for (auto line : lines) {
        process_line_part1(line);
    }
    auto count = count_reactor();
    std::cout << "Final reactor state: " << count << " cells are on." << std::endl;

    std::cout << "Day 22 - Part 2" << std::endl;
    process_lines_part2(lines);
    set_bounds();
    model_reactor();
    for (auto instruction : instructions) {
        set_reactor2(instruction);
    }
    std::cout << "Done with instructions - counting reactor cells..." << std::endl;
    count = count_reactor2();
    std::cout << "Final reactor state: " << count << " cells are on." << std::endl;

    recover_storage();
    return 0;
}
