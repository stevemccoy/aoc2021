// Day24.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <fstream>
#include <string>
#include "Alu.h"


static void tokenize(std::string const& str, const char delim, std::vector<std::string>& out) {
    size_t start;
    size_t end = 0;
    while ((start = str.find_first_not_of(delim, end)) != std::string::npos) {
        end = str.find(delim, start);
        out.push_back(str.substr(start, end - start));
    }
}

static bool ReadInputFile(std::string fileName, Alu& alu) {
    alu.reset();
    std::string line;
    std::vector<std::string> words;

    std::ifstream f(fileName);
    if (f.is_open()) {
        while (getline(f, line)) {
            words.clear();
            tokenize(line, ' ', words);
            while (words.size() < 3) {
                words.push_back("");
            }
            alu.AddInstruction(words[0], words[1], words[2]);
        }
        f.close();
        return true;
    }
    else {
        std::cerr << "Unable to open file " << fileName << std::endl;
        return false;
    }
}

static bool InputsFromString(std::string s, std::vector<int>& digits) {
    int i = 0;
    digits.clear();
    for (auto c : s) {
        i = c - '0';
        if ((i < 1) || (i > 9)) {
            // std::cerr << "Bad character '" << c << "' in input." << std::endl;
            return false;
        }
        digits.push_back(i);
    }
    if (digits.size() < 14) {
        digits.insert(digits.begin(), 14 - digits.size(), 0);
    }
    return true;
}

static bool InputsFromUlong(unsigned long long value, std::vector<int>& digits) {
    char buffer[50];
    sprintf_s(buffer, "%lld", value);
    std::string s = buffer;
    return InputsFromString(s, digits);
}

static void Increment(std::vector<int>& digits) {
    int i = digits.size() - 1;
    while (i >= 0) {
        if (++digits[i] <= 9) {
            break;
        }
        digits[i--] = 1;
    }
}

static std::string DigitString(const std::vector<int>& digits) {
    std::string s;
    for (auto c : digits) {
        s += '0' + c;
    }
    return s;
}

void part1()
{
    std::cout << "Part 1." << std::endl;
    Alu alu;
    ReadInputFile("day24.txt", alu);
    unsigned long long input = 99999999999999;
    std::vector<int> digits;
    const int handful = 9999999;
    int batchCount = handful;
    std::ofstream of("output.txt");

    while (input > 0)
    {
        if (InputsFromUlong(input, digits)) {
            if (alu.verifyMonad(digits)) {
                std::cout << "Valid model number found: " << input << std::endl;
                of << "Valid model number found: " << input << std::endl;
                break;
            }
        }
        if (--batchCount == 0) {
            std::string s = DigitString(digits);
            std::cout << s << std::endl;
            of << s << std::endl;
            batchCount = handful;
        }
        input--;
    }
}

void part2()
{
    std::cout << "Part 2." << std::endl;
    Alu alu;
    ReadInputFile("day24.txt", alu);
    // Pencil and paper analysis yielded (i1 == i14+3), (i2 == i13+4), (i3 == i6+2),
    // limiting the values for first 3 digits. 
    unsigned long long input = 45311111111111;
    std::vector<int> digits;
    const int handful = 999999; //  9;
    int batchCount = handful;
    std::ofstream of("output.txt");
    InputsFromUlong(input, digits);

    while (true)
    {
///        if (InputsFromUlong(input, digits)) {
        if (alu.verifyMonad(digits)) {
            std::string s = DigitString(digits);
            std::cout << "Valid model number found: " << s << std::endl;
            of << "Valid model number found: " << s << std::endl;
            break;
        }
//        }
        if (--batchCount == 0) {
            std::string s = DigitString(digits);
            std::cout << s << std::endl;
            of << s << std::endl;
            of.flush();
            batchCount = handful;
        }
        Increment(digits);
    }
}

int main()
{
    std::cout << "Advent of Code 2021\nDay 24 - Arithmetic Logic Unit" << std::endl;
    part2();

}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
