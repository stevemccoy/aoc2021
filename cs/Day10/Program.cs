using System;
using System.IO;
using System.Collections.Generic;

namespace Day10
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021\nDay 10 Part 1.");

            var total_score = ScoreFile("test.txt");
            Console.WriteLine($"Test Input Score = {total_score}");

            total_score = ScoreFile("input.txt");
            Console.WriteLine($"Input data score = {total_score}");

            Console.WriteLine("Day 10 Part 2.");
            var ac_score = AutoCompleteFile("test.txt");
            Console.WriteLine($"Test input auto-complete score = {ac_score}");

            ac_score = AutoCompleteFile("input.txt");
            Console.WriteLine($"Problem input auto-complete score = {ac_score}");
        }

        static List<string> ReadInputFile(string fileName)
        {
            string line;
            List<string> result = new List<string>();
            using (StreamReader sr = new StreamReader(fileName))
            {
                while ((line = sr.ReadLine()) != null)
                {
                    result.Add(line.Trim());
                }
            }
            return result;
        }

        static int ScoreFile(string fileName) {
            var input = ReadInputFile(fileName);
            Console.WriteLine("Read {0} lines", input.Count);

            int line_number = 0;
            int total_score = 0;
            foreach (var line in input) {
                line_number++;
                var score = ScoreLine(line);
                Console.WriteLine($"Line {line_number} score = {score}");
                total_score += score;
            }
            return total_score;
        }

        static Int64 AutoCompleteFile(string fileName) {
            var input = ReadInputFile(fileName);
            Console.WriteLine("Read {0} lines", input.Count);
            int line_number = 0;
            List<Int64> scoreList = new List<Int64>();
            foreach (var line in input) {
                line_number++;
                var score = ScoreLine(line);
                if (score == 0) {
                    var completions = CompletionsNeeded();                    
                    Console.WriteLine($"Incomplete line {line_number}, completions = {completions}");
                    var ac_score = AutoCompleteScore(completions.ToString());
                    Console.WriteLine($"Score = {ac_score}");
                    scoreList.Add(ac_score);
                }
            }
            // Sort scores and choose the middle one from the list.
            scoreList.Sort();
            Console.WriteLine($"{scoreList.Count} incomplete lines scored. Choosing the middle score.");
            var result = scoreList[scoreList.Count / 2];
            return result;
        }

        private static string CompletionsNeeded()
        {
            string s = string.Concat(stack);
            return s;
        }

        static Stack<char> stack = new Stack<char>();

        static int ScoreLine(string line)
        {
            stack.Clear();
            int score = 0;
            var line_ok = true;
            for (int i = 0; line_ok && (i < line.Length); i++) {
                var ch = line[i];
                switch (ch) 
                {
                    case '(': stack.Push(')'); break; 
                    case '[': stack.Push(']'); break; 
                    case '{': stack.Push('}'); break;
                    case '<': stack.Push('>'); break;
                    default:
                        if ((stack.Count == 0)||(stack.Pop() != ch)) {
                            switch (ch)
                            {
                                case ')': score = 3; break;
                                case ']': score = 57; break;
                                case '}': score = 1197; break;
                                case '>': score = 25137; break;
                            }
                            line_ok = false;
                        }
                        break;
                }
            }
            return score;
        }

        static Int64 AutoCompleteScore(string closings) {
            Int64 score = 0;
            foreach (var ch in closings) {
                score *= 5;
                switch (ch) {
                    case ')': score += 1; break;
                    case ']': score += 2; break;
                    case '}': score += 3; break;
                    case '>': score += 4; break;
                }
            }
            return score;
        }

    }
}
