using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Day14
{
    class Program
    {
        static Dictionary<string, string> Rules = new Dictionary<string, string>();
        static string Strand = "";

        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021.\nDay 14, Part 1");
            Console.WriteLine("Test:");
            ReadInputFile("test14.txt");
            for (int i = 0; i < 5; i++) {
                Console.WriteLine(Strand);
                Strand = DoStep(Strand);
            }

            ReadInputFile("input14.txt");
            DoSteps(10);
            var stats = Statistics().OrderBy(p => p.Value);
            var minValue = stats.First().Value;
            var maxValue = stats.Last().Value;
            Console.WriteLine($"Most common: {stats.Last().Key} : {maxValue}");
            Console.WriteLine($"Least common: {stats.First().Key} : {minValue}");
            Console.WriteLine($"Difference = {maxValue - minValue}");

            Console.WriteLine("Part 2.");
            DoSteps(30);
            stats = Statistics().OrderBy(p => p.Value);
            minValue = stats.First().Value;
            maxValue = stats.Last().Value;
            Console.WriteLine($"Most common: {stats.Last().Key} : {maxValue}");
            Console.WriteLine($"Least common: {stats.First().Key} : {minValue}");
            Console.WriteLine($"Difference = {maxValue - minValue}");

        }

        private static void DoDemo(string input)
        {
            Strand = input;
        }

        static void ReadInputFile(string fileName)
        {
            string line;
            Rules.Clear();
            using (StreamReader sr = new StreamReader(fileName))
            {
                Strand = sr.ReadLine();
                while ((line = sr.ReadLine()) != null)
                {
                    if (line.Contains(" -> ")) {
                        Rules[line.Substring(0, 2)] = line.Substring(6);
                    }
                }
            }
        }

        static void DoSteps(int n) {
            for (int i = 0; i < n; i++) {
                Strand = DoStep(Strand);
            }
        }
        static string DoStep(string input) {
            input += " ";
            StringBuilder output = new StringBuilder("", input.Length * 2);
            string from = "";
            for (int i = 0; i < input.Length - 1; i++) {
                from = input.Substring(i, 2);
                output.Append(from[0]);
                if (Rules.ContainsKey(from)) {
                    output.Append(Rules[from]);
                }
            }
//            output.Append(input[input.Length - 2]);
            return output.ToString();
        }

        static Dictionary<char, Int64> Statistics() {
            var result = new Dictionary<char, Int64>();
            foreach (char ch in Strand) {
                if (result.ContainsKey(ch)) {
                    result[ch]++;
                }
                else {
                    result[ch] = 1;
                }
            }
            return result;
        }
    }
}
