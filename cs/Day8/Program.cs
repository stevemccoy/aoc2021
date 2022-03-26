using System;
using System.IO;
using System.Collections.Generic;

namespace Day8
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021");
            Part1();
            Part2();
        }

        static void Part1()
        {
            Console.WriteLine("Day 8. Part 1.");
            IEnumerable<string> lines = ReadInputFile("input.txt");
            int count = 0;
            List<int> wantedCounts = new List<int> {2, 3, 4, 7};
            foreach (var line in lines)
            {
                var outputs = line.Substring(line.IndexOf('|')+1).Split(' ');
                foreach (var output in outputs)
                {
                    if (wantedCounts.Contains(output.Trim().Length))  {
                        count++;                        
                    }
                }
            }
            Console.Write("Count of digits 1, 4, 7, 8 in output values = ");
            Console.WriteLine(count);
        }

        static void Part2() 
        {
            Console.WriteLine("Day 8. Part 2.");
            IEnumerable<string> lines = ReadInputFile("input.txt");
            int total = 0;
            foreach (var line in lines) {
                total += DecodeLine(line);
            }
            Console.WriteLine("Completed processing lines.");
            Console.WriteLine($"Total of digit outputs = {total}");
        }

        static int DecodeLine(string line) {
            // Code for input signal patterns for digits.
            Dictionary<int, SortedSet<char>> digitMap = new Dictionary<int, SortedSet<char>>();
            // Read in input patterns.
            var inputs = line.Substring(0, line.IndexOf('|')).Trim().Split(' ');
            List<SortedSet<char>> inputEntries = new List<SortedSet<char>>();
            // Deduce the easy mappings.
            foreach (var input in inputs) {
                var s = new SortedSet<char>(input);
                switch (s.Count) {
                    case 2:     digitMap[1] = s;
                                break;
                    case 3:     digitMap[7] = s;
                                break;
                    case 4:     digitMap[4] = s;
                                break;
                    case 7:     digitMap[8] = s;
                                break;
                    default:    inputEntries.Add(s);
                                break;
                }
            }
            // Process the unmapped input entries.
            // Digits with 6 segments set.
            var sixers = inputEntries.FindAll(s => s.Count == 6);
            var i = sixers.FindIndex(s => !s.IsSupersetOf(digitMap[7]));
            digitMap[6] = sixers[i];
            sixers.RemoveAt(i);
            i = sixers.FindIndex(s => s.IsProperSupersetOf(digitMap[4]));
            digitMap[9] = sixers[i];
            sixers.RemoveAt(i);
            digitMap[0] = sixers[0];

            // Digits with 5 segments set.
            var fivers = inputEntries.FindAll(s => s.Count == 5);
            i = fivers.FindIndex(s => s.IsProperSupersetOf(digitMap[1]));
            digitMap[3] = fivers[i];
            fivers.RemoveAt(i);
            i = fivers.FindIndex(s => s.IsProperSubsetOf(digitMap[9]));
            digitMap[5] = fivers[i];
            fivers.RemoveAt(i);
            digitMap[2] = fivers[0];

            // Now use code to determine the output display digits.
            var outputs = line.Substring(line.IndexOf('|')+1).Trim().Split(' ');
            int result = 0;
            foreach (var outputString in outputs) {
                var s = new SortedSet<char>(outputString);
                var found = false;
                foreach (var d in digitMap.Keys) {
                    if (s.SetEquals(digitMap[d])) {
                        result = result * 10 + d;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    throw new Exception($"Unrecognized output pattern {outputString} for line:\n{line}");
                }
            }
            return result;
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
    }
}
