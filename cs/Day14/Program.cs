using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Day14
{
    class Polymer
    {
        public int Steps { get; set; }
        public string Strand { get; set; }

        public Polymer(int steps, string strand)
        {
            Steps = steps;
            Strand = strand;
        }
    }

    class Program
    {
        const int StringSplitLength = 1024;
        const int NumStringSplits = 8;

        static Dictionary<string, string> Rules = new Dictionary<string, string>();
        static string Strand = "";
        static Stack<Polymer> Work = new Stack<Polymer>();
        static Dictionary<char, Int64> RunningStats = new Dictionary<char, Int64>();

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
            Part2Again();
/*            
            Part2();
            stats = RunningStats.OrderBy(p => p.Value);
            minValue = stats.First().Value;
            maxValue = stats.Last().Value;
            Console.WriteLine($"Most common: {stats.Last().Key} : {maxValue}");
            Console.WriteLine($"Least common: {stats.First().Key} : {minValue}");
            Console.WriteLine($"Difference = {maxValue - minValue}");
*/            
        }

        static void Part2() {
            ReadInputFile("input14.txt");
            SetupStatistics();
            Work.Push(new Polymer(0, Strand));
            ProcessDay2Work_WrongWay(40);
        }

        private static void ProcessDay2Work_WrongWay(int numSteps)
        {
            Polymer p;
            char? previousChar = null;
            while (Work.TryPop(out p))
            {
                bool deferStats = false;
                // Grab strand and process.
                while (p.Steps < numSteps)
                {
                    p.Strand = DoStep(p.Strand, previousChar);
                    p.Steps++;
                    if (p.Strand.Length > StringSplitLength) {
                        SplitAndPushWork(p);
                        deferStats = true;
                        break;
                    }
                }
                if (!deferStats) {
                    AddStringToStats(p.Strand);
                    previousChar = p.Strand.Last<char>();
                }
            }
        }

        private static void SplitAndPushWork(Polymer p)
        {
            var input = p.Strand;
            int n = StringSplitLength / NumStringSplits;
            while (input.Length > n) {
                Work.Push(new Polymer(p.Steps, input.Substring(input.Length - n)));
                input = input.Substring(0, input.Length - n);
            }
            if (input.Length > 0) {
                Work.Push(new Polymer(p.Steps, input));
            }
        }

        private static void SetupStatistics()
        {
            RunningStats.Clear();
        }

        private static void AddStringToStats(string strand) 
        {
            foreach (char ch in strand) {
                if (RunningStats.ContainsKey(ch)) {
                    RunningStats[ch]++;
                }
                else {
                    RunningStats[ch] = 1;
                }
            }
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
        static string DoStep(string input, char? previousChar = null) {
            input += " ";
            string from = "";
            StringBuilder output = new StringBuilder("", input.Length * 2);
            // Deal with replacement due to previous character if needed.
            if (previousChar.HasValue) {
                from = previousChar.Value + input[0].ToString();
                if (Rules.ContainsKey(from)) {
                    output.Append(Rules[from]);
                }
            }
            // Process the main part of the input string.
            for (int i = 0; i < input.Length - 1; i++) {
                from = input.Substring(i, 2);
                output.Append(from[0]);
                if (Rules.ContainsKey(from)) {
                    output.Append(Rules[from]);
                }
            }
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

        static Dictionary<string, Int64> DoubleCounts = new Dictionary<string, long>();

        static Dictionary<string, Int64> Frequencies = new Dictionary<string, long>();

        static void Part2Again() {
            Console.WriteLine("Part 2. Reading input file.");
            ReadInputFile("input14.txt");
            Console.WriteLine("Setting up initial counts.");
            SetupCounts();
            Console.WriteLine("Stepping through 40 cycles of reaction.");
            for (int step = 0; step < 40; step++) {
                DoStepPart2Frequencies();
            }
            Console.WriteLine("Done.\nCalculating Statistics.");
            Part2Stats();
        }

        static void IncrementCount<KT>(Dictionary<KT, Int64> d, KT k, Int64 amount = 1)
        {
            if (d.ContainsKey(k)) {
                d[k] += amount;
            }
            else {
                d[k] = amount;
            }
        }

        static void SetupCounts() {
            for (int i = 0; i < Strand.Length - 1; i++) {
                var pair = Strand.Substring(i, 2);
                IncrementCount(Frequencies, pair, 1);
                IncrementCount(DoubleCounts, pair.Substring(0, 1), 1);
            }
            DoubleCounts[Strand.Substring(0, 1)]--;
        }

        static void DoStepPart2Frequencies() {
            Dictionary<string, Int64> dest = new Dictionary<string, long>();
            foreach (var reactant in Frequencies) {
                if (Rules.ContainsKey(reactant.Key)) {
                    var product = Rules[reactant.Key];
                    var pair1 = reactant.Key[0] + product;
                    var pair2 = product + reactant.Key[1];
                    IncrementCount(dest, pair1, reactant.Value);
                    IncrementCount(dest, pair2, reactant.Value);
                    IncrementCount(DoubleCounts, product, reactant.Value);
                }
                else {
                    dest[reactant.Key] = reactant.Value;
                }
            }
            Frequencies = dest;
        }

        static void Part2Stats() {
            Dictionary<char, Int64> elementCounts = new Dictionary<char, long>();
            foreach (var pair in Frequencies) {
                IncrementCount(elementCounts, pair.Key[0], pair.Value);
                IncrementCount(elementCounts, pair.Key[1], pair.Value);
            }
            foreach (var pair in DoubleCounts) {
                elementCounts[pair.Key[0]] -= DoubleCounts[pair.Key];
            }
            var sorted = elementCounts.AsEnumerable().OrderByDescending(p => p.Value);
            Console.WriteLine($"Most common element is {sorted.First().Key} with count {sorted.First().Value}");
            Console.WriteLine($"Least common element is {sorted.Last().Key} with count {sorted.Last().Value}");
            var difference = sorted.First().Value - sorted.Last().Value;
            Console.WriteLine($"Difference is : {difference}");            
        }
    }
}
