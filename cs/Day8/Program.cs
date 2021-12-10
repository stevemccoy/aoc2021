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
