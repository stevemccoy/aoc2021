using System;
using System.IO;
using System.Collections.Generic;

namespace Day20
{
    class Program
    {
        static public List<bool> Algorithm = new List<bool>();
        static public List<List<bool>> Field;
        static public int NumRows;
        static public int NumCols;

        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021. Day 20.");
            Console.WriteLine("Part 1 - Test:");
            Part1_For("test.txt", false, false);

            Console.WriteLine("Part 1 - Input:");
            Part1_For("day20.txt", true, true);

            Console.WriteLine("Part 2 - Input:");
            Part2_For("day20.txt", true, true);
        }

        static void Part1_For(string fileName, bool alternate, bool oddSet) {
            ReadInputFile(fileName);
            Display();
            ProcessFrames(2, alternate, oddSet);
            int count = CountLitPixels();
            Console.WriteLine($"No of lit pixels = {count}");
        }

        static void Part2_For(string fileName, bool alternate, bool oddSet) {
            ReadInputFile(fileName);
            Display();
            ProcessFrames(50, alternate, oddSet);
            int count = CountLitPixels();
            Console.WriteLine($"No of lit pixels = {count}");
        }

        static void ProcessFrames(int numSteps, bool alternate, bool oddSet) {
            for (int step = 0; step < numSteps; step++) {
                var defaultValue = alternate ? true : false;
                if (alternate) {
                    if (oddSet) {
                        defaultValue = (step % 2 == 0) ? false : true;
                    }
                    else {
                        defaultValue = (step % 2 == 0) ? true : false;
                    }
                }
                PadImage(defaultValue);
                Field = ProcessImage(defaultValue);
                Display();
            }
        }

        static int CountLitPixels() {
            int count = 0;
            for (var r = 0; r < NumRows; r++) {
                for (var c = 0; c < NumCols; c++) {
                    if (Field[r][c]) {
                        count++;
                    }
                }
            }
            return count;
        }

        static void ReadInputFile(string fileName)
        {
            Field = new List<List<bool>>();
            Algorithm = new List<bool>();
            NumCols = NumRows = 0;

            using (StreamReader sr = new StreamReader(fileName))
            {
                var AlgorithmString = sr.ReadLine();
                foreach (var ch in AlgorithmString)
                {
                    Algorithm.Add((ch == '#') ? true : false);
                }
                string line = string.Empty;
                while ((line = sr.ReadLine()) != null)
                {
                    if (line.Length < 1) {
                        continue;
                    }
                    List<bool> row = new List<bool>();
                    foreach (var ch in line)
                    {
                        row.Add((ch == '#') ? true : false);
                    }
                    Field.Add(row);
                }
                NumCols = Field[0].Count;
                NumRows = Field.Count;
            }
        }

        static List<bool> Neighbours(int col, int row, bool defaultValue) {
            List<bool> result = new List<bool>();
            int nx, ny;
            for (var dr = -1; dr <= 1; dr++) {
                ny = row + dr;
                for (var dc = -1; dc <= 1; dc++) {
                    nx = col + dc;
                    result.Add(GoodCoord(nx, ny) ? Field[ny][nx] : defaultValue);
                }
            }
            return result;
        }

        static int FromBits(List<bool> bits) {
            int value = 0;
            foreach (var b in bits)
            {
                value = 2 * value + (b ? 1 : 0);
            }
            return value;
        }

        static List<List<bool>> ProcessImage(bool defaultValue) {
            List<List<bool>> result = new List<List<bool>>();
            for (var r = 0; r < NumRows; r++) {
                var newRow = new List<bool>();
                for (var c = 0; c < NumCols; c++) {
                    var offset = FromBits(Neighbours(c, r, defaultValue));
                    newRow.Add(Algorithm[offset]);
                }
                result.Add(newRow);
            }
            return result;
        }

        static void PadImage(bool defaultValue) {
            NumCols += 2;
            var row = new List<bool>();
            while (row.Count < NumCols) {
                row.Add(defaultValue);
            }
            for (int r = 0; r < NumRows; r++) {
                Field[r].Insert(0, defaultValue);
                Field[r].Add(defaultValue);
            }
            Field.Insert(0, row);
            Field.Add(row);
            NumRows += 2;
        }

        static bool GoodCoord(int col, int row) {
            return ((col >= 0)&&(col < NumCols)&&(row >= 0)&&(row < NumRows));
        }

        static void Display() {
            Console.WriteLine();
            for (var r = 0; r < NumRows; r++) {
                var row = Field[r];
                for (var c = 0; c < NumCols; c++) {
                    if (row[c]) {
                        Console.Write("#");
                    }
                    else {
                        Console.Write(".");
                    }
                }
                Console.WriteLine();
            }
            Console.WriteLine();
        }

    }
}
