using System;
using System.IO;
using System.Collections.Generic;

namespace Day9
{
    class Program
    {
        static public List<string> InputLines { get; set; }

        static public List<List<int>> Field;
        static public int NumRows;
        static public int NumCols;

        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021.\nDay 9, Part 1.");
            InputLines = ReadInputFile("day9.txt");
            ProcessInput();
            var score = FindMinimaScore();
            Console.WriteLine($"Sum of minima scores = {score}");

            var basins = FindBasins();
            var basinSizes = basins.ConvertAll(b => b.Count);
            basinSizes.Sort();
            basinSizes.Reverse();
            Console.WriteLine("Part 2.");
            Console.WriteLine($"Product of 3 largest basin sizes:");
            var x = basinSizes[0];
            var y = basinSizes[1];
            var z = basinSizes[2];
            var product = x * y * z;
            Console.WriteLine($"{x} * {y} * {z} = {product}");
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

        static void ProcessInput() {
            Field = new List<List<int>>();
            foreach (var line in InputLines) {
                var row = new List<int>();
                foreach (var ch in line) {
                    row.Add(ch - '0');
                }
                Field.Add(row);
            }
            NumRows = Field.Count;
            NumCols = Field[0].Count;
        }

        static int FindMinimaScore() {
            int score = 0;
            for (int r = 0; r < NumRows; r++) {
                for (int c = 0; c < NumCols; c++) {
                    var f = Field[r][c];
                    var neighbours = Neighbours(c, r);
                    if (neighbours.TrueForAll(n => n > f)) {
                        score += (f + 1);
                    }
                }
            }
            return score;
        }

        static List<HashSet<Tuple<int,int>>> FindBasins() {
            var result = new List<HashSet<Tuple<int,int>>>();
            for (int r = 0; r < NumRows; r++) {
                for (int c = 0; c < NumCols; c++) {
                    var f = Field[r][c];
                    var neighbours = Neighbours(c, r);
                    if (neighbours.TrueForAll(n => n > f)) {
                        result.Add(BasinPoints(c, r));
                    }
                }
            }
            return result;
        }

        // Find all points in the basin around local minimum given.
        static HashSet<Tuple<int,int>> BasinPoints(int col, int row) {
            List<int> deltas = new List<int>() {-1, 1};
            var result = new HashSet<Tuple<int, int>>();
            var queue = new Queue<Tuple<int, int>>();
            queue.Enqueue(new Tuple<int, int>(col,row));
            Tuple<int, int> item;
            while (queue.TryDequeue(out item))
            {
                result.Add(item);
                Tuple<int, int> neighbour;
                foreach (int d in deltas) {
                    if (GoodCoord(item.Item1 + d, item.Item2)) {
                        neighbour = new Tuple<int, int>(item.Item1 + d, item.Item2);
                        if (!result.Contains(neighbour)&&(Field[neighbour.Item2][neighbour.Item1] < 9)) {
                            queue.Enqueue(neighbour);
                        }
                    }
                    if (GoodCoord(item.Item1, item.Item2 + d)) {
                        neighbour = new Tuple<int, int>(item.Item1, item.Item2 + d);
                        if (!result.Contains(neighbour)&&(Field[neighbour.Item2][neighbour.Item1] < 9)) {
                            queue.Enqueue(neighbour);
                        }
                    }
                }
            }
            return result;
        }

        static List<int> Neighbours(int col, int row) {
            List<int> result = new List<int>();
            int nx, ny;
            for (int i = -1; i < 2; i += 2) {
                nx = col + i;
                ny = row + i;
                if (GoodCoord(nx, row)) {
                    result.Add(Field[row][nx]);
                }
                if (GoodCoord(col, ny)) {
                    result.Add(Field[ny][col]);
                }
            }
            return result;
        }

        static bool GoodCoord(int col, int row) {
            return ((col >= 0)&&(col < NumCols)&&(row >= 0)&&(row < NumRows));
        }

    }

}
