using System;
using System.IO;
using System.Collections.Generic;

namespace Day22
{
    public class Interval {
        public Int64 Low;
        public Int64 High;

        public Interval(Int64 low, Int64 high) {
            Low = low;
            High = high;
        }

        public Interval Intersection(Interval other) {
            var x1 = (Low > other.Low) ? Low : other.Low;
            var x2 = (High < other.High) ? High : other.High; 
            if (x1 > x2) {
                return null;
            }
            return new Interval(x1, x2);
        }

        public bool Equals(Interval other) =>
            (Low == other.Low) && (High == other.High);

        public bool Valid() => (Low <= High);

    }

    public class Block {

        static int NextId = 1;

        public int Id;
        public bool State;
        public Interval X, Y, Z;

        static string[] Delimiters = new string[] { ",", " ", "..", "=" };

        public Block(string line) {
            Id = NextId++;
            var fields  = line.Split(Delimiters, StringSplitOptions.TrimEntries);
            State       = (fields[0] == "on");
            X = new Interval(int.Parse(fields[2]), int.Parse(fields[3]));
            Y = new Interval(int.Parse(fields[5]), int.Parse(fields[6]));
            Z = new Interval(int.Parse(fields[8]), int.Parse(fields[9]));
        }

        public Block(bool state, Interval x, Interval y, Interval z) {
            Id = NextId++;
            State = state;
            X = x;
            Y = y;
            Z = z;
        }

        public Block Intersection(Block other) {
            var xi = X.Intersection(other.X);
            var yi = Y.Intersection(other.Y);
            var zi = Z.Intersection(other.Z);
            if (xi == null || yi == null || zi == null) {
                return null;
            }
            else {
                return new Block(true, xi, yi, zi);
            }
        }

        public Int64 Volume() =>
            (X.High - X.Low + 1) * (Y.High - Y.Low + 1) * (Z.High - Z.Low + 1);

        public bool Equals(Block other) => 
            X.Equals(other.X) && Y.Equals(other.Y) && Z.Equals(other.Z);

        public IEnumerable<Block> Subtract(Block other) {
            // Explode the block into max 26 sub-blocks.
            Interval[] xil = {
                new Interval(X.Low, other.X.Low - 1),
                new Interval(other.X.Low,  other.X.High),
                new Interval(other.X.High + 1, X.High)
            };
            Interval[] yil = {
                new Interval(Y.Low, other.Y.Low - 1),
                new Interval(other.Y.Low,  other.Y.High),
                new Interval(other.Y.High + 1, Y.High)
            };
            Interval[] zil = {
                new Interval(Z.Low, other.Z.Low - 1),
                new Interval(other.Z.Low,  other.Z.High),
                new Interval(other.Z.High + 1, Z.High)
            };

            foreach (var xi in xil) {
                if (xi.Valid()) {
                    foreach (var yi in yil) {
                        if (yi.Valid()) {
                            foreach (var zi in zil) {
                                if (zi.Valid()) {
                                    var b = new Block(State, xi, yi, zi);
                                    if (!b.Equals(other)) {
                                        yield return b;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

    }

    class Program
    {
        static List<Block> Blocks = new List<Block>();

        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021 - Day 22, Part 2");
            Console.WriteLine("Running for test data.");
            ReadInputFile("test22.txt");
            ReduceActiveBlocks();
            Int64 total = 0;
            foreach (var b in Blocks) {
                total += b.Volume();
            }
            Console.WriteLine($"Total number of cells lit = {total}");

            Console.WriteLine("Running for all data.");
            ReadInputFile("day22.txt");
            ReduceActiveBlocks();
            total = 0;
            foreach (var b in Blocks) {
                total += b.Volume();
            }
            Console.WriteLine($"Total number of cells lit = {total}");           
        }

        static void ReadInputFile(string fileName)
        {
            Blocks = new List<Block>();
            using (StreamReader sr = new StreamReader(fileName))
            {
                string line = string.Empty;
                while ((line = sr.ReadLine()) != null)
                {
                    if (line.Length < 1) {
                        continue;
                    }
                    var block = new Block(line.Trim());
                    Blocks.Add(block);
                }
            }
        }

        static bool VerifyNonOverlapping(int fromIndex, int toIndex) {
            bool result = true;
            for (int i = fromIndex; i <= toIndex; i++) {
                for (int j = fromIndex; j <= toIndex; j++) {
                    if (i == j) {
                        continue;
                    }
                    if (Blocks[i].Intersection(Blocks[j]) != null) {
                        Console.WriteLine($"Block intersection {i} vs {j}");
                        result = false;
                    }
                }
            }
            return result;
        }

        static void ReduceActiveBlocks() {
            var dotCount = 0;
            for (int i = 0; i < Blocks.Count; ) {
                var b = Blocks[i];
                System.Console.Write(".");
                if (dotCount++ > 80) {
                    dotCount = 0;
                    System.Console.WriteLine();
                }
                if (b.State) {
                    // Turn 'em on
                    List<Block> extras = new List<Block>();
                    for (int j = 0; j < i; ) {
                        var other = Blocks[j];
                        var clash = b.Intersection(other);
                        if (clash != null) {
                            var fragments = other.Subtract(clash);
                            Blocks.RemoveAt(j);
                            i--;
                            extras.AddRange(fragments);
                        }
                        else {
                            j++;
                        }
                    }
                    // Need to add extras in so that we can pick up the next iteration 
                    // of i from the right position.
                    Blocks.InsertRange(i, extras);
                    i += extras.Count;

//                    VerifyNonOverlapping(0, i);
                    i++;
                }
                else {
                    // Turn 'em off
                    // TODO: Sort me out.
                    List<Block> extras = new List<Block>();
                    for (int j = 0; j < i; ) {
                        var other = Blocks[j];
                        var clash = b.Intersection(other);
                        if (clash != null) {
                            var fragments = other.Subtract(clash);
                            Blocks.RemoveAt(j);
                            i--;
                            extras.AddRange(fragments);
                        }
                        else {
                            j++;
                        }
                    }
                    // Don't keep block i in the list when it has been processed.
                    Blocks.RemoveAt(i);
                    // Need to add extras in so that we can pick up the next iteration 
                    // of i from the right position.
                    Blocks.InsertRange(i, extras);
                    i += extras.Count;

//                    VerifyNonOverlapping(0, i - 1);
                }
            }
        }
    }
}
