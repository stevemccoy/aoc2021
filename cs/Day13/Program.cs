using System;
using System.IO;
using System.Collections.Generic;

namespace Day13
{
    public class Point {
        public int X;
        public int Y;

        public Point(int x, int y)
        {
            X = x;
            Y = y;
        }
    }

    public class Sheet {
        public List<Point> Points;
        public List<Tuple<string,int>> Instructions;

        public Sheet()
        {
            Points = new List<Point>();
            Instructions = new List<Tuple<string, int>>();
        }

        public void Add(int x, int y) {
            Points.Add(new Point(x, y));
        }

        public void DoAllFolds() {
            foreach (var t in Instructions) {
                DoFold(t.Item1, t.Item2);
                Console.WriteLine($"Fold done. {Points.Count} dots remain.");
            }
        }

        public void DoFold(string axis, int value) {
            if (axis == "x") {
                foreach (var p in Points) {
                    if (p.X > value) {
                        p.X = 2 * value - p.X;
                    }
                }
                for (int i = 0; i < Points.Count; ) {
                    var p = Points[i];
                    if (p.X == value) {
                        Points.RemoveAt(i);
                        continue;
                    }
                    i++;
                }
            }
            else if (axis == "y") {
                foreach (var p in Points) {
                    if (p.Y > value) {
                        p.Y = 2 * value - p.Y;
                    }
                }
                for (int i = 0; i < Points.Count; ) {
                    var p = Points[i];
                    if (p.Y == value) {
                        Points.RemoveAt(i);
                        continue;
                    }
                    i++;
                }
            }
            RemoveDuplicates();
        }

        private void RemoveDuplicates() {
            for (int i = 0; i < Points.Count; ) {
                var p = Points[i];
                for (int j = i + 1; j < Points.Count; ) {
                    var q = Points[j];
                    if (p.X == q.X && p.Y == q.Y) {
                        Points.RemoveAt(j);
                        continue;
                    }
                    j++;
                }
                i++;
            }
        }

        bool IsDot(int x, int y) {
            foreach (var p in Points) {
                if (p.X == x && p.Y == y) {
                    return true;
                }
            }
            return false;
        }

        public void Display() {
            int mx = 0, my = 0;
            foreach (var p in Points) {
                if (p.X > mx) {
                    mx = p.X;
                }
                if (p.Y > my) {
                    my = p.Y;
                }    
                // Console.WriteLine($"{p.X}, {p.Y}");
            }
            Console.WriteLine();
            for (var r = 0; r <= my; r++) {
                for (var c = 0; c <= mx; c++) {
                    Console.Write(IsDot(c, r) ? '*' : ' ');
                }
                Console.WriteLine();
            }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021. Day 13 Part 1.");
            ReadInputFile("test13a.txt");
            Console.WriteLine("Read test sheet. Doing folds.");
            theSheet.DoAllFolds();
            Console.WriteLine($"All done. There are {theSheet.Points.Count} dots visible.");
            ReadInputFile("input13.txt");
            Console.WriteLine("Read input sheet. Doing folds.");
            theSheet.DoAllFolds();
            Console.WriteLine($"All done. There are {theSheet.Points.Count} dots visible.");
            theSheet.Display();
        }

        static Sheet theSheet = new Sheet();      

        static void ReadInputFile(string fileName)
        {
            string line;
            theSheet.Points.Clear();
            theSheet.Instructions.Clear();
            using (StreamReader sr = new StreamReader(fileName))
            {
                while ((line = sr.ReadLine()) != null)
                {
                    if (line.Contains(',')) {
                        var xs = line.Substring(0, line.IndexOf(','));
                        var ys = line.Substring(line.IndexOf(',')+1);
                        theSheet.Add(int.Parse(xs), int.Parse(ys));
                    }
                    else if (line.StartsWith("fold along ")) {
                        line = line.Substring(11);
                        var axis = line.Substring(0,1);
                        var value = line.Substring(2);
                        theSheet.Instructions.Add(new Tuple<string, int>(axis, int.Parse(value)));
                    }
                }
            }
        }
    }
}
