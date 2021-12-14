using System;
using System.IO;
using System.Collections.Generic;

namespace Day4
{
    public class Board
    {
        public int Index = 0;
        public List<List<int>> Rows = new List<List<int>>();
        public List<List<int>> Columns = new List<List<int>>();

        public int WinRow() {
            for (var i = 0; i < Rows.Count; i++) {
                if (Rows[i].Count == 0) {
                    return i;
                }
            }
            return -1;
        }

        public int WinColumn() {
            for (var i = 0; i < Columns.Count; i++) {
                if (Columns[i].Count == 0) {
                    return i;
                }
            }
            return -1;
        }

        public bool Call(int numberCalled) {
            for (var i = 0; i < 5; i++) {
                Rows[i].Remove(numberCalled);
                Columns[i].Remove(numberCalled);
            }
            return Rows.Exists(item => (item.Count == 0)) || Columns.Exists(item => (item.Count == 0));
        }

        public int RemainingTotal() {
            int count = 0;
            for (var i = 0; i < 5; i++) {
                Rows[i].ForEach(item => count += item);
            }
            return count;
        }

        public static Board ReadBoard(StreamReader sr) {
            Board b = new Board();
            string line = null;
            int r = 0;
            int v = 0;
            while ((r < 5)&&((line = sr.ReadLine()) != null))
            {
                while (line.Length == 0) {
                    line = sr.ReadLine();
                    continue;
                }
                var rowItems = new List<string>(line.Split(' '));
                b.Rows.Add(new List<int>());
                int c = 0;
                foreach (var item in rowItems) {
                    if (int.TryParse(item, out v)) {
                        b.Rows[r].Add(v);
                        c++;
                    }
                }
                r++;
            }
            if (line == null) {
                return null;
            }
            for (var c = 0; c < 5; c++) {
                b.Columns.Add(new List<int>());
                for (r = 0; r < 5; r++) {
                    b.Columns[c].Add(b.Rows[r][c]);
                }
            }
            return b;
        }
    }

    class Program
    {
        static public List<int> CallNumbers { get; set; }
        static public List<Board> Boards { get; set; }

        static void Main(string[] args) {
            Part1();
            Part2();
        }


        static void Part1()
        {
            Console.WriteLine("Advent of Code 2021.\nDay 4 Part 1.");
            ReadInputFile("input.txt");
            Console.WriteLine($"Loaded {Boards.Count} boards.");
            bool done = false;
            foreach (var n in CallNumbers) {
                Console.WriteLine($"Call: {n}");
                if (done) {
                    break;
                }
                Boards.ForEach(b => {
                    if (b.Call(n)) {
                        var score = n * b.RemainingTotal();
                        Console.WriteLine($"Winner! Score = {score}");
                        done = true;
                    }
                });
            }
        }

        static void Part2()
        {
            Console.WriteLine("Day 4 Part 2.");
            ReadInputFile("input.txt");
            Console.WriteLine($"Loaded {Boards.Count} boards.");

            bool done = false;
            int winBoardCount = 0;
            List<int> winningBoards = new List<int>();

            foreach (var n in CallNumbers) {
                if (done) {
                    break;
                }
                Console.WriteLine($"Call: {n}");
                Boards.ForEach(b => {
                    if (b.Call(n)) {
                        if (!winningBoards.Contains(b.Index)) {
                            var score = n * b.RemainingTotal();
                            Console.WriteLine($"Winner! Score = {score}");
                            winningBoards.Add(b.Index);
                            winBoardCount++;
                            if (winBoardCount == Boards.Count) {
                                done = true;
                            }
                        }
                    }
                });
            }
        }

        static void ReadInputFile(string fileName)
        {
            using (StreamReader sr = new StreamReader(fileName))
            {
                var line = sr.ReadLine();
                CallNumbers = new List<int>();
                foreach (var item in line.Split(','))
                {
                    CallNumbers.Add(int.Parse(item));
                }
                Boards = new List<Board>();
                var boardIndex = 1;
                while (!sr.EndOfStream) {
                    Board b = Board.ReadBoard(sr);
                    b.Index = boardIndex;
                    Boards.Add(b);
                    boardIndex++;
                }
            }
        }
    }
}
