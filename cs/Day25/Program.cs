using System;
using System.IO;
using System.Collections.Generic;

namespace Day25
{
    public class Delta {
        public int row, col;
        public char value;
        public Delta(int c, int r, char v) {
            row = r;
            col = c;
            value = v;
        }
    }

    class Program
    {
        static int NumColumns = 0, NumRows = 0;
        static List<List<char>> bgFrame = new List<List<char>>();
        static List<List<char>> fgFrame = new List<List<char>>();

        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021 - Day 25 (Sea Cucumbers)");
            Console.WriteLine("Part 1");
            ReadInputFile("day25.txt");

            int stepCount = 1;
            while (Step() > 0) {
                stepCount++;
//                Display();
            }
            Console.WriteLine($"No change after step {stepCount}.");
        }

        static void ReadInputFile(string fileName)
        {
            // Read input file into the background frame, then copy it forward.
            NumColumns = NumRows = 0;
            bgFrame.Clear();
            fgFrame.Clear();
            using (StreamReader sr = new StreamReader(fileName))
            {
                string line = string.Empty;
                while ((line = sr.ReadLine()) != null)
                {
                    if (line.Length == 0) {
                        continue;
                    }
                    if (NumColumns == 0) {
                        NumColumns = line.Length;
                    }
                    bgFrame.Add(new List<char>(line));
                }
                NumRows = bgFrame.Count;
            }
            CopyFrame(bgFrame, fgFrame);
        }

        static void CopyFrame(List<List<char>> src, List<List<char>> dest) {
            dest.Clear();
            foreach (var line in src) {
                dest.Add(new List<char>(line));
            }
        }

        static int Step() {
            // Stage and then commit mods in the background frame then copy forward.
            int changeCount = 0;
            Queue<Delta> changes = new Queue<Delta>();
            // Moves for east-facing herd first, then south-facing.
            for (var r = 0; r < NumRows; r++) {
                for (var c = 0; c < NumColumns; c++) {
                    if (bgFrame[r][c] == '>') {
                        var destCol = c + 1;
                        while (destCol >= NumColumns) {
                            destCol -= NumColumns;
                        }
                        if (bgFrame[r][destCol] == '.') {
                            changeCount++;
                            changes.Enqueue(new Delta(c, r, '.'));
                            changes.Enqueue(new Delta(destCol, r, '>'));
                        }                        
                    }
                }
            }
            foreach (var d in changes) {
                bgFrame[d.row][d.col] = d.value;
            }
            changes.Clear();
            for (var r = 0; r < NumRows; r++) {
                for (var c = 0; c < NumColumns; c++) {
                    if (bgFrame[r][c] == 'v') {
                        var destRow = r + 1;
                        while (destRow >= NumRows) {
                            destRow -= NumRows;
                        }
                        var already = ValueInDeltas(c, destRow, changes);
                        if ((bgFrame[destRow][c] == '.')&&(already != '>')) {
                            changeCount++;
                            changes.Enqueue(new Delta(c, r, '.'));
                            changes.Enqueue(new Delta(c, destRow, 'v'));
                        }
                    }
                }
            }
            foreach (var d in changes) {
                bgFrame[d.row][d.col] = d.value;
            }
            CopyFrame(bgFrame, fgFrame);
            return changeCount;
        }

        static char? ValueInDeltas(int c, int r, Queue<Delta> changes) {
            char? result = null;
            foreach (var d in changes) {
                if (d.col == c && d.row == r) {
                    result = d.value;
                }
            }
            return result;
        }

        static void Display() {
            Console.WriteLine();
            foreach (var row in fgFrame) {
                foreach (var val in row) {
                    Console.Write(val);
                }
                Console.WriteLine();
            }
        }
    }
}
