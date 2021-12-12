using System;
using System.IO;
using System.Collections.Generic;

namespace Day11
{
    public class Grid
    {
        private int[,] _grid = new int[10,10];
        private int[,] _flashes = new int[10,10];

        public Grid() {
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    _grid[i,j] = 0;
                    _flashes[i,j] = 0;
                }
            }
        }

        public bool Set(int x, int y, int value) {
            if (value < 0) {
                Console.WriteLine($"Set - Bad value at ({x},{y}): {value}");
                return false;
            }
            if ((x < 0)||(x > 9)||(y < 0)||(y > 9)) {
                Console.WriteLine($"Set - Bad coordinate at ({x},{y}): {value}");
                return false;
            }
            _grid[x,y] = value;
            return true;
        }

        public int RunStep() {
            CollectEnergy();
            TriggerFlashes();
            return FinaliseStep();
        }

        private void CollectEnergy() {
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    _grid[i,j]++;
                }
            }
        }

        private void TriggerFlashes() {
            ResetFlashes();
            while (FireFlashes() > 0) {
                SpreadFlashes();
            }
        }

        public int FireFlashes() {
            int count = 0;
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    if (_grid[i,j] > 9) {
                        if (GetFlash(i, j) == 0) {
                            SetFlash(i, j, 1);
                            count++;
                        }
                    }
                }
            }
            return count;
        }

        public int SpreadFlashes() {
            int count = 0;
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    if (GetFlash(i, j) == 1) {
                        Flash(i, j);
                        SetFlash(i, j, 2);
                        count++;
                    }
                }
            }
            return count;
        }

        private int Flash(int x, int y) {
            int flashCount = 0;
            for (var dx = -1; dx < 2; dx++) {
                for (var dy = -1; dy < 2; dy++) {
                    if ((dx == 0)&&(dy == 0)) {
                        continue;
                    }
                    var nx = x + dx;
                    var ny = y + dy;
                    if (GoodCoord(nx, ny))
                    {
                        _grid[nx,ny]++;
                        flashCount++;
                    }
                }
            }
            return flashCount;
        }

        private bool GoodCoord(int x, int y) {
            return ((x >= 0)&&(x < 10)&&(y >= 0)&&(y < 10));
        }

        public int FinaliseStep() {
            var count = 0;
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    if (_grid[i,j] > 9) {
                        _grid[i,j] = 0;
                        count++;
                    }
                }
            }
            return count;
        }

        public void Display() {
            Console.WriteLine();
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    Console.Write(_grid[j,i]);
                }
                Console.WriteLine();
            }
        }        

        public void ResetFlashes() {
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    _flashes[i,j] = 0;
                }
            }
        }

        public int GetFlash(int x, int y) {
            return _flashes[x,y];
        }

        public void SetFlash(int x, int y, int v) {
            _flashes[x,y] = v;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021\nDay 11.");
            var grid = ReadInputFile("input.txt");
            int totalFlashes = 0;
            int count = 0;
            for (int i = 1; i <= 1000; i++)
            {
                count = grid.RunStep();
                if (count == 100) {
                    Console.WriteLine($"Step {i} - all octopuses flashed!");
                    break;
                }
                totalFlashes += count;
            }

            grid.Display();
            Console.WriteLine($"All steps Done. Total Flashes = {totalFlashes}");
        }

        static Grid ReadInputFile(string fileName)
        {
            string line;
            Grid grid = new Grid();
            using (StreamReader sr = new StreamReader(fileName))
            {
                int y = 0;
                while ((line = sr.ReadLine()) != null)
                {
                    for (int x = 0; x < 10; x++) {
                        var v = line[x] - '0';
                        grid.Set(x, y, v);
                    }
                    y++;
                }
            }
            return grid;
        }
    }
}
