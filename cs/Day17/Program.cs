using System;

namespace Day17
{
    public class State {
        public int x, y, vx, vy;
    }

    public class Target {
        public int from_x, to_x, from_y, to_y;

        public bool Hit(int x, int y) => ((x >= from_x)&&(x <= to_x)&&(y >= from_y)&&(y <= to_y));
    }

    class Program
    {
        static Target target = new Target() {from_x = 150, to_x = 193, from_y = -136, to_y = -86};

        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021. Day 17");
            Console.Write("Please select which part (1 or 2) : ");
            var line = Console.ReadLine();
            if (int.Parse(line) == 1) {
                do_part1();
            }
            else if (int.Parse(line) == 2) {
                do_part2();
            }
            else {
                Console.WriteLine("Suit yourself. Bye!");
            }
        }

        static void do_part1() {
            var vx = 17; 
            var vy = 10;
            while (true) {
                var max_y = 0;
                var hit = Trajectory(vx, vy, out max_y);
                if (!hit) {
                    Console.WriteLine($"v = ({vx}, {vy}), max_y = {max_y} : MISS!");
                }
                else {
                    Console.WriteLine($"v = ({vx}, {vy}), max_y = {max_y} : HIT!");
                }
                Console.WriteLine("Next vy = ");
                var line = Console.ReadLine();
                if (line.ToLower() == "q") {
                    return;
                }
                vy = int.Parse(line);
            }
        }

        static void do_part2() {
            int max_y = 0;
            int count = 0;
            for (int vx = 15; vx <= 193; vx++) {
                for (int vy = -136; vy <= 140; vy++) {
                    if (Trajectory(vx, vy, out max_y)) {
                        Console.WriteLine($"v = ({vx}, {vy}), max_y = {max_y} : HIT!");
                        count++;
                    }
                }
            }
            Console.WriteLine($"All done. There were {count} trajectories resulting in a hit.");
        }

        static State Step(State from) {
            from.x += from.vx;
            from.y += from.vy;
            if (from.vx < 0) {
                from.vx++;
            }
            else if (from.vx > 0) {
                from.vx--;
            }
            from.vy--;
            return from;
        }

        static bool Trajectory(int start_vx, int start_vy, out int max_y) {
            State s = new State() { x = 0, y = 0, vx = start_vx, vy = start_vy };
            bool hit = false;
            var my = 0;
            while (!hit) {
                if (s.y < target.from_y) {
                    break;
                }
                if (s.y > my) {
                    my = s.y;
                }
                hit = target.Hit(s.x, s.y);
//                Console.Write($"({s.x}, {s.y}, {s.vx}, {s.vy})");
//                Console.WriteLine(hit ? " -> HIT" : string.Empty);
                Step(s);
            }
            max_y = my;
            return hit;
        }
    }
}
