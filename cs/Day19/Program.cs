using System;
using System.IO;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Day19
{
    public class Coordinate 
    {
        public int X, Y, Z;

        public Coordinate(int argx = 0, int argy = 0, int argz = 0) {
            X = argx;
            Y = argy;
            Z = argz;
        }

        public Coordinate RotateX(int steps = 1)
        {
            var (ytemp, ztemp) = (Y, Z);
            while (steps > 0) {
                (ytemp, ztemp) = (-1 * ztemp, ytemp);
                steps--;
            }
            return new Coordinate(X, ytemp, ztemp);
        }
        
        public Coordinate RotateY(int steps = 1)
        {
            var (xtemp, ztemp) = (X, Z);
            while (steps > 0) {
                (xtemp, ztemp) = (-1 * ztemp, xtemp);
                steps--;
            }
            return new Coordinate(xtemp, Y, ztemp);
        }
        
        public Coordinate RotateZ(int steps = 1)
        {
            var (xtemp, ytemp) = (X, Y);
            while (steps > 0) {
                (xtemp, ytemp) = (-1 * ytemp, xtemp);
                steps--;
            }
            return new Coordinate(xtemp, ytemp, Z);
        }

        public Coordinate Rotate(int xsteps, int ysteps, int zsteps) {
            return RotateX(xsteps).RotateY(ysteps).RotateZ(zsteps);
        }

        public Coordinate Subtract(Coordinate other) {
            return new Coordinate(X - other.X, Y - other.Y, Z - other.Z);
        }

        public Coordinate Add(Coordinate other) {
            return new Coordinate(X + other.X, Y + other.Y, Z + other.Z);
        }

        public bool Equals(Coordinate other) {
            return (X == other.X)&&(Y == other.Y)&&(Z == other.Z);
        }

        public override string ToString() {
            return $"{X},{Y},{Z}";
        }

    }

    public class BeaconSet {
        public List<Coordinate> Items;

        public BeaconSet() {
            Items =  new List<Coordinate>();
        }

        public bool Add(Coordinate c) {
            if (!Contains(c)) {
                Items.Add(c);
                return true;
            }
            return false;
        }

        public bool Contains(Coordinate c) {
            var index = Items.FindIndex(i => i.Equals(c));
            return (index != -1);
        }

        public BeaconSet Orientation(int xsteps, int ysteps, int zsteps) {
            var result = new BeaconSet();
            Items.ForEach(c => result.Add(c.Rotate(xsteps, ysteps, zsteps)));
            return result;
        }

        public int Merge(BeaconSet other) {
            int count = 0;
            other.Items.ForEach(i => { if (Add(i)) { count++; } } );
            return count;
        }

        public BeaconSet AddOffset(Coordinate offset) {
            var result = new BeaconSet();
            Items.ForEach(i => result.Add(i.Add(offset)));
            return result;
        }

        public BeaconSet IntersectWith(BeaconSet other) {
            var result = new BeaconSet();
            Items.ForEach(i => { if (other.Contains(i)) { result.Add(i); }});
            return result;
        }




    }

    public class ScannerReadings
    {
        public int ScannerId;
        public BeaconSet Beacons;

        public ScannerReadings(int Id) {
            ScannerId = Id;
            Beacons = new BeaconSet();
        }

        public BeaconSet Orientation(int xsteps, int ysteps, int zsteps) => Beacons.Orientation(xsteps, ysteps, zsteps);

        public override string ToString() {
            string result = $"--- scanner {ScannerId} ---\n";
            foreach (var b in Beacons.Items) {
                result += (b.ToString() + "\n");
            }
            return result;
        }

    }

    public class MatchResult 
    {
        public int MatchCount;
        public Coordinate OffsetPosition;
        public BeaconSet MatchedBeacons;
    }

    public class Program
    {
        static List<ScannerReadings> scanners = new List<ScannerReadings>();

        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021. Day 19.");
//            Console.WriteLine("Part 1 - Input:");
//            Part1_For("day19.txt");
            Console.WriteLine("Part 2 - Input:");
            Part2_For("day19.txt");           
        }

        static void Part1_For(string fileName) 
        {
            scanners.Clear();
            ReadInputFile(fileName);
            var scannersDone = new Dictionary<int, Coordinate>();
            var count = BruteForceMatch(scannersDone);
            System.Console.WriteLine($"There are {count} beacons.");
        }

        static void Part2_For(string fileName) {
            scanners.Clear();
            ReadInputFile(fileName);
            var scannersDone = new Dictionary<int, Coordinate>();
            var count = BruteForceMatch(scannersDone);
            var distance = FindMaxManhattanDistance(scannersDone);
            System.Console.WriteLine($"Maximum Manhattan distance = {distance}");
        }
 
        static void ReadInputFile(string fileName)
        {
            Regex headerRegex = new Regex(@"--- scanner (?<id>\d+) ---", RegexOptions.Compiled);
            ScannerReadings scanner = null;
            using (StreamReader sr = new StreamReader(fileName))
            {
                string line = string.Empty;
                while ((line = sr.ReadLine()) != null)
                {
                    if (line.Length < 1) {
                        continue;
                    }
                    var m = headerRegex.Match(line);
                    if (m.Success) {
                        if (scanner != null) {
                            scanners.Add(scanner);
                        }
                        scanner = new ScannerReadings(int.Parse(m.Groups["id"].Value));
                        continue;
                    }
                    var coords = line.Split(",");
                    if (coords.Length == 3) {
                        var c = new Coordinate(int.Parse(coords[0]), int.Parse(coords[1]), int.Parse(coords[2]));
                        scanner.Beacons.Add(c);
                    }
                }
                if (scanner != null) {
                    scanners.Add(scanner);
                }
            }
        }

        static int BruteForceMatch(Dictionary<int, Coordinate> ScannersDone) {
            // Fix known point of view from the first scanner in the input.
            var KnownBeacons = new BeaconSet();
            KnownBeacons.Merge(scanners[0].Beacons);
            // Result is a set of fixed scanner coordinates.
            ScannersDone[0] = new Coordinate(0, 0, 0);
            int[] legalRotations = {0, 1, 2, 3};

            // Attempt match for each of the remaining scanners.
            bool change = true;
            while (change && ScannersDone.Count < scanners.Count) {
                // Determine if any change is made in this sweep.
                change = false;
                foreach (var scanner in scanners) {
                    if (ScannersDone.ContainsKey(scanner.ScannerId)) {
                        continue;
                    }
                    bool matchFound = false;
                    foreach (var rotx in legalRotations) {
                        foreach (var roty in legalRotations) {
                            foreach (var rotz in legalRotations) {
                                if (matchFound) {
                                    continue;
                                }
                                var readings = scanner.Orientation(rotx, roty, rotz);
                                var tempSensor = new ScannerReadings(scanner.ScannerId) {
                                    Beacons = readings };

//                                System.Console.WriteLine(tempSensor);
                                var match = Match(KnownBeacons, readings);

                                if (match.MatchCount >= 12) {
                                    ScannersDone[scanner.ScannerId] = match.OffsetPosition;
                                    KnownBeacons.Merge(readings.AddOffset(match.OffsetPosition));
                                    matchFound = true;
                                    change = true;
                                }
                            }
                        }
                    }
                }
            }
            return KnownBeacons.Items.Count;
        }

        static MatchResult Match(BeaconSet knownBeacons, BeaconSet readings) {
            int numMatches = 0;
            var matchedBeacons = new BeaconSet();
            var resultOffset = new Coordinate(0, 0, 0);
            foreach (var r in readings.Items) {
                foreach (var b in knownBeacons.Items) {
                    // Candidate match - possible coordinates of sensor referred to known beacons.
                    var offset = b.Subtract(r);
                    var correctedReadings = readings.AddOffset(offset);
                    var iset = correctedReadings.IntersectWith(knownBeacons);
                    if (iset.Items.Count > numMatches) {
                        numMatches = iset.Items.Count;
                        matchedBeacons = iset;
                        resultOffset = offset;
                    }
                }
            }
            return new MatchResult 
            {   MatchCount = numMatches,
                OffsetPosition = resultOffset, 
                MatchedBeacons = matchedBeacons  
            };
        }

        static int FindMaxManhattanDistance(Dictionary<int, Coordinate> scannersDone) {
            int distance = 0;
            int maxDistance = 0;
            foreach (var s1 in scannersDone) {
                foreach (var s2 in scannersDone) {
                    if (s1.Key < s2.Key) {
                        distance = ManhattanDistance(s1.Value, s2.Value);
                        if (distance > maxDistance) {
                            maxDistance = distance;
                        }
                    }
                }
            }
            return maxDistance;
        }

        static int ManhattanDistance(Coordinate from, Coordinate to) {
            return Math.Abs(from.X - to.X) + Math.Abs(from.Y - to.Y) + Math.Abs(from.Z - to.Z);
        }

/*
        static List<Coordinate> SubtractOffset(List<Coordinate> coords, Coordinate offset) {
            var result = new List<Coordinate>();
            foreach (var c in coords) {
                result.Add(c.Subtract(offset));
            }
            return result;
        }

        static void TestRotations() {
            var scanner = new ScannerReadings(0) {
                Beacons = new List<Coordinate>() {
                new Coordinate(-1,-1,1),
                new Coordinate(-2,-2,2),
                new Coordinate(-3,-3,3),
                new Coordinate(-2,-3,1),
                new Coordinate(5,6,-4),
                new Coordinate(8,0,7)
                }
            };

            System.Console.WriteLine(scanner);
            scanner.Beacons = scanner.Orientation(0,1,0);
            System.Console.WriteLine(scanner);



        }

        static void TestMatch() {

            var from = new Coordinate(686,422,578);
            var to = new Coordinate(-618,-824,-621);
            


        }
*/
    }
}
