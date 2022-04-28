using System;
using System.IO;
using System.Collections.Generic;

namespace Day24
{
    public enum OpCode {
        Inp, Add, Mul, Div, Mod, Eql, None
    }

    public enum Reg {
        W, X, Y, Z, Direct, None
    }

    public class Instruction {
        public OpCode Op;
        public Reg Dest;
        public Reg Src;
        public int Value;
    }

    public class Alu {
        public List<Instruction> Code;
        public int IP { get; set; }
        public int W { get; set; }
        public int X { get; set; }
        public int Y { get; set; }
        public int Z { get; set; }

        public Queue<int> Inputs = new Queue<int>();

        public Alu() {
            Code = new List<Instruction>();
            Inputs = new Queue<int>();
        }

        public void Reset() {
            IP = 0;
            W = X = Y = Z = 0;
            Inputs.Clear();
        }

        public int Get(Reg r, int value = 0) {
            switch (r) {
                case Reg.W: return W;
                case Reg.X: return X;
                case Reg.Y: return Y;
                case Reg.Z: return Z;
                case Reg.Direct: return value;
                default:    return 0;
            }
        }

        public void Set(Reg r, int value) {
            switch (r) {
                case Reg.W: W = value;
                            break;
                case Reg.X: X = value;
                            break;
                case Reg.Y: Y = value;
                            break;
                case Reg.Z: Z = value;
                            break;
            }
        }

        public bool Step() {
            int dest = 0;
            int src = 0;
            var instruction = Code[IP];
            switch (instruction.Op) {
                case OpCode.Inp:    if (Inputs.TryDequeue(out var value)) {
                                        Set(instruction.Dest, value);
                                    }
                                    break;
                case OpCode.Add:    src = Get(instruction.Src, instruction.Value);
                                    dest = Get(instruction.Dest);
                                    Set(instruction.Dest, src + dest);
                                    break;
                case OpCode.Mul:    src = Get(instruction.Src, instruction.Value);
                                    dest = Get(instruction.Dest);
                                    Set(instruction.Dest, src * dest);
                                    break;
                case OpCode.Div:    src = Get(instruction.Src, instruction.Value);
                                    dest = Get(instruction.Dest);
                                    if (src == 0) {
                                        return false;
                                    }
                                    Set(instruction.Dest, dest / src);                                
                                    break;
                case OpCode.Mod:    src = Get(instruction.Src, instruction.Value);
                                    dest = Get(instruction.Dest);
                                    if ((dest < 0)||(src <= 0)) {
                                        return false;
                                    }
                                    Set(instruction.Dest, dest % src);
                                    break;
                case OpCode.Eql:    src = Get(instruction.Src, instruction.Value);
                                    dest = Get(instruction.Dest);
                                    Set(instruction.Dest, (dest == src) ? 1 : 0);
                                    break;
            }
            IP++;
            return true;
        }

        public void LoadInputs(IEnumerable<int> inputs) {
            foreach (var i in inputs) {
                Inputs.Enqueue(i);
            }
        }

        public bool RunProgram(IEnumerable<int> inputs) {
            Reset();
            LoadInputs(inputs);
            while (IP < Code.Count) {
                if (!Step()) {
                    Console.WriteLine($"Program stopped at {IP} with an error.");
                    return false;
                }
            }
            return true;
        }

        public bool VerifyMonad(IEnumerable<int> inputs) {
            if (RunProgram(inputs)) {
                return (Z == 0);
            }
            return false;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021, Day 24\nPart 1.");
            var alu = ReadInputFile("day24.txt");
            Int64 input = 13579246899999;

            while (true) {
                try {
                    if (alu.VerifyMonad(InputsFromInt64(input))) {
                        break;
                    }
                }
                catch (Exception ex) {
                    if (!ex.Message.StartsWith("Bad character")) {
                        throw ex;
                    }
                }
                input--;
            }

            Console.WriteLine($"Valid model number found: {input}");
        }

        static IEnumerable<int> InputsFromString(string s) {
            foreach (var c in s) {
                var i = c - '0';
                if ((i < 1)||(i > 9)) {
                    throw new Exception($"Bad character {c} in input.");
                }
                yield return i;
            }
        }

        static IEnumerable<int> InputsFromInt64(Int64 value) {
            return InputsFromString(value.ToString());
        }

        static OpCode OpFromString(string input) {
            switch (input.Trim()) {
                case "inp": return OpCode.Inp;
                case "add": return OpCode.Add;
                case "mul": return OpCode.Mul;
                case "div": return OpCode.Div;
                case "mod": return OpCode.Mod;
                case "eql": return OpCode.Eql;
                default: return OpCode.None;
            }
        }

        static bool RegFromString(string input, out Reg reg, out int value) {
            value = 0;
            switch (input.ToLowerInvariant().Trim()) {
                case "w":   reg = Reg.W; break;
                case "x":   reg = Reg.X; break;
                case "y":   reg = Reg.Y; break;
                case "z":   reg = Reg.Z; break;
                default:    reg = Reg.None;
                            if (!int.TryParse(input, out value)) {
                                return false;
                            }
                            reg = Reg.Direct;
                            break;
            }
            return true;
        }

        static Alu ReadInputFile(string fileName)
        {
            var alu = new Alu();
            using (StreamReader sr = new StreamReader(fileName))
            {
                string line = string.Empty;
                while ((line = sr.ReadLine()) != null)
                {
                    List<string> words = new List<string>(line.Split(' '));
                    while (words.Count < 3) {
                        words.Add(string.Empty);
                    }
                    int value1, value2;
                    Reg r1, r2;
                    RegFromString(words[1], out r1, out value1);
                    RegFromString(words[2], out r2, out value2);
                    var i = new Instruction() { Op = OpFromString(words[0]), Dest = r1, Src = r2, Value = value2 };
                    alu.Code.Add(i);
                }
            }
            return alu;
        }
    }
}
