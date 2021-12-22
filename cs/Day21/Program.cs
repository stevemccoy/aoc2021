using System;
using System.Collections.Generic;

namespace Day21
{
    public class UState {
        public int P1State { get; set; } 
        public int P1Score { get; set; }
        public int P2State { get; set; }
        public int P2Score { get; set; }
        public int NextToPlay { get; set; }
        public UInt64 NumUniverses { get; set; }

        public UState(int p1State, int p2State) {
            P1Score = P2Score = 0;
            P1State = p1State;
            P2State = p2State;
            NextToPlay = 1;
            NumUniverses = 1;
        }

        public UState Clone() {
            return (UState)MemberwiseClone();
        }

        public void PlayerOneMove(int spaces) {
            P1State = (P1State + spaces - 1) % 10 + 1;
            P1Score += P1State;
            NextToPlay = 2;
        }
        
        public void PlayerTwoMove(int spaces) {
            P2State = (P2State + spaces - 1) % 10 + 1;
            P2Score += P2State;
            NextToPlay = 1;
        }    
    }

    class Program
    {
        static int LastDiceValue = 100;
        static int NumDiceSides = 100;
        static int NumDiceRolls = 0;

        static Dictionary<int,UInt64> MoveFrequencies = new Dictionary<int, UInt64>() {
            {3,1}, {4,3}, {5,6}, {6,7}, {7,6}, {8,3}, {9,1} };

        static void Main(string[] args)
        {
            Console.WriteLine("Advent of Code 2021, Day 21");
            Console.WriteLine("Part 1");
            PlayDeterministic(4, 1);
            Console.WriteLine("Part 2");
            PlayDiracDice(4, 1);
        }

        static int RollDice() {
            LastDiceValue++;
            NumDiceRolls++;
            while (LastDiceValue > NumDiceSides) {
                LastDiceValue -= NumDiceSides;
            }
            return LastDiceValue;
        }

        static void PlayDeterministic(int playerOne, int playerTwo) {
            var playerOneScore = 0;
            var playerTwoScore = 0;
            var done = false;

            while (!done) {
                playerOne += RollDice() + RollDice() + RollDice();
                playerOne = (playerOne - 1) % 10 + 1;
                playerOneScore += playerOne;
                Console.WriteLine($"Player One: {playerOne}, score {playerOneScore}");
                if (playerOneScore >= 1000) {
                    Console.WriteLine($"Player One wins with a score of {playerOneScore}");
                    Console.WriteLine($"Player Two score is {playerTwoScore} and dice have been rolled {NumDiceRolls} times.");
                    Console.WriteLine($"Answer is {playerTwoScore * NumDiceRolls}.");
                    break;
                }

                playerTwo += RollDice() + RollDice() + RollDice();
                playerTwo = (playerTwo - 1) % 10 + 1;
                playerTwoScore += playerTwo;
                Console.WriteLine($"Player Two: {playerTwo}, score {playerTwoScore}");
                if (playerTwoScore >= 1000) {
                    Console.WriteLine($"Player Two wins with a score of {playerTwoScore}");
                    Console.WriteLine($"Player One score is {playerOneScore} and dice have been rolled {NumDiceRolls} times.");
                    Console.WriteLine($"Answer is {playerOneScore * NumDiceRolls}.");
                    break;
                }
            }
        }

        /*
            Part 2 - Using the Dirac dice (3-sided)

            When player takes a turn, they roll dice 3 times in a row and sum the values. Given that dice values are
            in {1,2,3}, the tree of possible universes is 3-ply with a branching factor of 3. Labelling each node with
            the sum results in a distribution across totals {3,4,5,6,7,8,9}, having the following frequencies: 
            {3:1, 4:3, 5:6, 6:7, 7:6, 8:3, 9:1}.

            The player's state and score are the same if the same total is rolled, so keep track of the frequencies
            when generating a tree of outcomes when two players take turns. Actually, use a queue to implement
            effective breadth first search computing numbers of universes at each leaf node and totalising wins for
            each player. Need UInt64 because int overflows quite quickly.
        */
        static void PlayDiracDice(int playerOne, int playerTwo) {
            UInt64 playerOneUniverses = 0;
            UInt64 playerTwoUniverses = 0;
            UState start = new UState(playerOne, playerTwo);
            UState u1 = start;
            Queue<UState> queue = new Queue<UState>();
            queue.Enqueue(start);
            while (queue.TryDequeue(out u1)) {
                if (u1.P1Score >= 21) {
                    // Player one wins
                    playerOneUniverses += u1.NumUniverses;
                    continue;
                }
                if (u1.P2Score >= 21) {
                    // Player two wins
                    playerTwoUniverses += u1.NumUniverses;
                    continue;
                }
                foreach (var spaces in MoveFrequencies.Keys) {
                    if (u1.NextToPlay == 1)
                    {
                        var u2 = u1.Clone();
                        u2.PlayerOneMove(spaces);
                        u2.NumUniverses = u1.NumUniverses * MoveFrequencies[spaces];
                        queue.Enqueue(u2);
                    }
                    else if (u1.NextToPlay == 2) {
                        var u2 = u1.Clone();
                        u2.PlayerTwoMove(spaces);
                        u2.NumUniverses = u1.NumUniverses * MoveFrequencies[spaces];
                        queue.Enqueue(u2);
                    }
                }
            }
            Console.WriteLine("All done.");
            Console.WriteLine($"Player 1 wins in {playerOneUniverses} universes.");
            Console.WriteLine($"Player 2 wins in {playerTwoUniverses} universes.");
        }
    }
}
