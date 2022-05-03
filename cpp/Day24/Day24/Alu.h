#pragma once

#include <vector>
#include <queue>
#include "Instruction.h"


class Alu
{
public:
	std::vector<Instruction> Code;
	std::queue<int> Inputs;
	unsigned IP;
	int W, X, Y, Z;

	Alu();
	void reset();
	int get(Reg r, int value = 0);
	void set(Reg r, int value);

	bool step();

	bool runProgram(const std::vector<int>& inputs);

	void loadInputs(const std::vector<int> inputs);

	bool verifyMonad(const std::vector<int> inputs);

	void AddInstruction(std::string opCode, std::string dest, std::string src);

};

