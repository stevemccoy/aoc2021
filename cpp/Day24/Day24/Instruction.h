#pragma once

#include <string>

enum class OpCode {
	OpNone = 0, Inp, Add, Mul, Div, Mod, Eql
};

enum class Reg {
	RegNone = 0, W, X, Y, Z, Direct
};

class Instruction
{
public:
	OpCode op;
	Reg dest;
	Reg src;
	int value;

	static bool OpFromString(std::string input, OpCode& opCode);

	static bool RegFromString(std::string input, Reg& reg, int& value);

};
