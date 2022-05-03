#include "Instruction.h"

bool Instruction::OpFromString(std::string input, OpCode& opCode)
{
    if (input == "inp") {
        opCode = OpCode::Inp;
    }
    else if (input == "add") {
        opCode = OpCode::Add;
    }
    else if (input == "mul") {
        opCode = OpCode::Mul;
    }
    else if (input == "div") {
        opCode = OpCode::Div;
    }
    else if (input == "mod") {
        opCode = OpCode::Mod;
    }
    else if (input == "eql") {
        opCode = OpCode::Eql;
    }
    else {
        opCode = OpCode::OpNone;
    }
    return true;
}

bool Instruction::RegFromString(std::string input, Reg& reg, int& value)
{
    value = 0;
    if (input.empty()) {
        reg = Reg::RegNone;
    }
    else if (input == "w") {
        reg = Reg::W;
    }
    else if (input == "x") {
        reg = Reg::X;
    }
    else if (input == "y") {
        reg = Reg::Y;
    }
    else if (input == "z") {
        reg = Reg::Z;
    }
    else {
        reg = Reg::Direct;
        value = std::stoi(input);
    }
    return true;
}
