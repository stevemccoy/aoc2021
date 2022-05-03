#include <iostream>
#include "Alu.h"

Alu::Alu()
{
    Code.clear();
    reset();
}

void Alu::reset()
{
    IP = W = X = Y = Z = 0;
    while (!Inputs.empty()) {
        Inputs.pop();
    }
}

int Alu::get(Reg r, int value)
{
    switch (r) {
    case Reg::W: return W;
    case Reg::X: return X;
    case Reg::Y: return Y;
    case Reg::Z: return Z;
    case Reg::Direct: return value;
    }
    return 0;
}

void Alu::set(Reg r, int value)
{
    switch (r) {
    case Reg::W: W = value; break;
    case Reg::X: X = value; break;
    case Reg::Y: Y = value; break;
    case Reg::Z: Z = value; break;
    }
}

bool Alu::step()
{
    int dest = 0, src = 0, value = 0;
    const Instruction& instruction = Code[IP];
    switch (instruction.op) {
    case OpCode::Inp:
        if (!Inputs.empty()) {
            set(instruction.dest, Inputs.front());
            Inputs.pop();
        }
        break;
    case OpCode::Add:
        src = get(instruction.src, instruction.value);
        dest = get(instruction.dest);
        set(instruction.dest, src + dest);
        break;
    case OpCode::Mul:
        src = get(instruction.src, instruction.value);
        dest = get(instruction.dest);
        set(instruction.dest, src * dest);
        break;
    case OpCode::Div:
        src = get(instruction.src, instruction.value);
        if (src == 0) {
            return false;
        }
        dest = get(instruction.dest);
        set(instruction.dest, dest / src);
        break;
    case OpCode::Mod:
        src = get(instruction.src, instruction.value);
        dest = get(instruction.dest);
        if ((dest < 0) || (src <= 0)) {
            return false;
        }
        set(instruction.dest, dest % src);
        break;
    case OpCode::Eql:
        src = get(instruction.src, instruction.value);
        dest = get(instruction.dest);
        set(instruction.dest, (src == dest) ? 1 : 0);
        break;
    }
    IP++;
    return true;
}

bool Alu::runProgram(const std::vector<int>& inputs)
{
    reset();
    loadInputs(inputs);
    while (IP < Code.size()) {
        if (!step()) {
            std::cerr << "Program stopped at " << IP << " with an error." << std::endl;
            return false;
        }
    }
    return true;
}

void Alu::loadInputs(const std::vector<int> inputs)
{
    for (auto i : inputs) {
        Inputs.push(i);
    }
}

bool Alu::verifyMonad(const std::vector<int> inputs)
{
    if (runProgram(inputs)) {
        return (Z == 0);
    }
    return false;
}

void Alu::AddInstruction(std::string opCode, std::string dest, std::string src)
{
    Instruction i;
    Instruction::OpFromString(opCode, i.op);
    Instruction::RegFromString(dest, i.dest, i.value);
    Instruction::RegFromString(src, i.src, i.value);
    Code.push_back(i);
}

