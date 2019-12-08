#include <cassert>
#include <fstream>
#include <iostream>
#include <vector>
#include <array>
#include <algorithm>

/* Copied from day 5: */

/* Supported Intcode instructions. */
enum class Inst
{
    ADD = 1,
    MUL,
    INP,
    OUT,
    JT,
    JNT,
    LT,
    EQ,
    HLT = 99
};

/* An Opcode stores the operation to execute and the associated parameter modes. */
struct Opcode
{
    Inst op;
    bool a_immediate;
    bool b_immediate;

    explicit Opcode(int x)
        : op(Inst {x % 100})
        , a_immediate((x % 1000) / 100)
        , b_immediate((x % 10000) / 1000)
    {
    }
};

/* Load parameter from program memory, using the appropriate mode. */
inline int read_parameter(const std::vector<int>& program, int ip, bool immediate = true)
{
    return immediate ? program[ip] : program[program[ip]];
}

/*
 * Execute an instruction, updating ip and io appropriately.
 *
 * Return is false if instruction is a halt, true otherwise.
 */
bool execute_inst(std::vector<int>& program, const Opcode& opcode, int& ip, int& io)
{
    int a, b, c;
    switch (opcode.op)
    {
        // Handle single parameter instructions (I/O).
        case Inst::INP: program[program[ip++]] = io; return true;
        case Inst::OUT:
            io = read_parameter(program, ip++, opcode.a_immediate);
            return true;

        // Handle two-parameter instructions (conditional jumps).
        case Inst::JT:
        case Inst::JNT:
            a = read_parameter(program, ip++, opcode.a_immediate);
            b = read_parameter(program, ip++, opcode.b_immediate);
            if ((a && opcode.op == Inst::JT) || (!a && opcode.op == Inst::JNT))
                ip = b;
            return true;

        // Handle three-parameter instructions (e.g. c = f(a, b))
        case Inst::ADD:
        case Inst::MUL:
        case Inst::LT:
        case Inst::EQ:
            a = read_parameter(program, ip++, opcode.a_immediate);
            b = read_parameter(program, ip++, opcode.b_immediate);
            c = read_parameter(program, ip++);
            program[c] = opcode.op == Inst::ADD
                             ? a + b
                             : opcode.op == Inst::MUL
                                   ? a * b
                                   : ((a < b && opcode.op == Inst::LT)
                                      || (a == b && opcode.op == Inst::EQ))
                                         ? 1
                                         : 0;
            return true;
        default:
            return false;  // Halt.
    }
}

/* START OF DAY 7 */

/*
 * Run the program on an amplifier with a given phase and input, returning the output.
 *
 * Note that the program is taken by value, so that any modifications
 * do not affect the original program.
 */
inline int run_amplifier(std::vector<int> program, int phase, int io)
{
    int ip = 0;
    // Execute the first input instruction, with phase as IO.
    execute_inst(program, Opcode{program[ip++]}, ip, phase);

    // Now execute the remaining program with input set to io.
    while (execute_inst(program, Opcode {program[ip++]}, ip, io)) {}
    return io;
}

int main()
{
    std::ifstream in_file {"input.txt"};
    std::vector<int> program;
    while (in_file.good())
    {
        char comma;
        int token;
        in_file >> token;
        in_file >> comma;
        assert(comma == ',' || in_file.eof());
        program.push_back(token);
    }


    int best = 0;
    std::array<int, 5> phases = {0, 1, 2, 3, 4};
    do {
        int io = 0;
        for (int p : phases)
            io = run_amplifier(program, p, io);
        best = std::max(best, io);
    }
    while (std::next_permutation(phases.begin(), phases.end()));

    std::cout << "Part 1: " << best << std::endl;
}
