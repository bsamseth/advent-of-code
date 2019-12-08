#include <cassert>
#include <fstream>
#include <iostream>
#include <vector>

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
bool execute_inst(std::vector<int>& program, const Opcode& opcode, int& ip, int& io);

/* Read a program from file, return as a vector of intcodes. */
std::vector<int> read_program(std::string&& filename);
