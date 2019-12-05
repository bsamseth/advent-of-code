#include <cassert>
#include <fstream>
#include <iostream>
#include <vector>

struct Opcode
{
    int op;
    int param_a_immediate;
    int param_b_immediate;
    int param_c_immediate;

    Opcode(int x)
        : op(x % 100)
        , param_a_immediate((x % 1000) / 100)
        , param_b_immediate((x % 10000) / 1000)
        , param_c_immediate(x / 10000)
    {
    }
};

int read_parameter(const std::vector<int>& program, int ip, int immediate = 0)
{
    return immediate ? program[ip] : program[program[ip]];
}

int run_program(std::vector<int> program, int io)
{
    int ip = 0;
    while (true)
    {
        Opcode op_code {program[ip++]};

        if (op_code.op == 99)
            break;

        if (op_code.op == 3)
        {
            program[program[ip++]] = io;
        }
        else if (op_code.op == 4)
        {
            io = read_parameter(program, ip++, op_code.param_a_immediate);
//            std::cout << "OUTPUT: " << io << "\n";
        }
        else if (op_code.op == 5 || op_code.op == 6)
        {
            int a = read_parameter(program, ip++, op_code.param_a_immediate);
            int b = read_parameter(program, ip++, op_code.param_b_immediate);
            if ((a && op_code.op == 5) || (!a && op_code.op == 6))
                ip = b;
        }
        else if (op_code.op == 7 || op_code.op == 8)
        {
            int a = read_parameter(program, ip++, op_code.param_a_immediate);
            int b = read_parameter(program, ip++, op_code.param_b_immediate);
            int c = read_parameter(program, ip++, 1);

            program[c]
                = ((a < b && op_code.op == 7) || (a == b && op_code.op == 8)) ? 1 : 0;
        }
        else
        {
            int a = read_parameter(program, ip++, op_code.param_a_immediate);
            int b = read_parameter(program, ip++, op_code.param_b_immediate);
            int c = read_parameter(program, ip++, 1);

            if (op_code.op == 1)
                program[c] = a + b;
            else  // op_code.op == 2
                program[c] = a * b;
        }
    }
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
    std::cout << "Part 1: " << run_program(program, 1) << std::endl;
    std::cout << "Part 2: " << run_program(program, 5) << std::endl;
}
