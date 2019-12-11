#include "intcode.hpp"

std::vector<int> read_program(const std::string& filename)
{
    std::ifstream in_file {filename};
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
    return program;
}

/* Load parameter from program memory, using the appropriate mode. */
static inline int read_parameter(const std::vector<int>& program, int ip, bool immediate = true)
{
    return immediate ? program[ip] : program[program[ip]];
}

bool Process::execute_inst(std::vector<int>& program, const Opcode& opcode, int& ip)
{
    int a, b, c;
    switch (opcode.op)
    {
        // Handle single parameter instructions (I/O).
        case Inst::INP: program[program[ip++]] = get_input(); return true;
        case Inst::OUT:
            send_output(read_parameter(program, ip++, opcode.a_immediate));
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


Process::Process(const std::vector<int>& prog) : program(prog), executing(true)
{
    executor = std::thread{[=]()
        {
            int ip = 0;
            while (execute_inst(program, Opcode{program[ip++]}, ip));
        }
    };
}

/* static void print(std::string s, const std::deque<int>& d) */
/* { */
/*     std::cout << s << ":  "; */
/*     for (auto e : d) */
/*         std::cout << e << " "; */
/*     std::cout << std::endl; */
/* } */

void Process::send_input(int input)
{
    std::lock_guard<std::mutex> guard(lock);
    inputs.push_back(input);
    needs_input.notify_one();
}

int Process::get_output()
{
    std::unique_lock<std::mutex> guard(lock);

    needs_output.wait(guard, [=]{ return outputs.size() > 0; });

    int value = outputs.front();
    outputs.pop_front();

    return value;
}

void Process::send_output(int output)
{
    std::lock_guard<std::mutex> guard(lock);
    outputs.push_back(output);
    needs_output.notify_one();
}

int Process::get_input()
{
    std::unique_lock<std::mutex> guard(lock);

    needs_input.wait(guard, [=]{ return inputs.size() > 0; });

    int value = inputs.front();
    inputs.pop_front();

    return value;
}
