#include "intcode.hpp"

#include <utility>

std::vector<cpp_int> read_program(const std::string& filename) {
    std::ifstream in_file{filename};
    std::vector<cpp_int> program;
    while (in_file.good()) {
        char comma;
        cpp_int token;
        in_file >> token;
        in_file >> comma;
        assert(comma == ',' || in_file.eof());
        program.push_back(token);
    }
    return program;
}

cpp_int Process::load(const cpp_int& ip) const {
    return memory.at(ip);
}

void Process::store(const cpp_int& addr, const cpp_int& value) {
    memory[addr] = value;
};

static inline auto read_memory(const std::map<cpp_int, cpp_int>& memory, const cpp_int& where) {
    auto it = memory.find(where);
    if (it == memory.end())
        return cpp_int{0};
    return it->second;
}

/* Load parameter from program memory, using the appropriate mode. */
static inline cpp_int
read_parameter(const std::map<cpp_int, cpp_int>& memory, const cpp_int& ip,
               ParameterMode pmode = ParameterMode::IMMEDIATE,
               cpp_int relative_base = 0) {

    if (pmode == ParameterMode::IMMEDIATE)
        return read_memory(memory, ip);
    else if (pmode == ParameterMode::POSITION)
        return read_memory(memory, read_memory(memory, ip));
    return read_memory(memory, relative_base + read_memory(memory, ip));
}

bool Process::execute_inst(const Opcode& opcode, cpp_int& ip) {
    cpp_int a, b, c;
    switch (opcode.op) {
        // Handle single parameter instructions (I/O).
        case Inst::INP:
            store(read_memory(memory, ip++)
                  + (opcode.a_mode == ParameterMode::POSITION ? 0 : relative_base),
                  inputs->pop());
            return true;
        case Inst::OUT:
            outputs->push(read_parameter(memory, ip++, opcode.a_mode, relative_base));
            return true;
        case Inst::ARB:
            relative_base += read_parameter(memory, ip++, opcode.a_mode, relative_base);
            return true;

            // Handle two-parameter instructions (conditional jumps).
        case Inst::JT:
        case Inst::JNT:
            a = read_parameter(memory, ip++, opcode.a_mode, relative_base);
            b = read_parameter(memory, ip++, opcode.b_mode, relative_base);
            if ((a && opcode.op == Inst::JT) || (!a && opcode.op == Inst::JNT))
                ip = b;
            return true;

            // Handle three-parameter instructions (e.g. c = f(a, b))
        case Inst::ADD:
        case Inst::MUL:
        case Inst::LT:
        case Inst::EQ:
            a = read_parameter(memory, ip++, opcode.a_mode, relative_base);
            b = read_parameter(memory, ip++, opcode.b_mode, relative_base);
            c = read_parameter(memory, ip++);
            store(c + (opcode.c_mode == ParameterMode::RELATIVE ? relative_base : 0), opcode.op == Inst::ADD
                     ? a + b
                     : opcode.op == Inst::MUL
                       ? a * b
                       : ((a < b && opcode.op == Inst::LT)
                          || (a == b && opcode.op == Inst::EQ))
                         ? cpp_int{1}
                         : cpp_int{0});
            return true;
        default:
            return false;  // Halt.
    }
}

Process::Process(std::vector<cpp_int> program,
                 std::shared_ptr<IOQueue<cpp_int>> in,
                 std::shared_ptr<IOQueue<cpp_int>> out)
        : inputs(std::move(in)), outputs(std::move(out)) {

    // Load the program into memory.
    for (int i = 0; i < (int) program.size(); ++i) {
        store(i, program[i]);
//        std::cout << load(i) << " ";
    }
//    std::cout << "read in" << std::endl;

    executor = std::thread{[=]() {
        cpp_int ip = 0;
        while (execute_inst(Opcode{read_memory(memory, ip++)}, ip));
    }};
}
