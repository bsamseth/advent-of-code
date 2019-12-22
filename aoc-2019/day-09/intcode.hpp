#pragma once

#include <cassert>
#include <condition_variable>
#include <deque>
#include <fstream>
#include <iostream>
#include <map>
#include <mutex>
#include <thread>
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
    ARB,  // Add to relative base.
    HLT = 99
};

enum class ParameterMode
{
    POSITION = 0,
    IMMEDIATE,
    RELATIVE
};

/* An Opcode stores the operation to execute and the associated parameter modes. */
template <typename Data>
struct Opcode
{
    Inst op;
    ParameterMode a_mode;
    ParameterMode b_mode;
    ParameterMode c_mode;

    explicit Opcode(Data x)
        : op(Inst {(int) x % 100})
        , a_mode(ParameterMode {(int) (x / 100) % 10})
        , b_mode(ParameterMode {(int) (x / 1000) % 10})
        , c_mode(ParameterMode {(int) (x / 10000) % 10})
    {
    }
};

/* Read a program from file, return as a vector of intcodes. */
template <typename Data>
std::vector<Data> read_program(const std::string& filename)
{
    std::ifstream in_file {filename};
    std::vector<Data> program;
    while (in_file.good())
    {
        char comma;
        Data token;
        in_file >> token;
        in_file >> comma;
        assert(comma == ',' || in_file.eof());
        program.push_back(token);
    }
    return program;
}

template <typename Data>
class IOQueue
{
private:
    std::mutex lock;
    std::condition_variable waiting;
    std::deque<Data> data;

public:
    Data pop()
    {
        std::unique_lock<std::mutex> guard(lock);
        waiting.wait(guard, [=] { return data.size() > 0; });
        Data value = data.front();
        data.pop_front();
        return value;
    }

    void push(Data d)
    {
        std::lock_guard<std::mutex> guard(lock);
        data.push_back(d);
        waiting.notify_one();
    }

    [[nodiscard]] auto size() const noexcept { return data.size(); }

    [[nodiscard]] const auto& get_data() const noexcept { return data; }
};

template <typename Data>
static inline auto read_memory(const std::map<Data, Data>& memory, const Data& where)
{
    auto it = memory.find(where);
    if (it == memory.end())
        return Data {0};
    return it->second;
}

/* Load parameter from program memory, using the appropriate mode. */
template <typename Data>
static inline Data read_parameter(const std::map<Data, Data>& memory,
                                  const Data& ip,
                                  ParameterMode pmode = ParameterMode::IMMEDIATE,
                                  Data relative_base = Data {0})
{
    if (pmode == ParameterMode::IMMEDIATE)
        return read_memory(memory, ip);
    else if (pmode == ParameterMode::POSITION)
        return read_memory(memory, read_memory(memory, ip));
    return read_memory<Data>(memory, relative_base + read_memory<Data>(memory, ip));
}

template <typename Data>
class Process
{
private:
    Data relative_base = Data {0};
    std::map<Data, Data> memory;
    std::shared_ptr<IOQueue<Data>> inputs;
    std::shared_ptr<IOQueue<Data>> outputs;
    std::thread executor;

    bool execute_inst(const Opcode<Data>& opcode, Data& ip)
    {
        Data a, b, c;
        switch (opcode.op)
        {
            // Handle single parameter instructions (I/O).
            case Inst::INP:
                memory[read_memory(memory, ip++)
                       + (opcode.a_mode == ParameterMode::POSITION ? Data {0}
                                                                   : relative_base)]
                    = inputs->pop();
                return true;
            case Inst::OUT:
                outputs->push(
                    read_parameter(memory, ip++, opcode.a_mode, relative_base));
                return true;
            case Inst::ARB:
                relative_base
                    += read_parameter(memory, ip++, opcode.a_mode, relative_base);
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
                memory[c
                       + (opcode.c_mode == ParameterMode::RELATIVE ? relative_base
                                                                   : Data {0})]
                    = opcode.op == Inst::ADD
                          ? a + b
                          : opcode.op == Inst::MUL
                                ? a * b
                                : ((a < b && opcode.op == Inst::LT)
                                   || (a == b && opcode.op == Inst::EQ))
                                      ? Data {1}
                                      : Data {0};
                return true;
            default: return false;  // Halt.
        }
    }

public:
    explicit Process(const std::string& filename)
        : Process(read_program<Data>(filename))
    {
    }

    explicit Process(const std::vector<Data>& program)
        : Process(program,
                  std::make_shared<IOQueue<Data>>(),
                  std::make_shared<IOQueue<Data>>())
    {
    }

    Process(std::vector<Data> program,
            std::shared_ptr<IOQueue<Data>> in,
            std::shared_ptr<IOQueue<Data>> out)
        : inputs(std::move(in)), outputs(std::move(out))
    {
        // Load the program into memory.
        for (std::size_t i = 0; i < program.size(); ++i)
            memory[i] = program[i];

        executor = std::thread {[=]() {
            Data ip = 0;
            while (execute_inst(Opcode {read_memory(memory, ip++)}, ip))
                ;
        }};
    }

    void join() { executor.join(); }

    [[nodiscard]] const auto& get_inputs() const noexcept { return inputs; }

    [[nodiscard]] const auto& get_outputs() const noexcept { return outputs; }
};
