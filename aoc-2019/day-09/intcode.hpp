#pragma once

#include <cassert>
#include <condition_variable>
#include <deque>
#include <map>
#include <fstream>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>
#include <boost/multiprecision/cpp_int.hpp>

using boost::multiprecision::cpp_int;

/* Supported Intcode instructions. */
enum class Inst {
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

enum class ParameterMode {
    POSITION = 0, IMMEDIATE, RELATIVE
};

/* An Opcode stores the operation to execute and the associated parameter modes. */
struct Opcode {
    Inst op;
    ParameterMode a_mode;
    ParameterMode b_mode;
    ParameterMode c_mode;

    explicit Opcode(cpp_int x)
            : op(Inst{(int) x % 100}),
              a_mode(ParameterMode{(int) (x / 100) % 10}),
              b_mode(ParameterMode{(int) (x / 1000) % 10}),
              c_mode(ParameterMode{(int) (x / 10000) % 10}) {
    }
};

/* Read a program from file, return as a vector of intcodes. */
std::vector<cpp_int> read_program(const std::string& filename);

template<typename Data>
class IOQueue {
private:
    std::mutex lock;
    std::condition_variable waiting;
    std::deque<Data> data;

public:
    Data pop() {
        std::unique_lock<std::mutex> guard(lock);
        waiting.wait(guard, [=] { return data.size() > 0; });
        Data value = data.front();
        data.pop_front();
        return value;
    }

    void push(Data d) {
        std::lock_guard<std::mutex> guard(lock);
        data.push_back(d);
        waiting.notify_one();
    }

    [[nodiscard]] auto size() const noexcept { return data.size(); }

    [[nodiscard]] const auto& get_data() const noexcept { return data; }
};

class Process {
private:
    cpp_int relative_base = 0;
    std::map<cpp_int, cpp_int> memory;
    std::shared_ptr<IOQueue<cpp_int>> inputs;
    std::shared_ptr<IOQueue<cpp_int>> outputs;
    std::thread executor;

    bool execute_inst(const Opcode& opcode, cpp_int& ip);

public:
    explicit Process(const std::string& filename) : Process(read_program(filename)) {}

    explicit Process(const std::vector<cpp_int>& program)
            : Process(program,
                      std::make_shared<IOQueue<cpp_int>>(),
                      std::make_shared<IOQueue<cpp_int>>()) {
    }

    Process(std::vector<cpp_int> program,
            std::shared_ptr<IOQueue<cpp_int>> inputs,
            std::shared_ptr<IOQueue<cpp_int>> outputs);

    void join() { executor.join(); }

    [[nodiscard]] const auto& get_inputs() const noexcept { return inputs; }

    [[nodiscard]] const auto& get_outputs() const noexcept { return outputs; }
};
