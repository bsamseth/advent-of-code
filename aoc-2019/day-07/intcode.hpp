#pragma once

#include <cassert>
#include <condition_variable>
#include <deque>
#include <fstream>
#include <iostream>
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

/* Read a program from file, return as a vector of intcodes. */
std::vector<int> read_program(const std::string& filename);

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

class Process
{
private:
    std::vector<int> program;
    std::shared_ptr<IOQueue<int>> inputs;
    std::shared_ptr<IOQueue<int>> outputs;
    std::thread executor;
    bool execute_inst(const Opcode& opcode, int& ip);

public:
    explicit Process(const std::string& filename) : Process(read_program(filename)) {}
    explicit Process(const std::vector<int>& prog) : Process(prog, nullptr, nullptr) {}

    Process(std::vector<int> prog,
            std::shared_ptr<IOQueue<int>> inputs,
            std::shared_ptr<IOQueue<int>> outputs);

    void join() { executor.join(); }
};
