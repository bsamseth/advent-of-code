#include <cassert>
#include <fstream>
#include <iostream>
#include <vector>
#include <mutex>
#include <condition_variable>
#include <thread>
#include <deque>

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

class Process
{
    private:
        std::vector<int> program;
        std::mutex lock;
        std::thread executor;
        std::condition_variable needs_input;
        std::condition_variable needs_output;

        std::deque<int> inputs;
        std::deque<int> outputs;

        bool execute_inst(std::vector<int>& program, const Opcode& opcode, int& ip);

    public:
        Process(const std::string& filename) : Process(read_program(filename)) {}
        Process(const std::vector<int>& prog);

        void send_input(int);
        void send_output(int);
        int get_input();
        int get_output();
        void join() { executor.join(); }
        int output_count() { return outputs.size(); }
};
