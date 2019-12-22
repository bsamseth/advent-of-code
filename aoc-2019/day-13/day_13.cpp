/*
 * Part 2 for this works only when run on input-hacked.txt.
 *
 * That is because in that input file, I've modified the entire
 * row where the paddle is located to be paddles. This effectively
 * makes it impossible for the ball not to hit the paddle, so
 * there is no need to control the joystick with any logic. So
 * we just supply zeros as inputs all the time (meaning don't move
 * the paddle) and let the game run.
 *
 * With printing disabled, this runs everything in about 350 milliseconds.
 */
#include "intcode.hpp"

#include <algorithm>
#include <boost/multiprecision/cpp_int.hpp>
#include <iostream>
#include <map>
#include <memory>

using Data = boost::multiprecision::cpp_int;  // int;
using Point = std::pair<Data, Data>;

enum class TileId
{
    EMPTY = 0,
    WALL,
    BLOCK,
    HPADDLE,
    BALL
};

std::string read_tile(const std::map<Point, TileId>& screen, Point&& where)
{
    auto it = screen.find(where);
    switch (it == screen.end() ? TileId::EMPTY : it->second)
    {
        case TileId::EMPTY: return " ";
        case TileId::WALL: return "█";
        case TileId::BLOCK: return "★";
        case TileId::HPADDLE: return "▂";
        case TileId::BALL: return "●";
    }
    assert(false && "What???");
}

// Make an input queue that always returns the same number, without waiting.
class ConstantInput : public IOQueue<Data>
{
    Data constant;

public:
    ConstantInput(Data value) : constant(value) {}

    Data pop() override { return constant; }
};

void draw(const std::map<Point, TileId>& screen, int score, bool final = false)
{
    Point xlim {0, 41};  // Change if input has different dimensions.
    Point ylim {0, 23};
    for (auto y = ylim.first; y <= ylim.second; ++y)
    {
        for (auto x = xlim.first; x <= xlim.second; ++x)
        {
            std::cout << read_tile(screen, Point {x, y});
        }
        std::cout << '\n';
    }
    std::cout << "Score: " << score << '\n';
    if (!final)
        std::cout << "\033[25A\r";
}

int main()
{
    auto assembly = read_program<Data>("input-hacked.txt");
    {
        auto inputs = std::make_shared<IOQueue<Data>>();
        auto outputs = std::make_shared<IOQueue<Data>>();
        Process<Data> engine {assembly, inputs, outputs};

        std::map<Point, TileId> screen;
        while (true)
        {
            auto maybe_x = outputs->pop_if_alive(engine);

            if (!maybe_x.has_value())
                break;

            Point pixel {*maybe_x, outputs->pop()};
            TileId id {(int) outputs->pop()};
            screen[pixel] = TileId {id};
        }
        engine.join();

        std::cout << "Part 1: "
                  << std::count_if(screen.begin(),
                                   screen.end(),
                                   [](const auto& entry) {
                                       return entry.second == TileId::BLOCK;
                                   })
                  << std::endl;
    }

    {
        assembly[0] = 2;  // Play for free!
        auto inputs = std::make_shared<ConstantInput>(0);
        auto outputs = std::make_shared<IOQueue<Data>>();
        Process<Data> engine {assembly, inputs, outputs};

        std::map<Point, TileId> screen;
        int score = 0;
        while (true)
        {
            auto maybe_x = outputs->pop_if_alive(engine);

            if (!maybe_x.has_value())
                break;

            Point pixel {*maybe_x, outputs->pop()};
            Data id_raw = outputs->pop();

            if (pixel == Point {-1, 0})
            {
                score = (int) id_raw;
            }
            else
            {
                TileId tile {(int) id_raw};
                screen[pixel] = tile;

                if (tile == TileId::BALL)
                {
                    draw(screen, score);
                    std::this_thread::sleep_for(std::chrono::milliseconds(1));
                }
            }
        }
        draw(screen, score, true);
        engine.join();
    }
}
