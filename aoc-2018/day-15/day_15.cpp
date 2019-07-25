#include <algorithm>
#include <array>
#include <deque>
#include <fstream>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

struct Coord
{
    int x;
    int y;

    Coord(int x_, int y_) : x(x_), y(y_) {}
};

bool operator<(const Coord& a, const Coord& b)
{
    return std::tie(a.y, a.x) < std::tie(b.y, b.x);
}

const std::array<Coord, 4> directions
    = {Coord {0, -1}, Coord {-1, 0}, Coord {1, 0}, Coord {0, 1}};

enum UnitType
{
    ELF,
    GOBLIN,
};

char UnitSymbol[2] = {'E', 'G'};

struct Unit
{
    UnitType type;
    int x;
    int y;
    int hp = 200;
    int ap = 3;

    Unit(UnitType t, int col, int row) : type(t), x(col), y(row) {}
};

bool operator<(const Unit& a, const Unit& b)
{
    return std::tie(a.y, a.x) < std::tie(b.y, b.x);
}

struct Action
{
    int dx;
    int dy;
    int att_dx;
    int att_dy;
} NullAction {0, 0, 0, 0};

struct Grid
{
    std::vector<std::vector<char>> grid;
    std::vector<Unit> units;
    std::array<int, 2> type_counts = {0, 0};

    Grid(std::string&& filename)
    {
        std::ifstream input_file(filename);
        std::string line;
        int y = 0;
        while (std::getline(input_file, line))
        {
            std::vector<char> row;
            for (int x = 0; x < (int) line.size(); ++x)
            {
                char c = line[x];
                row.push_back(c);
                if (c == 'E')
                {
                    units.emplace_back(UnitType::ELF, x, y);
                    ++type_counts[UnitType::ELF];
                }
                else if (c == 'G')
                {
                    units.emplace_back(UnitType::GOBLIN, x, y);
                    ++type_counts[UnitType::GOBLIN];
                }
            }
            ++y;
            grid.push_back(row);
        }

        input_file.close();
    }

    Unit& unit_at(const Coord& coord)
    {
        for (Unit& unit : units)
            if (unit.x == coord.x && unit.y == coord.y)
                return unit;
    }

    Action decide_action(const Unit& unit)
    {
        // Decide on move to make.
        char enemy = UnitSymbol[!unit.type];
        Coord cur_move = {2, 2};  // Move to make (init. move larger than possible.
        int min_dist = 1000000;   // Distance greater than any distance.
        std::vector<std::vector<bool>> visited {
            grid.size(), std::vector<bool>(grid[0].size(), false)};
        std::deque<std::tuple<int, int, int, int, int>>
            queue;  // step count, move x, move y, pos x, pos y

        // First, check if we need to move at all.
        for (Coord dir : directions)
            if (grid[unit.y + dir.y][unit.x + dir.x] == enemy)
            {
                cur_move = Coord {0, 0};
                goto move_determined;
            }

        // Not already adjacent to enemy - search for move to make.

        // Put all adjacent coords in the queue, ready to search.
        visited[unit.y][unit.x] = true;
        for (Coord dir : directions)
        {
            Coord next {unit.x + dir.x, unit.y + dir.y};
            if (grid[next.y][next.x] == '.')
            {
                queue.emplace_back(1, dir.x, dir.y, next.x, next.y);
                visited[next.y][next.x] = true;
            }
        }

        while (!queue.empty())
        {
            auto [step_count, dx, dy, x, y] = queue.front();
            queue.pop_front();

            // queue in increasing order of step count, stop if closer enemy is found
            // anyway.
            if (step_count > min_dist)
                break;

            for (Coord dir : directions)
            {
                Coord next {x + dir.x, y + dir.y};
                if (grid[next.y][next.x] == enemy)
                {
                    min_dist = step_count;  // min_dist >= step_count by construction.
                    cur_move = std::min(
                        cur_move,
                        Coord {dx, dy});  // Keep the first move, in reading order.
                }
                else if (grid[next.y][next.x] == '.' && !visited[next.y][next.x])
                {
                    visited[next.y][next.x] = true;
                    queue.emplace_back(step_count + 1, dx, dy, next.x, next.y);
                }
            }
        }

    move_determined:

        Action action {cur_move.x, cur_move.y, 0, 0};

        Coord dest {unit.x + action.dx, unit.y + action.dy};
        std::vector<std::tuple<int, Coord>> attacks;
        for (Coord dir : directions)
        {
            Coord next {dest.x + dir.x, dest.y + dir.y};
            if (grid[next.y][next.x] == enemy)
            {
                attacks.emplace_back(unit_at(next).hp, dir);
            }
        }
        if (attacks.size())
        {
            std::sort(attacks.begin(), attacks.end());
            action.att_dx = std::get<1>(attacks[0]).x;
            action.att_dy = std::get<1>(attacks[0]).y;
        }

        return action;
    }

    void apply_action(const Action& action, Unit& unit)
    {
        if (action.dx != 0 || action.dy != 0)
        {
            if (grid[unit.y + action.dy][unit.x + action.dx] == '.')
            {
                grid[unit.y][unit.x] = '.';
                unit.x += action.dx;
                unit.y += action.dy;
                grid[unit.y][unit.x] = UnitSymbol[unit.type];
            }
        }

        if (action.att_dx != 0 || action.att_dy != 0)
        {
            Unit& attacked
                = unit_at(Coord {unit.x + action.att_dx, unit.y + action.att_dy});
            attacked.hp -= unit.ap;
        }
    }

    bool iterate()
    {
        std::sort(units.begin(), units.end());
        std::vector<std::tuple<Unit*, Action>> actions;
        for (auto& unit : units)
            actions.emplace_back(&unit, decide_action(unit));

        for (auto [unit, action] : actions)
            apply_action(action, *unit);

        std::remove_if(units.begin(), units.end(), [&](const Unit& unit) {
            if (unit.hp <= 0)
            {
                --type_counts[unit.type];
                grid[unit.y][unit.x] = '.';
            }
            return unit.hp <= 0;
        });

        return type_counts[UnitType::ELF] && type_counts[UnitType::GOBLIN];
    }

    friend std::ostream& operator<<(std::ostream&, const Grid&);
};

std::ostream& operator<<(std::ostream& stm, const Grid& grid)
{
    for (const auto& row : grid.grid)
    {
        for (auto c : row)
            stm << c;
        stm << '\n';
    }
    return stm;
}

int main()
{
    Grid grid("testinput.txt");

    std::cout << grid << "\n";

    for (int i = 1; i <= 50; ++i) {
        bool not_done = grid.iterate();
        std::cout << "After round " << i << "\n" << grid << "\n";

        if (!not_done)
            break;
    }
}
