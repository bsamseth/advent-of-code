
## --- Day 24: Blizzard Basin ---

With everything replanted for next year (and with elephants and monkeys to tend the grove), you and the Elves leave for the extraction point.

Partway up the mountain that shields the grove is a flat, open area that serves as the extraction point. It's a bit of a climb, but nothing the expedition can't handle.

At least, that would normally be true; now that the mountain is covered in snow, things have become more difficult than the Elves are used to.

As the expedition reaches a valley that must be traversed to reach the extraction site, you find that strong, turbulent winds are pushing small **blizzards** of snow and sharp ice around the valley. It's a good thing everyone packed warm clothes! To make it across safely, you'll need to find a way to avoid them.

Fortunately, it's easy to see all of this from the entrance to the valley, so you make a map of the valley and the blizzards (your puzzle input). For example:

```
#.#####
#.....#
#&gt;....#
#.....#
#...v.#
#.....#
#####.#

```

The walls of the valley are drawn as `#`; everything else is ground. Clear ground - where there is currently no blizzard - is drawn as `.`. Otherwise, blizzards are drawn with an arrow indicating their direction of motion: up (`^`), down (`v`), left (`&lt;`), or right (`&gt;`).

The above map includes two blizzards, one moving right (`&gt;`) and one moving down (`v`). In one minute, each blizzard moves one position in the direction it is pointing:

```
#.#####
#.....#
#.&gt;...#
#.....#
#.....#
#...v.#
#####.#

```

Due to conservation of blizzard energy, as a blizzard reaches the wall of the valley, a new blizzard forms on the opposite side of the valley moving in the same direction. After another minute, the bottom downward-moving blizzard has been replaced with a new downward-moving blizzard at the top of the valley instead:

```
#.#####
#...v.#
#..&gt;..#
#.....#
#.....#
#.....#
#####.#

```

Because blizzards are made of tiny snowflakes, they pass right through each other. After another minute, both blizzards temporarily occupy the same position, marked `2`:

```
#.#####
#.....#
#...2.#
#.....#
#.....#
#.....#
#####.#

```

After another minute, the situation resolves itself, giving each blizzard back its personal space:

```
#.#####
#.....#
#....&gt;#
#...v.#
#.....#
#.....#
#####.#

```

Finally, after yet another minute, the rightward-facing blizzard on the right is replaced with a new one on the left facing the same direction:

```
#.#####
#.....#
#&gt;....#
#.....#
#...v.#
#.....#
#####.#

```

This process repeats at least as long as you are observing it, but probably forever.

Here is a more complex example:

```
#.######
#&gt;&gt;.&lt;^&lt;#
#.&lt;..&lt;&lt;#
#&gt;v.&gt;&lt;&gt;#
#&lt;^v^^&gt;#
######.#

```

Your expedition begins in the only non-wall position in the top row and needs to reach the only non-wall position in the bottom row. On each minute, you can **move** up, down, left, or right, or you can **wait** in place. You and the blizzards act **simultaneously**, and you cannot share a position with a blizzard.

In the above example, the fastest way to reach your goal requires `**18**` steps. Drawing the position of the expedition as `E`, one way to achieve this is:

```
Initial state:
#**E**######
#&gt;&gt;.&lt;^&lt;#
#.&lt;..&lt;&lt;#
#&gt;v.&gt;&lt;&gt;#
#&lt;^v^^&gt;#
######.#

Minute 1, move down:
#.######
#**E**&gt;3.&lt;.#
#&lt;..&lt;&lt;.#
#&gt;2.22.#
#&gt;v..^&lt;#
######.#

Minute 2, move down:
#.######
#.2&gt;2..#
#**E**^22^&lt;#
#.&gt;2.^&gt;#
#.&gt;..&lt;.#
######.#

Minute 3, wait:
#.######
#&lt;^&lt;22.#
#**E**2&lt;.2.#
#&gt;&lt;2&gt;..#
#..&gt;&lt;..#
######.#

Minute 4, move up:
#.######
#**E**&lt;..22#
#&lt;&lt;.&lt;..#
#&lt;2.&gt;&gt;.#
#.^22^.#
######.#

Minute 5, move right:
#.######
#2**E**v.&lt;&gt;#
#&lt;.&lt;..&lt;#
#.^&gt;^22#
#.2..2.#
######.#

Minute 6, move right:
#.######
#&gt;2**E**&lt;.&lt;#
#.2v^2&lt;#
#&gt;..&gt;2&gt;#
#&lt;....&gt;#
######.#

Minute 7, move down:
#.######
#.22^2.#
#&lt;v**E**&lt;2.#
#&gt;&gt;v&lt;&gt;.#
#&gt;....&lt;#
######.#

Minute 8, move left:
#.######
#.&lt;&gt;2^.#
#.**E**&lt;&lt;.&lt;#
#.22..&gt;#
#.2v^2.#
######.#

Minute 9, move up:
#.######
#&lt;**E**2&gt;&gt;.#
#.&lt;&lt;.&lt;.#
#&gt;2&gt;2^.#
#.v&gt;&lt;^.#
######.#

Minute 10, move right:
#.######
#.2**E**.&gt;2#
#&lt;2v2^.#
#&lt;&gt;.&gt;2.#
#..&lt;&gt;..#
######.#

Minute 11, wait:
#.######
#2^**E**^2&gt;#
#&lt;v&lt;.^&lt;#
#..2.&gt;2#
#.&lt;..&gt;.#
######.#

Minute 12, move down:
#.######
#&gt;&gt;.&lt;^&lt;#
#.&lt;**E**.&lt;&lt;#
#&gt;v.&gt;&lt;&gt;#
#&lt;^v^^&gt;#
######.#

Minute 13, move down:
#.######
#.&gt;3.&lt;.#
#&lt;..&lt;&lt;.#
#&gt;2**E**22.#
#&gt;v..^&lt;#
######.#

Minute 14, move right:
#.######
#.2&gt;2..#
#.^22^&lt;#
#.&gt;2**E**^&gt;#
#.&gt;..&lt;.#
######.#

Minute 15, move right:
#.######
#&lt;^&lt;22.#
#.2&lt;.2.#
#&gt;&lt;2&gt;**E**.#
#..&gt;&lt;..#
######.#

Minute 16, move right:
#.######
#.&lt;..22#
#&lt;&lt;.&lt;..#
#&lt;2.&gt;&gt;**E**#
#.^22^.#
######.#

Minute 17, move down:
#.######
#2.v.&lt;&gt;#
#&lt;.&lt;..&lt;#
#.^&gt;^22#
#.2..2**E**#
######.#

Minute 18, move down:
#.######
#&gt;2.&lt;.&lt;#
#.2v^2&lt;#
#&gt;..&gt;2&gt;#
#&lt;....&gt;#
######**E**#

```

**What is the fewest number of minutes required to avoid the blizzards and reach the goal?**

Your puzzle answer was `245`.

## --- Part Two ---

As the expedition reaches the far side of the valley, one of the Elves looks especially dismayed:

He **forgot his snacks** at the entrance to the valley!

Since you're so good at dodging blizzards, the Elves humbly request that you go back for his snacks. From the same initial conditions, how quickly can you make it from the start to the goal, then back to the start, then back to the goal?

In the above example, the first trip to the goal takes `18` minutes, the trip back to the start takes `23` minutes, and the trip back to the goal again takes `13` minutes, for a total time of `**54**` minutes.

**What is the fewest number of minutes required to reach the goal, go back to the start, then reach the goal again?**

Your puzzle answer was `798`.

Both parts of this puzzle are complete! They provide two gold stars: **

At this point, you should [return to your Advent calendar](/2022) and try another puzzle.

If you still want to see it, you can [get your puzzle input](24/input).

You can also [Shareon
  [Twitter](https://twitter.com/intent/tweet?text=I%27ve+completed+%22Blizzard+Basin%22+%2D+Day+24+%2D+Advent+of+Code+2022&amp;url=https%3A%2F%2Fadventofcode%2Ecom%2F2022%2Fday%2F24&amp;related=ericwastl&amp;hashtags=AdventOfCode)
[Mastodon](javascript:void(0);)] this puzzle.
