
## --- Day 23: Experimental Emergency Teleportation ---

Using your torch to search the darkness of the rocky cavern, you finally locate the man's friend: a small **reindeer**.

You're not sure how it got so far in this cave.  It looks sick - too sick to walk - and too heavy for you to carry all the way back.  Sleighs won't be invented for another 1500 years, of course.

The only option is **experimental emergency teleportation**.

You hit the "experimental emergency teleportation" button on the device and push **I accept the risk** on no fewer than 18 different warning messages. Immediately, the device deploys hundreds of tiny **nanobots** which fly around the cavern, apparently assembling themselves into a very specific **formation**. The device lists the `X,Y,Z` position (`pos`) for each nanobot as well as its **signal radius** (`r`) on its tiny screen (your puzzle input).

Each nanobot can transmit signals to any integer coordinate which is a distance away from it **less than or equal to** its signal radius (as measured by [Manhattan distance](https://en.wikipedia.org/wiki/Taxicab_geometry)). Coordinates a distance away of less than or equal to a nanobot's signal radius are said to be **in range** of that nanobot.

Before you start the teleportation process, you should determine which nanobot is the **strongest** (that is, which has the largest signal radius) and then, for that nanobot, the **total number of nanobots that are in range** of it, **including itself**.

For example, given the following nanobots:

```
pos=&lt;0,0,0&gt;, r=4
pos=&lt;1,0,0&gt;, r=1
pos=&lt;4,0,0&gt;, r=3
pos=&lt;0,2,0&gt;, r=1
pos=&lt;0,5,0&gt;, r=3
pos=&lt;0,0,3&gt;, r=1
pos=&lt;1,1,1&gt;, r=1
pos=&lt;1,1,2&gt;, r=1
pos=&lt;1,3,1&gt;, r=1

```

The strongest nanobot is the first one (position `0,0,0`) because its signal radius, `4` is the largest. Using that nanobot's location and signal radius, the following nanobots are in or out of range:

- The nanobot at `0,0,0` is distance `0` away, and so it is **in range**.
- The nanobot at `1,0,0` is distance `1` away, and so it is **in range**.
- The nanobot at `4,0,0` is distance `4` away, and so it is **in range**.
- The nanobot at `0,2,0` is distance `2` away, and so it is **in range**.
- The nanobot at `0,5,0` is distance `5` away, and so it is **not** in range.
- The nanobot at `0,0,3` is distance `3` away, and so it is **in range**.
- The nanobot at `1,1,1` is distance `3` away, and so it is **in range**.
- The nanobot at `1,1,2` is distance `4` away, and so it is **in range**.
- The nanobot at `1,3,1` is distance `5` away, and so it is **not** in range.

In this example, in total, `**7**` nanobots are in range of the nanobot with the largest signal radius.

Find the nanobot with the largest signal radius.  **How many nanobots are in range** of its signals?

To begin, [get your puzzle input](23/input).

Answer: <input autocomplete="off" name="answer" type="text"/> <input type="submit" value="[Submit]"/>

You can also [Shareon
  [Twitter](https://twitter.com/intent/tweet?text=%22Experimental+Emergency+Teleportation%22+%2D+Day+23+%2D+Advent+of+Code+2018&amp;url=https%3A%2F%2Fadventofcode%2Ecom%2F2018%2Fday%2F23&amp;related=ericwastl&amp;hashtags=AdventOfCode)
[Reddit](http://www.reddit.com/submit?url=https%3A%2F%2Fadventofcode%2Ecom%2F2018%2Fday%2F23&amp;title=%22Experimental+Emergency+Teleportation%22+%2D+Day+23+%2D+Advent+of+Code+2018)] this puzzle.
