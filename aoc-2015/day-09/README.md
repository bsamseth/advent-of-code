
## --- Day 9: All in a Single Night ---

Every year, Santa manages to deliver all of his presents in a single night.

This year, however, he has some new locations to visit; his elves have provided him the distances between every pair of locations.  He can start and end at any two (different) locations he wants, but he must visit each location exactly once.  What is the **shortest distance** he can travel to achieve this?

For example, given the following distances:

```
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141

```

The possible routes are therefore:

```
Dublin -&gt; London -&gt; Belfast = 982
London -&gt; Dublin -&gt; Belfast = 605
London -&gt; Belfast -&gt; Dublin = 659
Dublin -&gt; Belfast -&gt; London = 659
Belfast -&gt; Dublin -&gt; London = 605
Belfast -&gt; London -&gt; Dublin = 982

```

The shortest of these is `London -&gt; Dublin -&gt; Belfast = 605`, and so the answer is `605` in this example.

What is the distance of the shortest route?

Your puzzle answer was `117`.

## --- Part Two ---

The next year, just to show off, Santa decides to take the route with the **longest distance** instead.

He can still start and end at any two (different) locations he wants, and he still must visit each location exactly once.

For example, given the distances above, the longest route would be `982` via (for example) `Dublin -&gt; London -&gt; Belfast`.

What is the distance of the longest route?

Your puzzle answer was `909`.

Both parts of this puzzle are complete! They provide two gold stars: **

At this point, you should [return to your Advent calendar](/2015) and try another puzzle.

If you still want to see it, you can [get your puzzle input](9/input).

You can also [Shareon
  [Twitter](https://twitter.com/intent/tweet?text=I%27ve+completed+%22All+in+a+Single+Night%22+%2D+Day+9+%2D+Advent+of+Code+2015&amp;url=https%3A%2F%2Fadventofcode%2Ecom%2F2015%2Fday%2F9&amp;related=ericwastl&amp;hashtags=AdventOfCode)
[Mastodon](javascript:void(0);)] this puzzle.
