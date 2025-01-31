## \-\-- Day 16: Reindeer Maze \-\--

It\'s time again for the [Reindeer Olympics](/2015/day/14)! This year,
the big event is the *Reindeer Maze*, where the Reindeer compete for the
*[lowest
score]{title="I would say it's like Reindeer Golf, but knowing Reindeer, it's almost certainly nothing like Reindeer Golf."}*.

You and The Historians arrive to search for the Chief right as the event
is about to start. It wouldn\'t hurt to watch a little, right?

The Reindeer start on the Start Tile (marked `S`) facing *East* and need
to reach the End Tile (marked `E`). They can move forward one tile at a
time (increasing their score by `1` point), but never into a wall (`#`).
They can also rotate clockwise or counterclockwise 90 degrees at a time
(increasing their score by `1000` points).

To figure out the best place to sit, you start by grabbing a map (your
puzzle input) from a nearby kiosk. For example:

    ###############
    #.......#....E#
    #.#.###.#.###.#
    #.....#.#...#.#
    #.###.#####.#.#
    #.#.#.......#.#
    #.#.#####.###.#
    #...........#.#
    ###.#.#####.#.#
    #...#.....#.#.#
    #.#.#.###.#.#.#
    #.....#...#.#.#
    #.###.#.#.#.#.#
    #S..#.....#...#
    ###############

There are many paths through this maze, but taking any of the best paths
would incur a score of only *`7036`*. This can be achieved by taking a
total of `36` steps forward and turning 90 degrees a total of `7` times:

    ###############
    #.......#....E#
    #.#.###.#.###^#
    #.....#.#...#^#
    #.###.#####.#^#
    #.#.#.......#^#
    #.#.#####.###^#
    #..>>>>>>>>v#^#
    ###^#.#####v#^#
    #>>^#.....#v#^#
    #^#.#.###.#v#^#
    #^....#...#v#^#
    #^###.#.#.#v#^#
    #S..#.....#>>^#
    ###############

Here\'s a second example:

    #################
    #...#...#...#..E#
    #.#.#.#.#.#.#.#.#
    #.#.#.#...#...#.#
    #.#.#.#.###.#.#.#
    #...#.#.#.....#.#
    #.#.#.#.#.#####.#
    #.#...#.#.#.....#
    #.#.#####.#.###.#
    #.#.#.......#...#
    #.#.###.#####.###
    #.#.#...#.....#.#
    #.#.#.#####.###.#
    #.#.#.........#.#
    #.#.#.#########.#
    #S#.............#
    #################

In this maze, the best paths cost *`11048`* points; following one such
path would look like this:

    #################
    #...#...#...#..E#
    #.#.#.#.#.#.#.#^#
    #.#.#.#...#...#^#
    #.#.#.#.###.#.#^#
    #>>v#.#.#.....#^#
    #^#v#.#.#.#####^#
    #^#v..#.#.#>>>>^#
    #^#v#####.#^###.#
    #^#v#..>>>>^#...#
    #^#v###^#####.###
    #^#v#>>^#.....#.#
    #^#v#^#####.###.#
    #^#v#^........#.#
    #^#v#^#########.#
    #S#>>^..........#
    #################

Note that the path shown above includes one 90 degree turn as the very
first move, rotating the Reindeer from facing East to facing North.

Analyze your map carefully. *What is the lowest score a Reindeer could
possibly get?*

To begin, [get your puzzle input](16/input){target="_blank"}.

Answer:

You can also [\[Share[on
[Bluesky](https://bsky.app/intent/compose?text=%22Reindeer+Maze%22+%2D+Day+16+%2D+Advent+of+Code+2024+%23AdventOfCode+https%3A%2F%2Fadventofcode%2Ecom%2F2024%2Fday%2F16){target="_blank"}
[Twitter](https://twitter.com/intent/tweet?text=%22Reindeer+Maze%22+%2D+Day+16+%2D+Advent+of+Code+2024&url=https%3A%2F%2Fadventofcode%2Ecom%2F2024%2Fday%2F16&related=ericwastl&hashtags=AdventOfCode){target="_blank"}
[Mastodon](javascript:void(0);){onclick="var ms; try{ms=localStorage.getItem('mastodon.server')}finally{} if(typeof ms!=='string')ms=''; ms=prompt('Mastodon Server?',ms); if(typeof ms==='string' && ms.length){this.href='https://'+ms+'/share?text=%22Reindeer+Maze%22+%2D+Day+16+%2D+Advent+of+Code+2024+%23AdventOfCode+https%3A%2F%2Fadventofcode%2Ecom%2F2024%2Fday%2F16';try{localStorage.setItem('mastodon.server',ms);}finally{}}else{return false;}"
target="_blank"}]{.share-content}\]]{.share} this puzzle.
