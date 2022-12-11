# Advent of Code 2022 solutions

Some solutions in Ada/SPARK.

| Day | Place                            | SPARK? |
| --- | -------------------------------- | ------ |
| 1   | [day1.adb](day1/src/day1.adb)    | Yes    |
| 2   | [day2.adb](day2/src/day2.adb)    | Yes    |
| 3   | [day3.adb](day3/src/day3.adb)    | Yes    |
| 4   | [day4/](day4/src/)               | Yes    |
| 5   | [day5.adb](day5/src/day5.adb)    | No     |
| 6   | [day6.adb](day6/src/day6.adb)    | Yes    |
| 7   | [day7.adb](day7/src/day7.adb)    | Yes    |
| 8   | [day8.adb](day8/src/day8.adb)    | Yes    |
| 9   | [day9.adb](day9/src/day9.adb)    | Yes    |
| 10  | [day10.adb](day10/src/day10.adb) | Yes    |
| 11  | [day11/](day11/src/)             | No     |

## Build instructions

Install [Alire](https://alire.ada.dev/) and navigate to the given day (e.g.
`cd day1`). Run `alr run` to execute the code, and `alr gnatprove` to verify it.

Ada 2022 is required for things like bigint support, square bracket array
literals, and non-scalar images.

Input data is usually hardcoded to be an `inputs.txt` file. The output is
usually two lines for each part.
