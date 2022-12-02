# Advent of Code 2022 solutions

Some solutions in Ada/SPARK.

| Day | Place                         | SPARK? |
| --- | ----------------------------- | ------ |
| 1   | [day1.adb](day1/src/day1.adb) | Yes    |
| 2   | [day2.adb](day2/src/day2.adb) | Yes    |

## Build instructions

Install [Alire](https://alire.ada.dev/) and navigate to the given day (e.g.
`cd day1`). Run `alr run` to execute the code, and `alr gnatprove` to verify it.

Ada 2022 is required for things like bigint support.

Input data is usually hardcoded to be an `inputs.txt` file. The output is
usually two lines for each part.
