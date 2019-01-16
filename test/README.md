# How to add new test?

New test can be added by adding new directories and files in the `test/specs` folder.

Each directory name in the `test/specs` represent the test description, while each file within each directory is a test case.

Filename that does not starts with `@` are test cases that test for valid Keli's code.

Filename that starts with `@` are test cases that test for invalid Keli's code.


# How to run specific test case?

Prefix the filename with `ONLY:`. Note that you can do this to more than one file at the same time.
