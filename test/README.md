# How to add new test?

New test can be added by adding new directories and files in the `test/specs` folder.

Within `test/specs`, there are two directories, namely `compile` and `suggest`.

- `execute` is for testing executing a Keli source file
- `suggest` is for testing Intellisense (code completion)

## For `execute`
Each directory name in `execute` represent the test description, while each file within each directory is a test case.

Within each test description directory, the following 2 files must be present:

- `entry.keli` , which is the source file where the test runner will execute as the entry point
- `output`, which is the STDOUT after performing the action on the entry file

### Naming convention
Use the `kebab-case` convention. 
For testing invalid case, prefix the description with `@`.

## For `suggest`
Similarly, each directory name in `suggest` also represent the test description. 

Within each test description directory, the following 2 files must be present:

- `entry.keli` , which is the source file where the test runner will search for suggestion

- `output`, which is a Haskell data literal of type `[CompletionItem]`, after performing the action on the entry file

- `where`, which specify the lineNumber and columnNumber, first line is lineNumber, second line is columnNumber, columnNumber is actually the position of the dot operator
    - note that lineNumber and columnNumber are zero-based index
    - so, lineNumber 2 actually means line 3, columnNumber 0 means column 1 in text editor


# How to run specific test case?

Prefix the filename with `ONLY:`. Note that you can do this to more than one file at the same time.
