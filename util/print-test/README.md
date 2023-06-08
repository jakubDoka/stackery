## Print Testing

This crate allows you to easily use technique called `print testing` (further just PT). Rather then explicitly asserting with hardcoded values PT allows for more declarative testing. Each test is passed a buffer (`&mut String`) to whitch it should output deperministic sequence of characters that represent some code outcome. When you add a new test, it will initialy always panic until you accept it as valid. After accepting, each subsequent test just checks if output of each test changed. If somting changes, test fails and you eill be presented with a git like diff of what changed. The fact that outpuch changed acn be interpreted in many ways, either its intentional in which case joy just accept changes or test actually broke and you proceed with usual steps when test breaks.

## Usage

To pass arguments to the print tests, you unfortunately need to set environment variables. Here are the possible oprions with coresponding default values:

```bash
# directory where test results are cached, for each test there will be a file named
# as absolute path of given function where `::` is replaced with `-`, this also
# includes the crate name so central workspace cache can be used
PRINT_TEST_CACHE_DIR=print-test-cache
# when you run tests with this equal to true, all tests will be considered cirrect
# and saved in the cache (yes rerunning the tests is nessesary), if you want to
# select which tests to accept, filter them as you do with normal test
PRINT_TEST_WRITE_CHANGES=false
```
