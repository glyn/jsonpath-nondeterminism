# JSONPath Non-determinism

This project generates some non-deterministic testcases for the [JSONPath CTS](https://github.com/jsonpath-standard/jsonpath-compliance-test-suite).

For the background to this project, please see my blog post [Testing non-determinism](https://underlap.org/testing-non-determinism).

The approach is to use Haskell's list monad to model the non-determinism in the JSONPath [RFC 9535](https://www.rfc-editor.org/rfc/rfc9535).

To print out some examples, issue:
```
stack run
```

To run the tests, issue:
```
stack test
```

The current algorithm is very inefficient, so an example and a test are commented out to avoid run times of several minutes. I would welcome any significant performance improvements, especially any which are as readable as (or more readable than!) the current code.

Also, I'd welcome any other improvements as I have limited experience in Haskell.
