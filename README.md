# jsonpath-non-determinism

Generate some non-deterministic testcases for the [JSONPath CTS](https://github.com/jsonpath-standard/jsonpath-compliance-test-suite).

The approach is to use Haskell's list monad to model the non-determinism in [RFC 9535](https://www.rfc-editor.org/rfc/rfc9535).

To print out some examples, issue:
```
stack run
```

To run the tests, issue:
```
stack test
```

The current algorithm is inefficient, so an example and a test are commented out to avoid run times of several minutes.
