# jsonpath-non-determinism

**Work in progress**

Generate some non-deterministic testcases for the [JSONPath CTS](https://github.com/jsonpath-standard/jsonpath-compliance-test-suite).

The approach is to use Haskell's list monad to model the non-determinism in [RFC 9535](https://www.rfc-editor.org/rfc/rfc9535).
