---
title: The ByteString Situation
---

This document discusses the `GenUnchecked` instance for `ByteString`.


## History

Up to (but not including) version `0.4.0.0` of `genvalidity-bytestring`, the
`GenUnchecked` instance of `ByteString` and `Text` were principled and would generate
values that were significantly dangerous (to the point where they would cause segfaults).

From version `0.4.0.0` of `genvalidity-bytestring`, these instances call `error` instead.
The `GenInvalid` instance have been removed as well.

## Considerations
