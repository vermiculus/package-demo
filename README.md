# package-demo.el

Package Demo allows you to demonstrate package features in a
predictable way.  Think of it like a glorified keyboard macro that
runs at believably-human speeds.

## Example

```elisp
(package-demo-define-demo do-something
  (typewriter "Hello, world!")
  (pause 1)
  (typewriter "\nI'm package-demo.el!")
  (M-x #'save-buffer))
```
