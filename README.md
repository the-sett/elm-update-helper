# Helper functions for nesting updates in Elm

The `Task.Extra` module contains:

```
message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)
```

Which is a useful way of creating a `Cmd` from a `Msg`.

The `Update` modules provide functions for lifting nested update functions into a
parent update function, following the so-called 'nested TEA' pattern. The `Update2`
module is for update functions that return a new `Model` and a `Cmd`. The `Update3`
module is for update functions that also return an addition 'out message'. These
update functions can help to reduce the amount of boiler-plate code that needs
to be written.
