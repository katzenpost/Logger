# Logger

Runtime logging for Lean programs: levels, modules, and output to stdout, stderr, a file or any
stream. Lines look like those of Go's `op/go-logging`:

    15:02:12.581 NOTI client/conn: Connected to gateway.

```lean
import Logger
open Logger

def main : IO Unit := do
  let level ← Level.fromEnv "MYAPP_LOG" .notice
  let log ← Logger.new .stderr level "myapp"
  log.notice "starting"
  let conn := log.withModule "myapp/conn"
  conn.debug "dialing"

  -- In a monad that carries a logger:
  (do logInfo "from LoggerT" : LoggerT IO Unit).run' log
```

Levels, most severe first: `critical`, `error`, `warning`, `notice`, `info`, `debug`. A logger
emits records at its level and above. Loggers made with `withModule` and `withLevel` share their
output, and concurrent writers never interleave lines.

A monad with a logger in its own environment gets the `logInfo` family with one instance:

```lean
instance : MonadLogger AppM := ⟨return (← read).logger⟩
```

    lake build && lake exe logger-test
