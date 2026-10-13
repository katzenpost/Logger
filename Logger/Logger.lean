/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import Std.Sync.Mutex
import Std.Time
import Logger.Level
import Logger.Output

/-! # Loggers

A logger writes one line per record: the local time, the level's tag, the module, the message.

    15:02:12.581 NOTI client/conn: Connected to gateway.

Loggers made from one another with `withModule` and `withLevel` share an output, and a lock that
keeps lines from concurrent tasks whole. -/

namespace Logger

/-- An open output and the lock its writers take. -/
structure Sink where
  private stream : IO.FS.Stream
  private lock : Std.Mutex Unit

structure Logger where
  level : Level := .notice
  module : String := ""
  sink : Option Sink := none

namespace Logger

def new (output : Output) (level : Level) (module : String := "") : IO Logger := do
  return { level, module, sink := some { stream := ← output.open, lock := ← Std.Mutex.new () } }

/-- Writes nothing. -/
def off : Logger := {}

instance : Inhabited Logger := ⟨off⟩

def withModule (l : Logger) (module : String) : Logger := { l with module }

def withLevel (l : Logger) (level : Level) : Logger := { l with level }

def enabled (l : Logger) (lvl : Level) : Bool := l.sink.isSome && decide (lvl ≤ l.level)

private def pad (width : Nat) (n : Nat) : String :=
  let s := toString n
  "".pushn '0' (width - s.length) ++ s

/-- `HH:MM:SS.mmm`. -/
def formatTime (t : Std.Time.PlainTime) : String :=
  s!"{pad 2 t.hour.val.toNat}:{pad 2 t.minute.val.toNat}:{pad 2 t.second.val.toNat}." ++
    pad 3 (t.nanosecond.val.toNat / 1000000)

def formatLine (time : String) (lvl : Level) (module msg : String) : String :=
  let source := if module.isEmpty then "" else module ++ ": "
  s!"{time} {lvl.tag} {source}{msg}"

def log (l : Logger) (lvl : Level) (msg : String) : IO Unit := do
  let some sink := l.sink | return
  unless lvl ≤ l.level do return
  let line := formatLine (formatTime (← Std.Time.PlainTime.now)) lvl l.module msg
  sink.lock.atomically do
    sink.stream.putStrLn line
    sink.stream.flush

def critical (l : Logger) (msg : String) : IO Unit := l.log .critical msg
def error (l : Logger) (msg : String) : IO Unit := l.log .error msg
def warning (l : Logger) (msg : String) : IO Unit := l.log .warning msg
def notice (l : Logger) (msg : String) : IO Unit := l.log .notice msg
def info (l : Logger) (msg : String) : IO Unit := l.log .info msg
def debug (l : Logger) (msg : String) : IO Unit := l.log .debug msg

end Logger

end Logger
