/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import Logger
import Logger.Tests.Util

/-! # Logger tests

`logger-test emit stdout|stderr` logs one line of each level to that output, for the output tests
to read from a child process. -/

namespace Logger.Tests

open Logger

/-- `HH:MM:SS.mmm TAG rest`, with the time well formed. -/
def splitLine (line : String) : Option (String × String × String) :=
  match line.splitOn " " with
  | time :: tag :: rest =>
    let digits (s : String) := s.length == 2 && s.all Char.isDigit
    match time.splitOn ":" with
    | [h, m, sms] =>
      match sms.splitOn "." with
      | [sec, ms] =>
        if digits h && digits m && digits sec && ms.length == 3 && ms.all Char.isDigit then
          some (time, tag, " ".intercalate rest)
        else none
      | _ => none
    | _ => none
  | _ => none

def tempFile (name : String) : IO System.FilePath := do
  let dir : System.FilePath := ".lake" / "test-logs"
  IO.FS.createDirAll dir
  let path := dir / name
  if ← path.pathExists then IO.FS.removeFile path
  return path

def readLines (path : System.FilePath) : IO (Array String) := do
  return (← IO.FS.lines path)

def logEveryLevel (l : Logger) : IO Unit := do
  l.critical "c"; l.error "e"; l.warning "w"; l.notice "n"; l.info "i"; l.debug "d"

def levels : TestM Unit := do
  checkEq "order" "critical error warning notice info debug"
    (" ".intercalate (Level.all.map (·.name)))
  check "critical is most severe" (Level.all.all (Level.critical ≤ ·))
  check "debug is least severe" (Level.all.all (· ≤ Level.debug))
  checkEq "tags" "CRIT ERRO WARN NOTI INFO DEBU" (" ".intercalate (Level.all.map (·.tag)))
  check "names parse" (Level.all.all fun l => Level.ofString? l.name == some l)
  check "parsing ignores case and spaces" (Level.ofString? " DeBuG " == some .debug)
  check "unknown name" (Level.ofString? "verbose").isNone
  check "unset variable" ((← Level.fromEnv "LOGGER_TEST_UNSET_VARIABLE" .warning) == .warning)
  check "set variable" ((← Level.fromEnv "PATH" .info) == .info)

def filtering : TestM Unit := do
  for (lvl, i) in Level.all.zipIdx do
    let path ← tempFile s!"filter-{lvl.name}.log"
    logEveryLevel (← Logger.new (.file path) lvl)
    let tags := (← readLines path).toList.filterMap (splitLine · |>.map (·.2.1))
    checkEq s!"level {lvl.name} emits" (" ".intercalate ((Level.all.take (i + 1)).map (·.tag)))
      (" ".intercalate tags)
  let path ← tempFile "off.log"
  IO.FS.writeFile path ""
  logEveryLevel Logger.off
  checkEq "off writes nothing" 0 (← readLines path).size

def format : TestM Unit := do
  let path ← tempFile "format.log"
  let l ← Logger.new (.file path) .debug "client/conn"
  l.notice "Connected to gateway."
  l.withModule "" |>.info "no module"
  let lines ← readLines path
  match lines.toList.map splitLine with
  | [some (_, tag1, rest1), some (_, tag2, rest2)] =>
    checkEq "tag" "NOTI" tag1
    checkEq "module and message" "client/conn: Connected to gateway." rest1
    checkEq "second tag" "INFO" tag2
    checkEq "no module" "no module" rest2
  | _ => check "two well-formed lines" false (fun _ => toString lines)
  let t : Std.Time.PlainTime := Std.Time.PlainTime.ofHourMinuteSecondsNano 9 5 7 42000000
  checkEq "time format" "09:05:07.042" (Logger.formatTime t)

def files : TestM Unit := do
  let path ← tempFile "append.log"
  (← Logger.new (.file path) .info).info "first"
  (← Logger.new (.file path) .info).info "second"
  let msgs := (← readLines path).toList.filterMap (splitLine · |>.map (·.2.2))
  checkEq "appends across loggers" "first,second" (",".intercalate msgs)
  let nested ← tempFile "nested/dir/created.log"
  (← Logger.new (.file nested) .info).info "made"
  check "creates missing directories" (← nested.pathExists)

def customStream : TestM Unit := do
  let buf ← IO.mkRef ({} : IO.FS.Stream.Buffer)
  let l ← Logger.new (.stream (IO.FS.Stream.ofBuffer buf)) .info "pane"
  l.info "into a buffer"
  l.debug "filtered out"
  let text := String.fromUTF8! (← buf.get).data
  let msgs := (text.splitOn "\n").filterMap (splitLine · |>.map (·.2.2))
  checkEq "a stream of the program's own" "pane: into a buffer" (",".intercalate msgs)

def streams : TestM Unit := do
  let self ← IO.appPath
  for (which, other) in [("stdout", "stderr"), ("stderr", "stdout")] do
    let out ← IO.Process.output { cmd := self.toString, args := #["emit", which] }
    let (mine, theirs) := if which == "stdout" then (out.stdout, out.stderr) else (out.stderr, out.stdout)
    let tags := (mine.splitOn "\n").filterMap (splitLine · |>.map (·.2.1))
    checkEq s!"{which} receives the lines" "CRIT ERRO WARN NOTI" (" ".intercalate tags)
    checkEq s!"{other} stays empty" "" theirs

def concurrency : TestM Unit := do
  let path ← tempFile "concurrent.log"
  let l ← Logger.new (.file path) .info
  let tasks ← (List.range 8).mapM fun t => IO.asTask do
    for i in [0:500] do l.withModule s!"task{t}" |>.info s!"line {i} {"x".pushn 'x' 200}"
  for t in tasks do discard <| IO.wait t
  let lines ← readLines path
  checkEq "every line written" 4000 lines.size
  check "no line interleaved" (lines.all fun line => match splitLine line with
    | some (_, "INFO", rest) => (rest.splitOn ": ").length == 2 && rest.endsWith ("x".pushn 'x' 200)
    | _ => false)

def subLoggers : TestM Unit := do
  let path ← tempFile "sub.log"
  let root ← Logger.new (.file path) .notice "root"
  let child := root.withModule "child" |>.withLevel .debug
  root.debug "hidden"
  child.debug "shown"
  root.notice "root line"
  let rests := (← readLines path).toList.filterMap (splitLine · |>.map (·.2.2))
  checkEq "shared output, own module and level" "child: shown|root: root line" ("|".intercalate rests)
  check "enabled" (child.enabled .debug && !root.enabled .info && !Logger.off.enabled .critical)

/-- A monad with a logger of its own. -/
structure Env where
  logger : Logger
  name : String

abbrev AppM := ReaderT Env IO

instance : MonadLogger AppM := ⟨return (← read).logger⟩

def monad : TestM Unit := do
  let path ← tempFile "monad.log"
  let l ← Logger.new (.file path) .info
  (do logInfo "from LoggerT"; logDebug "hidden" : LoggerT IO Unit).run' l
  (do logWarning s!"from {(← read).name}" : AppM Unit).run { logger := l, name := "AppM" }
  (do logNotice "through ExceptT" : ExceptT String (LoggerT IO) Unit).run.run' l |> discard
  let msgs := (← readLines path).toList.filterMap (splitLine · |>.map (·.2.2))
  checkEq "logs from each monad" "from LoggerT|from AppM|through ExceptT" ("|".intercalate msgs)

end Logger.Tests

open Logger Logger.Tests in
def main (args : List String) : IO UInt32 := do
  if let ["emit", which] := args then
    let output := if which == "stderr" then Output.stderr else Output.stdout
    logEveryLevel (← Logger.new output .notice)
    return 0
  let results := #[
    ← runSuite "levels" levels,
    ← runSuite "filtering" filtering,
    ← runSuite "format" format,
    ← runSuite "files" files,
    ← runSuite "custom stream" customStream,
    ← runSuite "streams" streams,
    ← runSuite "concurrency" concurrency,
    ← runSuite "sub-loggers" subLoggers,
    ← runSuite "monad" monad]
  return if results.all id then 0 else 1
