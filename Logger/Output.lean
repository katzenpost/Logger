/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

/-! # Outputs -/

namespace Logger

inductive Output where
  | stdout
  | stderr
  /-- Appended to, and created if missing. -/
  | file (path : System.FilePath)

def Output.open : Output → IO IO.FS.Stream
  | .stdout => IO.getStdout
  | .stderr => IO.getStderr
  | .file path => do
    if let some dir := path.parent then
      unless dir.toString.isEmpty do IO.FS.createDirAll dir
    return IO.FS.Stream.ofHandle (← IO.FS.Handle.mk path .append)

end Logger
