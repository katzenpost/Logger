/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

/-! # Test helpers -/

namespace Logger.Tests

abbrev TestM := StateT (Nat × Nat) IO

def check (name : String) (ok : Bool) (detail : Unit → String := fun _ => "") : TestM Unit := do
  let (pass, fail) ← get
  if ok then set (pass + 1, fail)
  else
    set (pass, fail + 1)
    let d := detail ()
    IO.println s!"FAIL {name}{if d.isEmpty then "" else ": " ++ d}"

def checkEq {α : Type} [BEq α] [ToString α] (name : String) (expected actual : α) : TestM Unit :=
  check name (expected == actual) fun _ => s!"expected {expected}, got {actual}"

def runSuite (name : String) (t : TestM Unit) : IO Bool := do
  let ((), (pass, fail)) ← t.run (0, 0)
  IO.println s!"{name}: {pass} passed, {fail} failed"
  return fail == 0

end Logger.Tests
