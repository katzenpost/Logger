/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

/-! # Levels

From most to least severe. A logger emits the records at its level and every more severe one. -/

namespace Logger

inductive Level where
  | critical
  | error
  | warning
  | notice
  | info
  | debug
  deriving DecidableEq, Repr, Inhabited

namespace Level

def all : List Level := [critical, error, warning, notice, info, debug]

/-- 0 for `critical`, growing as severity falls. -/
def rank : Level → Nat
  | critical => 0
  | error => 1
  | warning => 2
  | notice => 3
  | info => 4
  | debug => 5

/-- `a ≤ b` when `a` is at least as severe as `b`. -/
instance : LE Level := ⟨fun a b => a.rank ≤ b.rank⟩

instance (a b : Level) : Decidable (a ≤ b) := inferInstanceAs (Decidable (a.rank ≤ b.rank))

def tag : Level → String
  | critical => "CRIT"
  | error => "ERRO"
  | warning => "WARN"
  | notice => "NOTI"
  | info => "INFO"
  | debug => "DEBU"

def name : Level → String
  | critical => "critical"
  | error => "error"
  | warning => "warning"
  | notice => "notice"
  | info => "info"
  | debug => "debug"

instance : ToString Level := ⟨name⟩

def ofString? (s : String) : Option Level := all.find? (·.name == s.trimAscii.toString.toLower)

/-- The level named by environment variable `var`, or `default` when it is unset or names no
level. -/
def fromEnv (var : String) (default : Level) : IO Level := do
  return ((← IO.getEnv var).bind ofString?).getD default

end Level

end Logger
