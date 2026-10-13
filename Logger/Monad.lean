/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import Logger.Logger

/-! # Logging in any monad

A monad that can give its logger logs with `logInfo` and the rest. `LoggerT` carries one; a monad
that already carries a logger in its own environment needs a one-line instance:

    instance : MonadLogger ClientM := ⟨return (← read).logger⟩
-/

namespace Logger

class MonadLogger (m : Type → Type) where
  getLogger : m Logger

export MonadLogger (getLogger)

abbrev LoggerT := ReaderT Logger

instance {m : Type → Type} [Monad m] : MonadLogger (LoggerT m) := ⟨read⟩

instance {ρ : Type} {m : Type → Type} [Monad m] [MonadLogger m] : MonadLogger (ReaderT ρ m) :=
  ⟨liftM (getLogger : m Logger)⟩

instance {ε : Type} {m : Type → Type} [Monad m] [MonadLogger m] : MonadLogger (ExceptT ε m) :=
  ⟨liftM (getLogger : m Logger)⟩

instance {σ : Type} {m : Type → Type} [Monad m] [MonadLogger m] : MonadLogger (StateT σ m) :=
  ⟨liftM (getLogger : m Logger)⟩

def LoggerT.run' {m : Type → Type} {α : Type} (x : LoggerT m α) (l : Logger) : m α := x.run l

section
variable {m : Type → Type} [Monad m] [MonadLiftT IO m] [MonadLogger m]

def logAt (lvl : Level) (msg : String) : m Unit := do (← getLogger).log lvl msg
def logCritical (msg : String) : m Unit := logAt .critical msg
def logError (msg : String) : m Unit := logAt .error msg
def logWarning (msg : String) : m Unit := logAt .warning msg
def logNotice (msg : String) : m Unit := logAt .notice msg
def logInfo (msg : String) : m Unit := logAt .info msg
def logDebug (msg : String) : m Unit := logAt .debug msg

end

end Logger
