#!/bin/bash
# Compiles each probes/*.fs on its own against SqlHydra.Query and records the compiler's verdict.
# Run from src/Spike.WriteRecord; pass a glob prefix to run a subset (e.g. ./run-probes.sh A).
cd "$(dirname "$0")" || exit 1
DOTNET="${DOTNET:-dotnet}"
for f in probes/${1:-}*.fs; do
  echo "############ $f"
  out=$($DOTNET build Probe.fsproj -p:Probe="$f" -v q --nologo 2>&1)
  if echo "$out" | grep -q "Build succeeded"; then
    echo "  COMPILES"
  else
    echo "$out" | grep -E "error FS" | sed 's|.*probes/|  probes/|; s| \[/.*||' | sort -u
  fi
done
