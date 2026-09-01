#!/bin/bash
# Compiles each probes/*.fs on its own against SqlHydra.Query and records the
# compiler's verdict. Run from src/Spike.ReadOnly.
cd "$(dirname "$0")" || exit 1
for f in probes/*.fs; do
  echo "############ $f"
  out=$(dotnet build Probe.fsproj -p:Probe="$f" -v q --nologo 2>&1)
  if echo "$out" | grep -q "Build succeeded"; then
    echo "  COMPILES"
  else
    echo "$out" | grep -E "error FS" | sed 's|.*probes/|  probes/|' | sort -u
  fi
done
