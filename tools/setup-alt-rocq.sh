#!/usr/bin/env bash
#
# Copyright (c) 2026 Pierre-Loic Garoche, ENAC.  LGPL-2.1-only.
#
# Make an *alternate* opam switch's Rocq usable from Frama-C/WP, so
# `-local-tactic` can run its coqc (e.g. one that provides the `validsdp`
# tactic) instead of the Rocq in the Frama-C switch.
#
# Why this is needed: Why3's Coq backend prepends `Require Import BuiltIn.` &c.
# to every generated .v -- its own Coq "realisation" library. A .vo can only be
# loaded by the exact Rocq build that produced it, so the realisations shipped
# with the Frama-C switch cannot be loaded by another switch's coqc
# ("inconsistent assumptions over Corelib.Init.Prelude"). This script rebuilds
# the realisations with the alternate switch's coqc and registers a matching
# Why3 prover.
#
# Usage:
#   tools/setup-alt-rocq.sh <alt-switch-name> [prover-name]
#
# Then run Frama-C from the *Frama-C* switch with:
#   frama-c -wp -local-tactic \
#     -local-tactic-prover <prover-name> \
#     -wp-why3-extra-config ~/.local/share/why3-<alt-switch-name>.conf \
#     -local-tactic-rocq-import mathcomp.all_ssreflect,ValidSDP.validsdp \
#     file.c
#
set -euo pipefail

ALT_SWITCH=${1:?usage: setup-alt-rocq.sh <alt-switch-name> [prover-name]}
PROVER_NAME=${2:-CoqAlt}

FRAMAC_SWITCH=$(opam switch show)
ALT_PREFIX=$(opam var --switch "$ALT_SWITCH" prefix)
WHY3_VER=$(opam list --switch "$FRAMAC_SWITCH" --columns=version why3 2>/dev/null | tail -1 | tr -d ' ')
WHY3_SRC="$HOME/.opam/$FRAMAC_SWITCH/.opam-switch/sources/why3.$WHY3_VER/lib/coq"
REALIS_DIR="$HOME/.local/share/why3-$ALT_SWITCH"
CONF="$HOME/.local/share/why3-$ALT_SWITCH.conf"
ALT_COQTOP="$ALT_PREFIX/bin/coqtop"
ROCQ_VER=$("$ALT_PREFIX/bin/coqc" --version | sed -n 's/.*version \([0-9.]*\).*/\1/p' | head -1)

echo "Frama-C switch      : $FRAMAC_SWITCH"
echo "alternate switch    : $ALT_SWITCH  (Rocq $ROCQ_VER)"
echo "Why3 Coq sources    : $WHY3_SRC"
echo "realisations output : $REALIS_DIR"
echo "why3 extra config   : $CONF"
echo "WP prover name      : $PROVER_NAME"
echo

[ -f "$WHY3_SRC/_CoqProject" ] || {
  echo "error: Why3 Coq realisation sources not found at $WHY3_SRC" >&2
  echo "       (the why3.$WHY3_VER opam source must be present; 'opam switch reinstall' keeps it)" >&2
  exit 1
}

# 1. rebuild Why3's Coq realisations with the alternate switch's coqc
rm -rf "$REALIS_DIR"
mkdir -p "$REALIS_DIR"
cp -R "$WHY3_SRC"/. "$REALIS_DIR"/
cd "$REALIS_DIR"
find . \( -name '*.vo' -o -name '*.glob' -o -name '*.vos' -o -name '*.vok' -o -name '*.aux' \) -delete
find . -name '*.v' | sort > .vfiles
xargs "$ALT_PREFIX/bin/coq_makefile" -R . Why3 -o Makefile.coq < .vfiles
make -f Makefile.coq -j"$(sysctl -n hw.ncpu 2>/dev/null || nproc)" COQBIN="$ALT_PREFIX/bin/"
echo "built $(find . -name '*.vo' | wc -l | tr -d ' ') realisation .vo files"

# 2. register a Why3 prover that uses the alternate coqtop + these realisations
mkdir -p "$(dirname "$CONF")"
cat > "$CONF" <<EOF
[prover]
command = "$ALT_COQTOP -batch -R $REALIS_DIR Why3 -l %f"
command_steps = ""
driver = "coq"
editor = "coqide"
in_place = false
interactive = true
name = "$PROVER_NAME"
alternative = ""
version = "$ROCQ_VER"
shortcut = "$(echo "$PROVER_NAME" | tr '[:upper:]' '[:lower:]')"
EOF

echo
echo "done. Sanity check:"
why3 config --extra-config="$CONF" list-provers | sed 's/^/  /'
echo
echo "use it with:"
echo "  frama-c -wp -local-tactic -local-tactic-prover $PROVER_NAME \\"
echo "    -wp-why3-extra-config $CONF \\"
echo "    -local-tactic-rocq-import <libs> file.c"
