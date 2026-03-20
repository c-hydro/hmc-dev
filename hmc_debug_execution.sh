#!/usr/bin/env bash
set -Eeuo pipefail

# -----------------------------------------------------------------------------
# Script information
SCRIPT_NAME="HMC - RUN TEST"
SCRIPT_VERSION="2.0.0"
SCRIPT_DATE="2026/03/20"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Default configuration (can be overridden by arguments or env vars)

HMC_ENV_FILE="${HMC_ENV_FILE:-$HOME/fp_libs_system/fp_env_system}"
HMC_EXECUTABLE="${HMC_EXECUTABLE:-$HOME/fp_libs_system/hmc/HMC_Model_V3_Exec.x}"
HMC_INFO_FILE="${HMC_INFO_FILE:-$PWD/domain.info.txt}"
RUN_DIR="${RUN_DIR:-$PWD}"

# -----------------------------------------------------------------------------

log()  { printf '[INFO] %s\n' "$*"; }
warn() { printf '[WARN] %s\n' "$*" >&2; }
err()  { printf '[ERROR] %s\n' "$*" >&2; }
die()  { err "$*"; exit 1; }

usage() {
cat <<EOF
Usage:
  $0 [executable] [run_dir] [domain.info.txt]

Examples:
  $0
  $0 HMC_Model_V3_Exec.x ./run ./marche.info.txt

Or via env:
  HMC_EXECUTABLE=... HMC_INFO_FILE=... $0
EOF
exit 0
}

[[ "${1:-}" == "--help" || "${1:-}" == "-h" ]] && usage

# -----------------------------------------------------------------------------
# Parse arguments (optional override)

[[ $# -ge 1 ]] && HMC_EXECUTABLE="$1"
[[ $# -ge 2 ]] && RUN_DIR="$2"
[[ $# -ge 3 ]] && HMC_INFO_FILE="$3"

# -----------------------------------------------------------------------------
# Checks

[[ -f "$HMC_EXECUTABLE" ]] || die "Executable not found: $HMC_EXECUTABLE"
[[ -f "$HMC_INFO_FILE" ]] || die "Info file not found: $HMC_INFO_FILE"

mkdir -p "$RUN_DIR"
RUN_DIR="$(cd "$RUN_DIR" && pwd)"

EXEC_NAME="$(basename "$HMC_EXECUTABLE")"
INFO_NAME="$(basename "$HMC_INFO_FILE")"

EXEC_LOCAL="$RUN_DIR/$EXEC_NAME"
INFO_LOCAL="$RUN_DIR/$INFO_NAME"

# -----------------------------------------------------------------------------
# Environment

if [[ -f "$HMC_ENV_FILE" ]]; then
    log "Loading environment: $HMC_ENV_FILE"
    source "$HMC_ENV_FILE"
else
    warn "Environment file not found: $HMC_ENV_FILE"
fi

ulimit -s unlimited || warn "Could not set stack size"

# -----------------------------------------------------------------------------
# Prepare run directory

log "Preparing run directory: $RUN_DIR"

cp "$HMC_EXECUTABLE" "$EXEC_LOCAL"
chmod +x "$EXEC_LOCAL"

cp "$HMC_INFO_FILE" "$INFO_LOCAL"

# -----------------------------------------------------------------------------
# Run model

cd "$RUN_DIR"

echo " ==================================================================================="
echo " ==> $SCRIPT_NAME (Version: $SCRIPT_VERSION Release_Date: $SCRIPT_DATE)"
echo " ==> START ..."
echo ""

log "Executable : $EXEC_LOCAL"
log "Info file  : $INFO_LOCAL"
log "Command    : ./$EXEC_NAME $INFO_NAME"

"./$EXEC_NAME" "$INFO_NAME"

echo ""
echo " ==> $SCRIPT_NAME (Version: $SCRIPT_VERSION Release_Date: $SCRIPT_DATE)"
echo " ==> ... END"
echo " ==> Bye, Bye"
echo " ==================================================================================="
