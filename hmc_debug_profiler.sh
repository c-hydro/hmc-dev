#!#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# HMC Profiling Tool
# Version: 1.1.0
# Date: 2026/03/20
#
# PURPOSE
#   Run an HMC profiling test using the new command-line interface:
#
#       HMC.x {domain}.info.txt
#
#   The script:
#     1) uses a profiling-compiled HMC executable (-pg)
#     2) copies executable and info file into a run directory
#     3) runs the model with:
#            ./HMC_Model_V3_ProfTest.x {domain}.info.txt
#     4) generates gprof text and graph outputs
#
# USAGE
#   ./HMC_Tools_Profiler.sh \
#       /path/to/compiled_hmc_executable \
#       /path/to/run_directory \
#       /path/to/domain.info.txt
#
# EXAMPLE
#   ./HMC_Tools_Profiler.sh \
#       "$HOME/fp_libs_system/hmc/HMC_Model_V3_Profile.x" \
#       "$HOME/hmc_runs/test_profile" \
#       "$HOME/hmc_runs/test_profile/marche.info.txt"
#
# OPTIONAL ENVIRONMENT VARIABLES
#   EXEC_NAME=HMC_Model_V3_ProfTest.x
#   PROFILER_DOT=/path/to/gprof2dot.py
#   NETCDF_DIR=/path/to/nc4
#   KEEP_EXEC_COPY=true|false
# -----------------------------------------------------------------------------

set -Eeuo pipefail

log()  { printf '[INFO] %s\n' "$*"; }
warn() { printf '[WARN] %s\n' "$*" >&2; }
err()  { printf '[ERROR] %s\n' "$*" >&2; }
die()  { err "$*"; exit 1; }

on_error() {
    local exit_code=$?
    err "Script failed at line ${BASH_LINENO[0]} with exit code ${exit_code}"
    exit "${exit_code}"
}
trap on_error ERR

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}

bool_is_true() {
    [[ "${1:-false}" == "true" ]]
}

usage() {
    sed -n '1,80p' "$0"
    exit 0
}

[[ "${1:-}" == "--help" || "${1:-}" == "-h" ]] && usage

# -----------------------------------------------------------------------------
# Arguments
[[ $# -eq 3 ]] || die "Expected 3 arguments: <compiled_exec> <run_dir> <domain.info.txt>"

EXEC_SRC="$1"
RUN_DIR="$2"
DOMAIN_INFO_SRC="$3"

[[ -f "$EXEC_SRC" ]] || die "Compiled executable not found: $EXEC_SRC"
[[ -f "$DOMAIN_INFO_SRC" ]] || die "Domain info file not found: $DOMAIN_INFO_SRC"

EXEC_NAME="${EXEC_NAME:-HMC_Model_V3_ProfTest.x}"
KEEP_EXEC_COPY="${KEEP_EXEC_COPY:-true}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXEC_DIR="$(cd "$(dirname "$EXEC_SRC")" && pwd)"

PROFILER_DOT="${PROFILER_DOT:-}"
if [[ -z "$PROFILER_DOT" ]]; then
    if [[ -f "$SCRIPT_DIR/gprof2dot.py" ]]; then
        PROFILER_DOT="$SCRIPT_DIR/gprof2dot.py"
    elif [[ -f "$EXEC_DIR/gprof2dot.py" ]]; then
        PROFILER_DOT="$EXEC_DIR/gprof2dot.py"
    else
        die "gprof2dot.py not found. Set PROFILER_DOT=/full/path/to/gprof2dot.py"
    fi
fi

[[ -f "$PROFILER_DOT" ]] || die "gprof2dot.py not found: $PROFILER_DOT"

# -----------------------------------------------------------------------------
# Requirements
require_cmd cp
require_cmd rm
require_cmd mkdir
require_cmd chmod
require_cmd gprof
require_cmd dot
require_cmd python3

# -----------------------------------------------------------------------------
# Environment
if [[ -n "${NETCDF_DIR:-}" && -d "${NETCDF_DIR}/lib" ]]; then
    export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}:${NETCDF_DIR}/lib"
    log "Updated LD_LIBRARY_PATH with NETCDF_DIR: ${NETCDF_DIR}/lib"
fi

ulimit -s unlimited || warn "Could not set stack size to unlimited"

# -----------------------------------------------------------------------------
# Prepare run directory
mkdir -p "$RUN_DIR"
RUN_DIR="$(cd "$RUN_DIR" && pwd)"

EXEC_DST="${RUN_DIR}/${EXEC_NAME}"
DOMAIN_INFO_NAME="$(basename "$DOMAIN_INFO_SRC")"
DOMAIN_INFO_DST="${RUN_DIR}/${DOMAIN_INFO_NAME}"

log "Run directory: $RUN_DIR"
log "Compiled executable: $EXEC_SRC"
log "Executable copy: $EXEC_DST"
log "Domain info file: $DOMAIN_INFO_DST"
log "gprof2dot.py: $PROFILER_DOT"

# Clean previous outputs
rm -f "$RUN_DIR/gmon.out" \
      "$RUN_DIR/hmc_model_analysis.txt" \
      "$RUN_DIR/hmc_model_analysis.png"

if [[ -f "$EXEC_DST" ]]; then
    log "Removing previous profiling executable copy: $EXEC_DST"
    rm -f "$EXEC_DST"
fi

cp "$EXEC_SRC" "$EXEC_DST"
chmod +x "$EXEC_DST"

cp "$DOMAIN_INFO_SRC" "$DOMAIN_INFO_DST"

# -----------------------------------------------------------------------------
# Run profiling
cd "$RUN_DIR"

log "Starting profiling run ..."
log "Command: ./${EXEC_NAME} ${DOMAIN_INFO_NAME}"

"./${EXEC_NAME}" "${DOMAIN_INFO_NAME}"

log "Executable finished"

[[ -f "$RUN_DIR/gmon.out" ]] || die "gmon.out not generated. Check that executable was compiled with -pg"

# Text profile
gprof "$EXEC_DST" "$RUN_DIR/gmon.out" > "$RUN_DIR/hmc_model_analysis.txt"

# Graph profile
gprof "$EXEC_DST" "$RUN_DIR/gmon.out" | python3 "$PROFILER_DOT" | dot -Tpng -o "$RUN_DIR/hmc_model_analysis.png"

log "Profiling outputs generated:"
log " - $RUN_DIR/hmc_model_analysis.txt"
log " - $RUN_DIR/hmc_model_analysis.png"

if ! bool_is_true "$KEEP_EXEC_COPY"; then
    rm -f "$EXEC_DST"
    log "Removed copied executable: $EXEC_DST"
fi

echo "----------------------------------------------------------------"
echo "HMC Profiling Summary"
echo "----------------------------------------------------------------"
echo "Executable source     : $EXEC_SRC"
echo "Executable used       : $EXEC_DST"
echo "Run directory         : $RUN_DIR"
echo "Domain info           : $DOMAIN_INFO_DST"
echo "Output text profile   : $RUN_DIR/hmc_model_analysis.txt"
echo "Output graph profile  : $RUN_DIR/hmc_model_analysis.png"
echo "----------------------------------------------------------------"
