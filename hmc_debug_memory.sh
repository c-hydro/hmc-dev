#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# HMC Memory / Valgrind Test
# Version: 1.1.0
# Date: 2026/03/20
#
# PURPOSE
#   Run HMC under Valgrind using the new interface:
#
#       HMC.x {domain}.info.txt
#
#   The script supports:
#     - memcheck   : memory leak / invalid access analysis
#     - callgrind  : call profiling (CPU hotspots)
#     - massif     : heap memory profiling
#
# EXAMPLES
#
#   1) Memcheck test (recommended first)
#      ./HMC_Tools_Memory_Test.sh memcheck \
#          "$HOME/fp_libs_system/hmc/HMC_Model_V3_Exec.x" \
#          "$HOME/hmc_runs/test_memory" \
#          "$HOME/hmc_runs/test_memory/marche.info.txt"
#
#   2) Callgrind test (CPU profiling)
#      ./HMC_Tools_Memory_Test.sh callgrind \
#          "$HOME/fp_libs_system/hmc/HMC_Model_V3_Exec.x" \
#          "$HOME/hmc_runs/test_memory" \
#          "$HOME/hmc_runs/test_memory/marche.info.txt"
#
#   3) Massif test (heap usage over time)
#      ./HMC_Tools_Memory_Test.sh massif \
#          "$HOME/fp_libs_system/hmc/HMC_Model_V3_Exec.x" \
#          "$HOME/hmc_runs/test_memory" \
#          "$HOME/hmc_runs/test_memory/marche.info.txt"
#
#   4) Using environment variables
#      export NETCDF_DIR="$HOME/fp_libs_system/nc4"
#      ./HMC_Tools_Memory_Test.sh memcheck \
#          "$HOME/fp_libs_system/hmc/HMC_Model_V3_Exec.x" \
#          "./run_memory" \
#          "./marche.info.txt"
#
#   5) Minimal example (local execution)
#      ./HMC_Tools_Memory_Test.sh memcheck \
#          ./HMC_Model_V3_Exec.x \
#          ./run \
#          ./domain.info.txt
#
# EXECUTION MODEL
#   Internally the script runs:
#
#       ./HMC_Model_V3_MemoryTest.x domain.info.txt
#
#   (no old positional parameters are used)
#
# INPUT ARGUMENTS
#   $1 Mode                : memcheck | callgrind | massif
#   $2 HMC executable      : compiled model
#   $3 Run directory       : working folder
#   $4 Domain info file    : {domain}.info.txt
#
# OPTIONAL ENVIRONMENT VARIABLES
#   EXEC_NAME=filename          Override copied executable name
#   NETCDF_DIR=/path/to/nc4     Set NetCDF runtime libraries
#   KEEP_EXEC_COPY=true|false   Keep or remove copied executable
#
# OUTPUT FILES
#   memcheck:
#       memory_check.txt
#
#   callgrind:
#       memory_check.txt
#       callgrind.out
#
#   massif:
#       memory_check.txt
#       massif.out
#       massif_report.txt
#
# REQUIREMENTS
#   sudo apt-get install valgrind graphviz
#
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
    sed -n '1,120p' "$0"
    exit 0
}

[[ "${1:-}" == "--help" || "${1:-}" == "-h" ]] && usage

# -----------------------------------------------------------------------------
# Arguments
[[ $# -eq 4 ]] || die "Expected 4 arguments: <mode> <compiled_exec> <run_dir> <domain.info.txt>"

MODE="$1"
EXEC_SRC="$2"
RUN_DIR="$3"
DOMAIN_INFO_SRC="$4"

[[ -f "$EXEC_SRC" ]] || die "Compiled executable not found: $EXEC_SRC"
[[ -f "$DOMAIN_INFO_SRC" ]] || die "Domain info file not found: $DOMAIN_INFO_SRC"

case "$MODE" in
    memcheck|callgrind|massif) ;;
    *) die "Unsupported mode '$MODE'. Use: memcheck | callgrind | massif" ;;
esac

EXEC_NAME="${EXEC_NAME:-HMC_Model_V3_MemoryTest.x}"
KEEP_EXEC_COPY="${KEEP_EXEC_COPY:-true}"

# -----------------------------------------------------------------------------
# Requirements
require_cmd cp
require_cmd rm
require_cmd mkdir
require_cmd chmod
require_cmd tee
require_cmd valgrind

if [[ "$MODE" == "massif" ]]; then
    require_cmd ms_print
fi

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
log "Mode: $MODE"

# Clean previous outputs
rm -f \
    "$RUN_DIR/memory_check.txt" \
    "$RUN_DIR/callgrind.out" \
    "$RUN_DIR/massif.out" \
    "$RUN_DIR/massif_report.txt"

if [[ -f "$EXEC_DST" ]]; then
    log "Removing previous executable copy: $EXEC_DST"
    rm -f "$EXEC_DST"
fi

cp "$EXEC_SRC" "$EXEC_DST"
chmod +x "$EXEC_DST"

cp "$DOMAIN_INFO_SRC" "$DOMAIN_INFO_DST"

# -----------------------------------------------------------------------------
# Run test
cd "$RUN_DIR"

echo "----------------------------------------------------------------"
echo "HMC Memory Test"
echo "----------------------------------------------------------------"
echo "Mode                : $MODE"
echo "Executable          : $EXEC_DST"
echo "Domain info         : $DOMAIN_INFO_NAME"
echo "----------------------------------------------------------------"

case "$MODE" in
    memcheck)
        log "Running Valgrind memcheck"
        valgrind \
            -v \
            --track-origins=yes \
            --tool=memcheck \
            --leak-check=full \
            --show-leak-kinds=all \
            --num-callers=50 \
            "./${EXEC_NAME}" "${DOMAIN_INFO_NAME}" \
            2>&1 | tee "$RUN_DIR/memory_check.txt"
        ;;
    callgrind)
        log "Running Valgrind callgrind"
        valgrind \
            --tool=callgrind \
            --callgrind-out-file="$RUN_DIR/callgrind.out" \
            "./${EXEC_NAME}" "${DOMAIN_INFO_NAME}" \
            2>&1 | tee "$RUN_DIR/memory_check.txt"
        ;;
    massif)
        log "Running Valgrind massif"
        valgrind \
            --tool=massif \
            --massif-out-file="$RUN_DIR/massif.out" \
            "./${EXEC_NAME}" "${DOMAIN_INFO_NAME}" \
            2>&1 | tee "$RUN_DIR/memory_check.txt"

        ms_print "$RUN_DIR/massif.out" > "$RUN_DIR/massif_report.txt"
        ;;
esac

if ! bool_is_true "$KEEP_EXEC_COPY"; then
    rm -f "$EXEC_DST"
    log "Removed copied executable: $EXEC_DST"
fi

echo "----------------------------------------------------------------"
echo "HMC Memory Test Summary"
echo "----------------------------------------------------------------"
echo "Mode                : $MODE"
echo "Executable source   : $EXEC_SRC"
echo "Executable used     : $EXEC_DST"
echo "Run directory       : $RUN_DIR"
echo "Domain info         : $DOMAIN_INFO_DST"
echo "Main log            : $RUN_DIR/memory_check.txt"
if [[ "$MODE" == "callgrind" ]]; then
    echo "Callgrind output    : $RUN_DIR/callgrind.out"
fi
if [[ "$MODE" == "massif" ]]; then
    echo "Massif output       : $RUN_DIR/massif.out"
    echo "Massif report       : $RUN_DIR/massif_report.txt"
fi
echo "----------------------------------------------------------------"
