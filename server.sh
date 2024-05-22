#!/bin/bash

killall optitrust_trace_server || echo "no server to kill"
dune exec optitrust_trace_server

