#!/bin/bash
set -e

# enable debugging and point the f# interpreter at our library symlinks
fsharpi --debug:full -I:./libs "$@"