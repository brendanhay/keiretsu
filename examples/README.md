# Example

This directory contains a number of contrived services that have intertwined dependencies.

Each service will launch it's own redis database, with `web1` and `web2` having a
circular dependency upon each other, and both depending upon `queue`. `queue`, `web1`, and `web2`
all depend on the `cache` process.

You can see the example in action by running `keiretsu` from any of the sub-directories.
Try using the `--debug` flag to see the configured environments for each of the proctypes.
