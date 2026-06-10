#!/usr/bin/env python3
"""Verifies tri-pp --line-markers output against its input.

Mirrors the TriCera-side consumer: every line matching
^#\\s*[0-9]+\\s+"[^"]*".*$ is recorded into a segment map (the next line is
line N of file F, following lines count up from there) and dropped; lines
before the first marker map to themselves. The same map is built for the
input, so inputs that already carry markers (cpp output, earlier tri-pp
stage) are handled identically.

Every identifier containing "probe_" must map to the same (file, line) set
in input and output -- i.e. every probe occurrence in the output, including
ones in copied text such as function clones, maps back to the authentic
original line of that probe.

usage: check_markers.py <input.c> <output.c> [--expect-zero-markers]
                        [--alias=<name>]
--alias names the work file of a chained tri-pp run: markers naming it
refer to the original input and are compared as such.
prints PASS or FAIL lines; exits non-zero on FAIL.
"""
import re
import sys

MARKER = re.compile(r'^#\s*([0-9]+)\s+"([^"]*)"')
# the input may also carry #line directives (clang consumes both forms);
# tri-pp itself must only ever emit the GNU linemarker form
LINE_DIRECTIVE = re.compile(r'^#\s*line\s+([0-9]+)\s+"([^"]*)"')
PROBE = re.compile(r'\bprobe_[A-Za-z0-9_]*')


def consumer_map(lines, default_file, with_line_directives=False):
    """(file, line) for each physical line; None for marker lines."""
    mapping = []
    cur_file, cur_line = None, 0
    for ln in lines:
        m = MARKER.match(ln)
        if m is None and with_line_directives:
            m = LINE_DIRECTIVE.match(ln)
        if m:
            mapping.append(None)
            cur_file, cur_line = m.group(2), int(m.group(1))
            continue
        if cur_file is None:
            mapping.append((default_file, len(mapping) + 1))
        else:
            mapping.append((cur_file, cur_line))
            cur_line += 1
    return mapping


def probe_locations(lines, mapping, alias=None, alias_to=None):
    """probe name -> set of mapped (file, line) of its occurrences.

    File names are compared by base name: the tool absolutizes the input
    path, so markers carry absolute paths while the input is named as
    invoked. A file named like `alias` is reported as `alias_to`."""
    locs = {}
    for i, ln in enumerate(lines):
        if mapping[i] is None:
            continue
        f, l = mapping[i]
        base = f.split('/')[-1]
        if alias is not None and base == alias:
            base = alias_to
        for m in PROBE.finditer(ln):
            locs.setdefault(m.group(0), set()).add((base, l))
    return locs


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    expect_zero = '--expect-zero-markers' in sys.argv
    alias = None
    for a in sys.argv[1:]:
        if a.startswith('--alias='):
            alias = a.split('=', 1)[1]
    input_path, output_path = args
    with open(input_path) as f:
        in_lines = f.read().split('\n')
    with open(output_path) as f:
        out_lines = f.read().split('\n')

    failures = 0

    n_markers = sum(1 for ln in out_lines if MARKER.match(ln))
    if expect_zero:
        if n_markers == 0:
            print("PASS: no markers for line-count-preserving input")
        else:
            print("FAIL: expected zero markers, found %d" % n_markers)
            failures += 1

    # the input maps through its own markers; the file the consumer reads is
    # named like the input here, so identity regions agree between the two
    in_map = consumer_map(in_lines, input_path, with_line_directives=True)
    out_map = consumer_map(out_lines, input_path)

    for i, ln in enumerate(out_lines):
        if LINE_DIRECTIVE.match(ln):
            print("FAIL: output line %d is a #line directive; only GNU"
                  " linemarkers may be emitted" % (i + 1))
            failures += 1
    in_base = input_path.split('/')[-1]
    in_probes = probe_locations(in_lines, in_map)
    out_probes = probe_locations(out_lines, out_map, alias, in_base)

    if not in_probes:
        print("FAIL: input contains no probe_ tokens")
        failures += 1

    for name in sorted(in_probes):
        want = in_probes[name]
        got = out_probes.get(name, set())
        if got == want:
            where = ", ".join("%s:%d" % (f.split('/')[-1], l)
                              for f, l in sorted(want))
            print("PASS: %s -> %s" % (name, where))
        else:
            print("FAIL: %s mapped to %s, expected %s" %
                  (name, sorted(got), sorted(want)))
            failures += 1

    for name in sorted(set(out_probes) - set(in_probes)):
        print("FAIL: probe %s appears only in the output" % name)
        failures += 1

    sys.exit(1 if failures else 0)


if __name__ == '__main__':
    main()
