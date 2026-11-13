#!/usr/bin/env python3
"""
qwen_count_annot_lines.py — Count lines in C/C++ files with four sub-counts:

  1. "annot"   : lines part of a __-prefixed function call with at least
                   one string-literal argument (may span multiple lines),
                   or __GHOST_END(...) calls (no string required), or
                   const __ghost_fn <name> = __ghost_begin(...);
  2. "include"   : #include directive lines
  3. "whitespace": lines with no code content (empty or comment-only)
  4. "remaining" : everything else

Categories are mutually exclusive.
Priority: annot > include > whitespace > remaining.

Usage:
    python3 qwen_count_annot_lines.py file1.c file2.cpp [more files ...]
"""

import sys
import re
import argparse


# ─────────────────────────────────────────────
# Comment stripping (preserves line structure)
# ─────────────────────────────────────────────
def strip_comments(code: str) -> str:
    out = []
    i, n = 0, len(code)
    in_line = False
    in_block = False

    while i < n:
        ch = code[i]

        if in_line:
            if ch == '\n':
                in_line = False
                out.append('\n')
            else:
                out.append(' ')
            i += 1

        elif in_block:
            if ch == '*' and i + 1 < n and code[i + 1] == '/':
                in_block = False
                out.append('  ')
                i += 2
            else:
                out.append('\n' if ch == '\n' else ' ')
                i += 1

        else:
            if ch == '/' and i + 1 < n:
                if code[i + 1] == '/':
                    in_line = True
                    out.append('  ')
                    i += 2
                    continue
                elif code[i + 1] == '*':
                    in_block = True
                    out.append('  ')
                    i += 2
                    continue

            if ch == '"' or ch == "'":
                quote = ch
                out.append(ch)
                i += 1
                while i < n and code[i] != quote:
                    if code[i] == '\\' and i + 1 < n:
                        out.append(code[i])
                        out.append(code[i + 1])
                        i += 2
                    else:
                        out.append(code[i])
                        i += 1
                if i < n:
                    out.append(code[i])
                    i += 1
                continue

            out.append(ch)
            i += 1

    return ''.join(out)


# ─────────────────────────────────────────────
# Line-index helpers
# ─────────────────────────────────────────────
def build_line_starts(code):
    starts = [0]
    for idx, ch in enumerate(code):
        if ch == '\n':
            starts.append(idx + 1)
    return starts


def line_of(line_starts, pos):
    lo, hi = 0, len(line_starts) - 1
    while lo < hi:
        mid = (lo + hi + 1) // 2
        if line_starts[mid] <= pos:
            lo = mid
        else:
            hi = mid - 1
    return lo


# ─────────────────────────────────────────────
# Generic __ call detection (multi-line aware)
# ─────────────────────────────────────────────
def match_underscore_calls(code, line_starts):
    """
    Find identifiers starting with `__` immediately followed by a
    parenthesised argument list.  A call is flagged as annot if:
      • it contains at least one double-quoted string literal, OR
      • the identifier is exactly `__GHOST_END`.
    Returns the set of 0-based line indices covered.
    """
    matched = set()
    i, n = 0, len(code)

    while i < n:
        if code[i] != '_':
            i += 1
            continue
        if i + 1 >= n or code[i + 1] != '_':
            i += 1
            continue

        if i > 0 and (code[i - 1].isalnum() or code[i - 1] == '_'):
            i += 1
            continue

        # consume the full identifier
        j = i
        while j < n and (code[j].isalnum() or code[j] == '_'):
            j += 1
        func_name = code[i:j]

        # skip whitespace, expect '('
        k = j
        while k < n and code[k] in ' \t\r\n':
            k += 1
        if k >= n or code[k] != '(':
            i = j
            continue

        # walk to matching ')'
        depth = 0
        has_str = False
        m = k
        while m < n:
            c = code[m]
            if c == '(':
                depth += 1
            elif c == ')':
                depth -= 1
                if depth == 0:
                    break
            elif c == '"':
                has_str = True
            m += 1

        if depth == 0:
            is_annot = has_str or func_name == '__GHOST_END'
            if is_annot:
                start_ln = line_of(line_starts, i)
                end_ln = line_of(line_starts, m)
                matched.update(range(start_ln, end_ln + 1))
            i = m + 1
        else:
            i = j

    return matched


# ─────────────────────────────────────────────
# Ghost-fn declaration detection (multi-line aware)
# ─────────────────────────────────────────────
_GHOST_START_RE = re.compile(r'\bconst\s+__ghost_fn\s+\w+\s*=', re.MULTILINE)


def match_ghost_fn_declarations(code, line_starts):
    matched = set()
    for m in _GHOST_START_RE.finditer(code):
        decl_start = m.start()
        pos = m.end()
        while pos < len(code) and code[pos] in ' \t\r\n':
            pos += 1

        if not code.startswith('__ghost_begin', pos):
            continue
        pos += len('__ghost_begin')

        while pos < len(code) and code[pos] in ' \t\r\n':
            pos += 1
        if pos >= len(code) or code[pos] != '(':
            continue

        depth = 0
        end = pos
        while end < len(code):
            c = code[end]
            if c == '(':
                depth += 1
            elif c == ')':
                depth -= 1
                if depth == 0:
                    break
            end += 1

        if depth == 0:
            start_ln = line_of(line_starts, decl_start)
            end_ln = line_of(line_starts, end)
            matched.update(range(start_ln, end_ln + 1))

    return matched


# ─────────────────────────────────────────────
# #include detection
# ─────────────────────────────────────────────
_INCLUDE_RE = re.compile(r'^\s*#\s*include\b', re.MULTILINE)


def match_include_lines(code, line_starts):
    matched = set()
    for m in _INCLUDE_RE.finditer(code):
        matched.add(line_of(line_starts, m.start()))
    return matched


# ─────────────────────────────────────────────
# Whitespace-only line detection (on stripped text)
# ─────────────────────────────────────────────
def match_whitespace_lines(stripped_lines):
    return {i for i, ln in enumerate(stripped_lines) if ln.strip() == ''}


# ─────────────────────────────────────────────
# Per-file processing
# ─────────────────────────────────────────────
def process_file(path, verbose=False):
    with open(path, 'r', encoding='utf-8', errors='replace') as fh:
        raw = fh.read()

    if raw.endswith('\n'):
        raw = raw[:-1]

    total = raw.count('\n') + 1
    raw_lines = raw.split('\n')

    stripped = strip_comments(raw)
    if stripped.endswith('\n'):
        stripped = stripped[:-1]

    line_starts = build_line_starts(stripped)
    stripped_lines = stripped.split('\n')

    # detect each category
    annot = set()
    annot |= match_ghost_fn_declarations(stripped, line_starts)
    annot |= match_underscore_calls(stripped, line_starts)

    includes = match_include_lines(stripped, line_starts)
    whitespace = match_whitespace_lines(stripped_lines)

    # enforce mutual exclusivity: annot > include > whitespace
    includes -= annot
    whitespace -= annot
    whitespace -= includes

    # clamp to valid range
    annot = {ln for ln in annot if ln < total}
    includes = {ln for ln in includes if ln < total}
    whitespace = {ln for ln in whitespace if ln < total}

    annot_count = len(annot)
    include_count = len(includes)
    whitespace_count = len(whitespace)
    remaining_count = total - annot_count - include_count - whitespace_count

    if verbose:
        for ln in sorted(includes):
            print(f'    {ln + 1:5d}  [include]    {raw_lines[ln]}')
        for ln in sorted(whitespace):
            print(f'    {ln + 1:5d}  [whitespace] (no code)')
        for ln in sorted(annot):
            print(f'    {ln + 1:5d}  [annot]    {raw_lines[ln]}')

    return total, annot_count, include_count, whitespace_count, remaining_count


# ─────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────
def main():
    ap = argparse.ArgumentParser(
        description='Count C/C++ lines by category: annot, include, '
                    'whitespace, remaining.'
    )
    ap.add_argument('files', nargs='+', metavar='FILE',
                    help='C/C++ source files to process')
    ap.add_argument('-v', '--verbose', action='store_true',
                    help='print the matching lines')
    args = ap.parse_args()

    tot_all = tot_annot = tot_include = tot_ws = tot_remaining = 0

    for path in args.files:
        try:
            total, annot, includes, ws, remaining = process_file(
                path, args.verbose)
        except OSError as e:
            print(f'error: {e}', file=sys.stderr)
            continue

        tot_all += total
        tot_annot += annot
        tot_include += includes
        tot_ws += ws
        tot_remaining += remaining

        print(f'{path}')
        print(f'  total lines        : {total}')
        print(f'  annot lines      : {annot}')
        print(f'  include lines      : {includes}')
        print(f'  whitespace lines   : {ws}')
        print(f'  remaining lines    : {remaining}')
        print()

    print(f'{"=" * 40}')
    print(f'TOTAL  lines     : {tot_all}')
    print(f'  annot        : {tot_annot}')
    print(f'  include        : {tot_include}')
    print(f'  whitespace     : {tot_ws}')
    print(f'  remaining      : {tot_remaining}')


if __name__ == '__main__':
    main()
