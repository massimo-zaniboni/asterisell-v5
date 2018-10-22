#!/usr/bin/env python

import sys

# 0 for start of mark-down mode
# 1 for blank line in mark-down mode (not sure of start of code)
# 2 for code mode
state = 0

for line in sys.stdin:
    if line.startswith('--> '):
        if not (state == 2):
            print('```')
        state = 2
        print('> ' + line[4:]),
    elif line.startswith('-- '):
        if state == 2:
            print '```'
        state = 0
        print(line[3:]),
    elif line.startswith('--'):
        if state == 2:
            print '```'
        state = 0
        print('')
    elif len(line.strip()) == 0:
        if state == 0:
            state = 1
        print('')
    else:
        # there is some code-content to display
        if not (state == 2):
            print('```')
        state = 2
        print(line),

if state == 2:
    print('```')
