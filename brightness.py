#!/usr/bin/env python

import sys, os

MAX_PATH = "/sys/class/drm/card0-LVDS-1/intel_backlight/max_brightness"
CUR_PATH = "/sys/class/drm/card0-LVDS-1/intel_backlight/brightness"

MAX = int(open(MAX_PATH, 'r').read().strip())
CUR = int(open(CUR_PATH, 'r').read().strip())
assert 0 <= CUR <= MAX <= 1e7

def usage():
    print "Usage: %s (up|down|[NUMBER])" % sys.argv[0]
    sys.exit(0)

def up():
    if CUR < 5:
        return CUR + 1
    if CUR < 25:
        return ((CUR + 7) // 5) * 5
    else:
        return int(CUR * 1.30)

def down():
    if CUR <= 5:
        return CUR - 1
    elif CUR <= 25:
        return ((CUR - 3) // 5) * 5
    else:
        return int(CUR / 1.30)

def main():
    if len(sys.argv) == 1:
        print CUR
        return

    if len(sys.argv) != 2:
        usage()

    cmd = sys.argv[1]
    if cmd == 'up':
        res = up()
    elif cmd == 'down':
        res = down()
    else:
        res = int(cmd)

    if res < 0:
        res = 0
    elif res > MAX:
        res = MAX

    print res
    os.execl("/usr/local/bin/_brightness", "_brightness", str(res))

if __name__ == "__main__":
    main()
