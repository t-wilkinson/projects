#!/usr/bin/env python
import multiprocessing
import threading  # preemptive
import asyncio  # cooperative
import subprocess
import sys
# https://docs.python.org/3/library/concurrency.html

process = subprocess.run(['echo', '$HOME'], capture_output=True)
print(vars(process))

with subprocess.Popen(['python'], stdout=sys.stdout) as popen:
    popen.communicate(input=sys.stdin)
