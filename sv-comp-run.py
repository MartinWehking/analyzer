#!/usr/bin/python3

import sys
import glob
import subprocess
import re
import collections
import os.path
import shlex
import yaml # pip3 install pyyaml
from timeit import default_timer as timer


OVERVIEW = False # with True Goblint isn't executed
GOBLINT_COMMAND = "./goblint --enable ana.sv-comp --disable ana.int.trier --enable ana.int.enums --enable ana.int.interval --sets solver td3 --enable exp.widen-context --enable exp.malloc-fail --enable exp.partition-arrays.enabled {code_filename}"
TIMEOUT = 30 # with some int that's Goblint timeout for single execution
START = 1


def str2bool(s):
    return {
        "true": True,
        "false": False
    }[s]

def extract_bool(p, s):
    m = re.search(p, s)
    return str2bool(m.group(1)) if m else None


stats = collections.defaultdict(int)
total_time = 0
try:
    inp = sys.argv[1]
    task_filenames = []
    if inp.endswith('.yml'):
        task_filenames.append(inp)
    elif inp.endswith('.set'):
        with open(inp) as set_file:
            for pattern in set_file:
                pattern = pattern.strip()
                if pattern:
                    pattern = os.path.join(os.path.dirname(inp), pattern)
                    for task_filename in glob.iglob(pattern):
                        task_filenames.append(task_filename)
    else:
        print('Call script on .yml or .set file') # TODO also allow .c to just call goblint on with above options?

    for task_i, task_filename in list(enumerate(sorted(task_filenames)))[START - 1:]:
        print(f"{task_i + 1}/{len(task_filenames)}: {task_filename}: ", end="", flush=True)

        if task_filename.endswith(".yml"):
            with open(task_filename) as task_file:
                y = yaml.safe_load(task_file)

                code_filename = y["input_files"]
                code_filename = os.path.join(os.path.dirname(task_filename), code_filename)

                expected = None
                for y_property in y["properties"]:
                    if y_property["property_file"] == "../properties/unreach-call.prp":
                        expected = y_property["expected_verdict"]
        else:
            code_filename = task_filename
            expected = extract_bool(r"_(false|true)-unreach-call", task_filename)

        if OVERVIEW:
            actual = None
            task_time = None
        else:
            start_time = timer()
            try:
                p = subprocess.run(shlex.split(GOBLINT_COMMAND.format(code_filename=code_filename)), stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="utf-8", timeout=TIMEOUT)
                if "Fatal error: exception " in p.stdout:
                    print(p.stdout)
                    exit(1)
                actual = extract_bool(r"SV-COMP \(unreach-call\): (false|true)", p.stdout)
            except subprocess.TimeoutExpired:
                actual = "timeout"
            finally:
                end_time = timer()
                task_time = end_time - start_time
                total_time += task_time

        if "p" in locals(): # sometimes on timeout p is declared, sometimes isn't
            missing_funcs = False
            for m in re.finditer(r"Function definition missing for (__VERIFIER_\S+)", p.stdout):
                missing_funcs = True
                print(f"MISSING FUNC {m.group(1)}")

            if missing_funcs:
                sys.exit(1)

        text = None
        if expected is None:
            text = f"NONE {actual}, no expected_verdict for unreach-call"
        elif actual == expected:
            text = f"CORRECT {expected}"
            points += 2 if actual else 1
        elif actual == "timeout" or actual is None:
            text = f"UNKNOWN {actual}, expected {expected}"
        else:
            text = f"INCORRECT {actual}, expected {expected}"
            points -= 32 if actual else 16

        time_text = f" ({task_time:.2f} s)" if task_time is not None else ""
        print(text + time_text)
        stats[text] += 1

except KeyboardInterrupt:
    pass
finally:
    print()
    print("-" * 80)
    print("For each .yml we compare with expected_verdict for 'c/properties/unreach-call.prp':")
    print("CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )")
    print("-> 'expected True' means for all paths __VERIFIER_error is never called (__VERIFIER_assert always true) ")
    print("-> 'expected False' means there exists a path where  __VERIFIER_error could be called (__VERIFIER_assert can be false)")
    print("-" * 80)
    for text, count in stats.items():
        print(f"{text}: {count}")
    print("-" * 80)
    print(f"total time: {total_time:.2f} s")
    print(f"total points: {points} (assuming witnesses are correct)")
