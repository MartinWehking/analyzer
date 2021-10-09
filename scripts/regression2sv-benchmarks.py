#!/usr/bin/python3

import sys
from pathlib import Path
import re
import subprocess
import argparse
import copy

EXCLUDE_TASKS = [
    "04-mutex_13-failed_locking",
    "04-mutex_31-uninitialized",
    "04-mutex_49-type-invariants",
    "04-mutex_29-funstruct_rc",
    "04-mutex_30-funstruct_nr",
    "04-mutex_30-funstruct_nr",

    "06-symbeq_16-type_rc", # extern
    "06-symbeq_17-type_nr", # extern
    "06-symbeq_20-mult_accs_nr", # extern
    "06-symbeq_21-mult_accs_rc", # extern
    "10-synch_04-two_mainfuns", # no main
    "05-lval_ls_17-per_elem_simp", # no pthread include, locksmith pragma

    "09-regions_29-malloc_race_cp", # duplicate of 02/25
    "09-regions_30-list2alloc-offsets", # duplicate of 09/28
    "09-regions_31-equ_rc", # duplicate of 06/10
    "09-regions_32-equ_nr", # duplicate of 06/11
    "09-regions_34-escape_rc", # duplicate of 04/45
    "09-regions_35-list2_rc-offsets-thread", # duplicate of 09/03
    "10-synch_17-glob_fld_nr", # duplicate of 05/08
    "19-spec_02-mutex_rc", # duplicate of 04/01

    "29-svcomp_01-race-2_3b-container_of", # duplicate sv-benchmarks
    "29-svcomp_01-race-2_4b-container_of", # duplicate sv-benchmarks
    "29-svcomp_01-race-2_5b-container_of", # duplicate sv-benchmarks
]

def parse_arguments():
    parser = argparse.ArgumentParser()

    parser.add_argument("-g", "--goblint_path", dest = "goblint_path", default = ".", help="Path to Goblint root.")
    parser.add_argument("-t", "--target_path", dest = "target_path", default = ".", help="Path to the regression tests.")
    parser.add_argument("-f", "--target_folder", dest = "target_folder", default = "**", help="Path to the folder wih a group of tests. Default: all folders.")

    global args
    args = parser.parse_args()

    goblint_root = Path(args.goblint_path)
    global goblint_regression
    goblint_regression = goblint_root / "tests" / "regression"

    global target_root
    target_root = Path(args.target_path)

def process_files():
    for goblint_f in sorted(goblint_regression.glob(args.target_folder+"/*.c")):
        print(goblint_f, end=": ")

        content = goblint_f.read_text()
        # handle & strip Goblint param hints
        m = re.match(r"^//(.*?)\n(.*)$", content, flags=re.DOTALL)
        if m:
            top_comment = m.group(1)
            content = m.group(2)
        else:
            top_comment = None

        if top_comment is not None:
            if "SKIP" in top_comment:
                print("skip")
                continue
            elif "--set kernel true" in top_comment:
                print("kernel")
                continue
            elif "osek" in top_comment:
                print("osek")
                continue
            elif "--set allfuns true" in top_comment:
                print("allfuns")
                continue

        task_name = Path(goblint_f.parent.name + "_" + goblint_f.name).stem
        if task_name in EXCLUDE_TASKS:
            print("exclude")
            continue

        properties = {}

        content = re.sub(r"//\s*RACE(?!!)", "// NORACE", content)
        if re.search(r"//\s*RACE!", content):
            properties["../properties/no-data-race.prp"] = False
        elif re.search(r"//\s*NORACE", content):
            # if didn't contain RACE!, must be race-free
            properties["../properties/no-data-race.prp"] = True

        if re.search(r"assert_racefree[^\n]*//\s*UNKNOWN", content):
            properties["../properties/unreach-call.prp"] = False
        elif "assert_racefree" in content:
            # if didn't contain UNKNOWN assert_racefree, must be race-free
            properties["../properties/unreach-call.prp"] = True

        handle_asserts(properties, content, task_name, top_comment, 0)

def handle_asserts(properties, content, task_name, top_comment, version):
    # TODO: unreach-call property based on asserts

    # Split the file into parts by asserts 
    code_chunks = []
    read = 0
    pattern = re.compile("assert[ \t]*\(.*\)[ \t]*;(.*)*(\r\n|\r|\n)*")
    for match in pattern.finditer(content):
        #print(match.start(), match.group())
        #print(re.search("\\\\(.*)(\r\n|\r|\n)*", match.group()))

        code_before = content[read:match.start()]
    
        code_chunks.append({"kind": "code", "content": code_before})
        
        assertion_code = match.group()
        comment = re.search("\\\\(.*)(\r\n|\r|\n)*", match.group())
        code_chunks.append({"kind": "assert", "content": assertion_code, "comment": comment})

        read = match.start() + len(assertion_code)

    code_after = content[read:]
    
    code_chunks.append({"kind": "code", "content": code_after})

    version = 0
    # Create benchmarks for each UNKNOWN! assert
    prefix_code = ""
    i = 0
    for chunk in code_chunks:
        i += 1
        if chunk["kind"] == "assert":
            if chunk["comment"] != None and chunk["comment"].find("UNKNOWN!") != -1:
                sufix_code = ""
                for chunk2 in code_chunks[i:]:
                    if chunk2["kind"] == "code":
                        sufix_code += chunk2["content"]
                assertion_normal = chunk["content"].replace("assert", "__VERIFIER_assert")
                res = prefix_code
                res += assertion_normal
                res += sufix_code
                properties["../properties/unreach-call.prp"] = False
                wrap_up_assert(properties, task_name + "_v" + str(version), res, top_comment)
                version += 1
                assertion_negated = chunk["content"].replace("assert", "__VERIFIER_assert(!") + ")"
                res = prefix_code
                res += assertion_negated
                res += sufix_code
                properties["../properties/unreach-call.prp"] = False
                wrap_up_assert(properties, task_name + "_v" + str(version), res, top_comment)
                version += 1
        else:
            prefix_code += chunk["content"]   

    # Create one big benchmark for all the other asserts
    res = ""
    for chunk in code_chunks:
        if chunk["kind"] == "assert":
            if chunk["comment"] == None or chunk["comment"].find("UNKNOWN!") == -1:
                if chunk["comment"] != None and chunk["comment"].find("FAIL!") == -1:
                    res += chunk["content"].replace("assert", "__VERIFIER_assert(!") + ")"
                else: 
                    res += chunk["content"].replace("assert", "__VERIFIER_assert")
        else:
            res += chunk["content"]
    properties["../properties/unreach-call.prp"] = True

    wrap_up_assert(properties, task_name + "_v" + str(version), res, top_comment)
    version += 1
            
def wrap_up_assert(properties, task_name, content, top_comment):
    content = content[:].replace("__VERIFIER_ASSERT", "__VERIFIER_assert")
    handle_properties(properties, task_name, content, top_comment)
    
def handle_properties(properties, task_name, content, top_comment):
    if properties:
        print()
        for property_file, expected_verdict in properties.items():
            print(f"  {property_file}: {expected_verdict}")

        # copy file
        target_f = target_root / (task_name + ".c")
        print(f"  -> {target_f}")
        target_f.write_text(content)

        # preprocess file
        preprocessed_f = target_root / (task_name + ".i")
        print(f"  -> {preprocessed_f}")
        preprocessed_f.touch()
        with preprocessed_f.open("w") as f:
            # TODO: change -m32 to -m64
            # running gcc in target_root with relative path avoid absolute paths in preprocessor
            subprocess.run(["gcc", "-E", "-P", "-m32", str(target_f.relative_to(target_root))], stdout=f, check=True, cwd=target_root)

        # create task definition
        task_definition_f = target_root / (task_name + ".yml")
        task_definition_f.touch()
        # write yml manually to get consistent formatting
        with task_definition_f.open("w") as f:
            f.write("format_version: '2.0'\n")
            f.write("\n")
            if top_comment:
                f.write(f"# original top comment: {top_comment}\n")
            f.write(f"input_files: '{preprocessed_f.relative_to(target_root)}'\n")
            f.write("\n")
            f.write("properties:\n")
            for property_file, expected_verdict in properties.items():
                f.write(f"  - property_file: {property_file}\n")
                f.write(f"    expected_verdict: {'true' if expected_verdict else 'false'}\n")
            f.write("\n")
            f.write("options:\n")
            f.write("  language: C\n")
            f.write("  data_model: ILP32\n") # TODO: is this right for Goblint tests?

    else:
        print("no properties")

def main():
    parse_arguments()
    process_files()

if __name__ == "__main__":
    main()
