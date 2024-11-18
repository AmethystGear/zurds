# copies files from `res/` to `pkg/`, fails if there would be any name conflicts.
import os
import shutil
import sys

def copy_contents(source_dir, target_dir):
    for item in os.listdir(source_dir):
        source_path = os.path.join(source_dir, item)
        target_path = os.path.join(target_dir, item)
        if os.path.exists(target_path):
            print(f"\033[91mConflict detected: {target_path} already exists.\033[0m", file=sys.stderr)
            sys.exit(1)

        if os.path.isdir(source_path):
            shutil.copytree(source_path, target_path)
        else:
            shutil.copy2(source_path, target_path)

try:
    copy_contents('res', 'pkg')
    print("\033[92mCopy completed successfully\033[0m")
except Exception as e:
    print(f"\033[91mError: {e}\033[0m", file=sys.stderr)
    sys.exit(1)
