import sys
import os
import progressbar

widgets = "checking..."

bar = progressbar.ProgressBar(widgets=widgets).start()
def file_check(path):
    file = open(path, 'r')
    return sum(1 for line in file)

def folder_check(path):
    global bar
    content = os.listdir(path)
    result = 0
    for i in range(0, len(content) - 1):
        item_path = os.path.join(path, content[i])
        try:
            result += file_check(item_path) if os.path.isfile(item_path)  else folder_check(item_path)
        except:
            pass
        bar.update()
    return result

path = sys.argv[1] if len(sys.argv) != 1 else "."
line_count = 0

line_count = file_check(path) if os.path.isfile(path) else folder_check(path)
print(f"Number of lines: {line_count}")

