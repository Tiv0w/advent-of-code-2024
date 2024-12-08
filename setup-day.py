import sys
import requests
import pyfiglet
import re
import pypandoc


if len(sys.argv) < 2:
    print("Usage: python setup-day.py <day>")
    sys.exit(1)

day = sys.argv[1]


SESSION_COOKIE = r"redacted"


full_page = requests.get(f"https://adventofcode.com/2024/day/{day}", cookies={"session": SESSION_COOKIE})
matches = re.search("<pre><code>(.*)</code></pre>", full_page.text, flags=re.DOTALL)
with open(f"./examples/day{day}.txt", 'w') as f:
    if matches:
        f.write(matches.group(1))
print(f"Written ./examples/day{day}.txt")

pypandoc.convert_text(full_page.text, to="md", format="html", outputfile=f"./problems/day{day}.md")
print(f"Written ./problems/day{day}.md")

input_page = requests.get(f"https://adventofcode.com/2024/day/{day}/input", cookies={"session": SESSION_COOKIE})
with open(f"./input/day{day}.txt", "wb") as f:
    f.write(input_page.content)
print(f"Written ./input/day{day}.txt")


with open(f"./src/day{day}.sc", "w") as f:
    f.write(f"""import scala.io.Source
import pprint.pprintln


def readInput(s: String) = Source.fromFile(s).getLines.filterNot(_.isBlank).toVector

def part1() = {{
  val input = readInput("./examples/day{day}.txt")
  pprintln(input)
}}

part1()
""")
print(f"Written ./src/day{day}.sc")


print(pyfiglet.figlet_format("Happy Advent of Coding!"))