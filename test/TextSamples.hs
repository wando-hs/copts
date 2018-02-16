module TextSamples where


navalFate = "Naval Fate.\n\
\\n\
\Usage:\n\
\  naval_fate ship new <name>...\n\
\  naval_fate ship <name> move <x> <y> [--speed=<kn>]\n\
\  naval_fate ship shoot <x> <y>\n\
\  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]\n\
\  naval_fate -h | --help\n\
\  naval_fate --version\n\
\\n\
\Options:\n\
\  -h --help     Show this screen. \n\
\  --version     Show version.  \n\
\  --speed=<kn>  Speed in knots [default: 10].\n\
\  --moored      Moored (anchored) mine.\n\
\  --drifting    Drifting mine.\n\
\\n\
\Examples:\n\
\\n\
\  naval_fate ship new AAA BBB"

myProgram = "Usage:\n\
\  my_program command --option <argument>\n\
\  my_program [<optional-argument>]\n\
\  my_program --another-option=<with-argument>\n\
\  my_program (--either-that-option | <or-this-argument>)\n\
\  my_program <repeating-argument> <repeating-argument>...\n\
\  my_program [command --option <argument>] <q>\n\
\  my_program [command] [--option] [<argument>] <q>"
