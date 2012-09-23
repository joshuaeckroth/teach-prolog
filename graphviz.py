from subprocess import Popen, PIPE
import re

def create_graph(prefix, graphviz):
    for i in range(len(graphviz)):
        g = "digraph G {\n"
        g += "node[fontname=\"Courier\"]; edge[fontname=\"Courier\"]; graph[fontname=\"Courier\"];\n"
        for j in range(i+1):
            if j == (len(graphviz)-1):
                g += re.sub(r'color="black"', 'color="blue"', graphviz[j])
            else:
                g += graphviz[j]
        if i != (len(graphviz)-1):
            g += "node [color=\"white\", fontcolor=\"white\"]; edge [color=\"white\", fontcolor=\"white\"];\n"
        for j in range(i+1, len(graphviz)):
            g += re.sub(r'color=".*?"', 'color="white"', graphviz[j])
        g += "}\n"
        p = Popen(['dot', '-Tpng', '-o', "%s-%03d.png" % (prefix, i)],
                  stdin=PIPE, stdout=PIPE, stderr=PIPE)
        p.stdin.write(g)
        p.communicate()



