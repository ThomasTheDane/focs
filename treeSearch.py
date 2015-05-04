doms = [("1000", "0"), ("01", "0"), ("1", "101"), ("00", "001")]
# doms = [("0", "00"), ("011", "1"), ("10", "11")]
def bfs():
	queue = []
	depth = 0
	for aDom in doms:
		queue.append((aDom[0],aDom[1]))

	while queue:
		vertex = queue.pop(0)
		if depth % 10000 == 0:
			print depth
			print min(len(vertex[0]), len(vertex[1]))
		depth = depth + 1
		if vertex[0] == vertex[1]:
			return vertex
		for aDom in doms:
			minLength = min(len(vertex[0]), len(vertex[1]))
			if vertex[0][0:minLength] == vertex[1][0:minLength]:
				queue.append((vertex[0] + aDom[0], vertex[1] + aDom[1]))

print len(bfs()[0])
