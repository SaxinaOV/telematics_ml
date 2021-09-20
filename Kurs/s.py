with open("ecoli.data", "r+") as f:
	f2 = open("ecoli.data2", "w+")
	for line in f:
		space = 0
		i=0
		while line[i] != '\n':
			print(line)
			if line[i] == ' ' or line[i] == '\t':
				line = line[:i] + '' + line[i+1:]
				i -= 1
				space = 1
			elif line[i] != ' ' and space == 1:
				line = line[:i] + '\t' + line[i:]
				space = 0
			i += 1
		f2.write(line)
		f2.write('\n')
f2.close()
