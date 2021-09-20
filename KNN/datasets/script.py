
def main():
    with open("Titanic_test.csv") as f_in, open("Titanic_test2.csv", 'w') as f_out:
        header = f_in.readline()
        f_out.write(header)
        for line in f_in:
            print(line)
            l = line[1:-2] + "\n"
            quotes = 0
            i = 0
            while quotes < 2:
                if l[i] == '"' and l[i+1] == '"':
                    quotes += 1
                    l = l[:i] + l[i+1:]
                i += 1
            f_out.write(l)

main()