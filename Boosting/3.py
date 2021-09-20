import numpy as np
from collections import Counter
import csv
import random
import copy
from sklearn.tree import DecisionTreeClassifier

def distance(instance1, instance2):
    instance1 = np.array(instance1) 
    instance2 = np.array(instance2)
    return np.linalg.norm(instance1 - instance2)

def get_distances(tr, te):
    train = copy.deepcopy(tr)
    test = copy.deepcopy(te)
    for i in range(len(te)):
        test[i].pop(-1)
    for i in range(len(tr)):
        train[i].pop(-1)
    distances = {}
    test_ind = 0
    for i in test:
        distances[test_ind] = []
        train_ind = 0
        for j in train:
            distances[test_ind].append([train_ind, distance(i, j)])
            train_ind += 1
        test_ind += 1
    return distances

def get_neighbors(train, train_labels, test_num, k, distances):
    #neighbour = (index, dist, class, weight)
    neighbors = []
    distances[test_num].sort(key=lambda x: x[1])
    neighbors = copy.deepcopy(distances[test_num][:k])
    for i in range(k):
        neighbors[i].append(train_labels[neighbors[i][0]])
        neighbors[i].append(train[neighbors[i][0]][-1])
    return(neighbors)

def vote(neighbors):
    class_counter = Counter()
    for neighbor in neighbors:
        class_counter[neighbor[2]] += neighbor[3]
    return class_counter.most_common(1)[0][0]

def add_weight(test_num, right_label, wrong_label, train, train_labels, neighbors):
    for n in neighbors:
        ind = n[0]
        if train_labels[ind] == right_label:
            train[ind][-1] += 0.5
        elif train_labels[ind] == wrong_label:
            train[ind][-1] -= 0.5

def read_data(file):
    data = []
    with open(file) as csvfile:
        reader = csv.reader(csvfile)
        for row in reader: 
            data.append(row)
    for row in range(1,len(data)):
        for el in range(len(data[row]) - 1):
            data[row][el] = float(data[row][el])
        try:
            data[row][-1] = int(float(data[row][-1]))
        except:
            pass
    return data

def knn(train0, main_train_labels):
    for s in range(len(train0)):
        print('\n____________________')
        print(str(s) + '__________________\n')
        k = 6
        train_labels = copy.deepcopy(main_train_labels)
        test = [train0[s]]
        test_labels = [train_labels[s]]
        if s == 0:
            train = train0[1:]
        elif s == len(train0):
            train = train0[:s]
        else: 
            train = train0[:s] + train0[s+1:]
        d = get_distances(train, test)
        votes = []
        for test_ind in range(len(test)):
            neighbors = get_neighbors(train, train_labels, test_ind, k, d)
            votes.append(vote(neighbors))
        wrongs = {}
        for i in range(len(votes)):
            if votes[i] != test_labels[i]:
                wrongs[i] = (test_labels[i], votes[i])
                print(str(i) + ') predicted: ' + str(votes[i]) + ' real: ' + str(test_labels[i]))
        for i in wrongs:
            add_weight(i, wrongs[i][0], wrongs[i][1], train, train_labels, get_neighbors(train, train_labels, i, k+1, d))

        print('\n')
        if wrongs:
            k += 1
            votes = []
            for test_ind in range(len(test)):
                neighbors = get_neighbors(train, train_labels, test_ind, k, d)
                votes.append(vote(neighbors))
            wrongs = {}
            for i in range(len(votes)):
                if votes[i] != test_labels[i]:
                    wrongs[i] = (test_labels[i], votes[i])
                    print(str(i) + ') predicted: ' + str(votes[i]) + ' real: ' + str(test_labels[i]) )
            for i in wrongs:
                add_weight(i, wrongs[i][0], wrongs[i][1], train, train_labels, get_neighbors(train, train_labels, i, k+1, d))
            print('\n')

            if wrongs:
                k += 1
                votes = []
                for test_ind in range(len(test)):
                    neighbors = get_neighbors(train, train_labels, test_ind, k, d)
                    votes.append(vote(neighbors))
                wrongs = {}
                for i in range(len(votes)):
                    if votes[i] != test_labels[i]:
                        wrongs[i] = (test_labels[i], votes[i])
                        print(str(i) + ') predicted: ' + str(votes[i]) + ' real: ' + str(test_labels[i]) )
                for i in wrongs:
                    add_weight(i, wrongs[i][0], wrongs[i][1], train, train_labels, get_neighbors(train, train_labels, i, k+1, d))
    return train0

def main():
    #data = read_data('glass.csv')
    data = read_data('vehicle_csv.csv')
    rows = len(data)
    data[0].insert(-1, "weight")
    for i in range(rows):
        data[i].insert(-1, 1)
    cols = len(data[0])
    names = data[0]
    data.pop(0)
    random.shuffle(data)
    train_size = round(0.7 * rows)
    train0 = data[:train_size]
    test0 = data[train_size:]
    main_train_labels = [row[-1] for row in train0]
    main_test_labels = [row[-1] for row in test0]
    for row in test0:
        row.pop(-1)
    for row in train0:
        row.pop(-1) 
    #tree
    clf = DecisionTreeClassifier(max_depth = 4, random_state = 0)
    clf.fit(train0, main_train_labels)
    clf.predict(test0)
    score = clf.score(test0, main_test_labels)
    #knn
    train = knn(train0, main_train_labels)
    k = 8
    test = test0
    train_labels = copy.deepcopy(main_train_labels)
    test_labels = copy.deepcopy(main_test_labels)
    d = get_distances(train, test)
    votes = []
    for test_ind in range(len(test)):
        neighbors = get_neighbors(train, train_labels, test_ind, k, d)
        votes.append(vote(neighbors))
    wrongs = {}
    print('\n test \n')
    for i in range(len(votes)):
        if votes[i] != test_labels[i]:
            wrongs[i] = test_labels[i]
            print(str(i) + ') predicted: ' + str(votes[i]) + ' real: ' + str(test_labels[i]))
    print('\n knn missclasification = ' + str(len(wrongs)/len(test)))
    print('\n tree missclasification = ' + str(1 - score))


main()


'''
data = read_data('glass.csv')
rows = len(data)
cols = len(data[0])
names = data[0]
data.pop(0)
random.shuffle(data)
#упорядочить items по измерению
data.sort(key=lambda x: x[9])
#выбрать первые 10 и последние 10
first = data[:10]
last = data[-10:]
#посчитать классы в обеих выборках
first_counter = Counter()
for item in first:
    first_counter[item[-1]] += 1
first_class = first_counter.most_common(1)[0]
last_counter = Counter()
for item in last:
    last_counter[item[-1]] += 1
last_class = last_counter.most_common(1)[0]
print()
'''