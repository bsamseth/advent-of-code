with open('input1.txt', 'r') as f:
    freq = 0
    for line in f:
        freq += eval(line.strip())
    print('Final frequency:', freq)
