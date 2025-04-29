def sliding_window(X):
    sum = X[0] + X[1] + X[2]
    max =  sum
    for i in range(3, len(X)):
        sum += X[i] - X[i-3]
        if sum > max:
            max = sum
    return max

print(sliding_window([3,5,-3,0,0,-3,-3,3,4,-2,-4,-4,5,4,-3,-1,-2,0,-3,-2]))

