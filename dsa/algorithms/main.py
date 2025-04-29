import random
import time
import math


def timeit(method):
    def timed(*args, **kw):
        ts = time.time()
        result = method(*args, **kw)
        te = time.time()
        if 'log_time' in kw:
            name = kw.get('log_name', method.__name__.upper())
            kw['log_time'][name] = int((te - ts) * 1000)
        else:
            print(f'{method.__name__:<15} {(te - ts) * 1000:.2f}: {result}')
        return result
    return timed


@timeit
def selection_sort(data):
    for i in range(len(data)):
        min = math.inf
        for j in range(i, len(data)):
            if data[j] < min:
                min = data[j]
                key = j
        data[key] = data[i]
        data[i] = min
    return data


@timeit
def insertion_sort(data):
    for j in range(1, len(data)):
        key = data[j]
        i = j - 1
        while i >= 0 and data[i] > key:
            data[i + 1] = data[i]
            i = i - 1
        data[i + 1] = key
    return data


@timeit
def insertion_sort_v2(data):
    for i in range(len(data)):
        for j in range(i, 0, -1):
            if data[j] < data[j - 1]:
                j1, j2 = data[j], data[j - 1]
                data[j] = j2
                data[j-1] = j1
    return data


count = 0


@timeit
def merge_sort(A):
    global count
    count = 0

    def merge_sort(A, p, r):
        def merge(A, p, q, r):
            n1 = q - p + 1
            n2 = r - q
            L = []
            R = []

            for i in range(0, n1):
                L.append(A[p + i])
            for j in range(0, n2):
                R.append(A[q + j + 1])

            L.append(math.inf)
            R.append(math.inf)
            i = j = 0

            for k in range(p, r + 1):
                if L[i] <= R[j]:
                    A[k] = L[i]
                    i += 1
                else:
                    if L[i] != math.inf:
                        global count
                        count += len(L[i:]) - 1
                    A[k] = R[j]
                    j += 1

        if p < r:
            q = math.floor((p + r) / 2)
            merge_sort(A, p, q)
            merge_sort(A, q + 1, r)
            merge(A, p, q, r)

        return p, r, A, count
    return merge_sort(A, 0, len(A) - 1)


@timeit
def linear_search(data, value):
    key = None
    for i, d in enumerate(data):
        if d == value:
            key = i
    return key, data


def data(size=10):
    x = list(range(0, size))
    random.shuffle(x)
    return x


if __name__ == '__main__':
    # sort
    insertion_sort(data(10))
    insertion_sort_v2(data(10))
    selection_sort(data())
    merge_sort(list(range(9, -1, -1)))
    merge_sort(list(range(0, 10)))

    # search
    linear_search(data(), 4)
