# TODO: Create function to calculate the time complexity of algorithms
import math

class Data:
    def __init__(self, n, sort=False):
        self.n = n

def insertion_sort_inplace(inputs: list) -> list:
    """
    Examples:
    >>> insertion_sort_inplace([3,2,1])
    [1, 2, 3]
    """

    for j in range(1, len(inputs)):
        key = inputs[j]
        i = j - 1
        # Swap indexes until key is greater
        while i >= 0 and inputs[i] > key:
            inputs[i+1] = inputs[i]
            i -= 1
        inputs[i+1] = key

    # for i1, I1 in enumerate(inputs):
    #     for i2, I2 in enumerate(inputs):
    #         if I1 < I2:
    #             inputs[i1], inputs[i2] = inputs[i2], inputs[i1]

    return inputs

def insertion_sort(inputs: list) -> list:
    """
    Examples:
    >>> insertion_sort([3,2,1])
    [1, 2, 3]
    """
    # Initialize an empty collection for values ordered from least to greatest.
    outputs = []

    for value in inputs:
        # Iterate through each value and insert it before the first value that is greater than it.
        for i, output in enumerate(outputs):
            if value < output:
                outputs.insert(i, value)
                break
        else:
            outputs.append(value)

    return outputs

def merge_sort_inplace(inputs: list) -> list:
    """
    Examples:
    >>> merge_sort_inplace([3,2,1])
    [1, 2, 3]
    """
    A = inputs

    def _merge_sort(p, r):
        if r - p <= 1:
            return
        q = (p + r) // 2
        _merge_sort(p, q)
        _merge_sort(q, r)
        merge(p,q,r)

    def merge(p,q,r):
        """Merge A[p:q] and A[q:r]
        Where:
            p <= q < r
            A[p:q] and A[q:r] are sorted
        """
        if r - q < 2: # insertion_sort has lower constant time
            A[p:r] = insertion_sort(A[p:r])
            return
        L, R = A[p:q], A[q:r]
        L.append(math.inf)
        R.append(math.inf)
        left, right = 0, 0

        for n in range(p, r):
            if L[left] < R[right]:
                A[n] = L[left]
                left += 1
            else:
                A[n] = R[right]
                right += 1

    _merge_sort(0, len(A)) # expects 1 + greatest index
    return A

def merge_sort(inputs: list) -> list:
    """Divide and conquer algorithm which splits lists and merge_sorts them, then combining the results while preserving the order
    Examples:
    >>> merge_sort([3,2,1])
    [1, 2, 3]
    """

    def _merge_sort(A: list) -> list:
        """Split A into two sub arrays and sort them
        """

        if len(A) == 1:
            return A
        n = len(A) // 2
        L = _merge_sort(A[:n])
        R = _merge_sort(A[n:])

        return merge(L,R)

    def merge(L: list, R: list) -> list:
        """Merge the left and right lists in increasing order
        """

        def _merge():
            while L and R:
                yield (L if L[0] <= R[0] else R).pop(0)
            yield from L
            yield from R

        return list(_merge())

#         A = []
#         while R and L:
#             A.append((L if L[0] < R[0] else R).pop(0))
#         return A + L + R

#         A = []
#         l, r = 0, 0
#         while r < len(R) or l < len(L):
#             if r >= len(R) or l < len(L) and L[l] <= R[r]:
#                 A.append(L[l])
#                 l += 1
#             elif l >= len(L) or r < len(R) and R[r] <= L[l]:
#                 A.append(R[r])
#                 r += 1
#         return A

    return _merge_sort(inputs)

def binary_search(inputs, value):
    """Expects a sorted list
    Examples:
    >>> binary_search([1,2,3], 1)
    0
    >>> binary_search([1,2,3], 2)
    1
    >>> binary_search([1,2,3], 3)
    2
    """
    p, r = 0, len(inputs) - 1
    while p <= r:
        q = math.floor((p + r) / 2)
        if value < inputs[q]:
            r = q - 1
        elif value > inputs[q]:
            p = q + 1
        else:
            return q

def binary_search_sort(inputs):
    """Sorts"""
    # Initialize empty list A
    # For each input binary search for input in A
    # Either replace or insert input in location
    raise NotImplementedError

if __name__ == '__main__':
    import doctest
    doctest.testmod()
