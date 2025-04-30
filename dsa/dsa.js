function Z(...args) {
  for (const i in args) {
    if (Array.isArray(args[i])) {
      args[i] = args[i].map((arg) => JSON.stringify(arg, null, 0));
      args[i] = "[" + args[i].join(" ") + "]";
    }
  }

  console.log(...args);
}

// const MAX_SAFE_INT = Math.pow(2, 31) - 1;
const PRIME = 1000003;
const ASCII_SIZE = 128;

function charToAscii(c) {
  return c.charCodeAt(0);
}

/*
 * @param {string} S - The string to convert to integer.
 * @param {int} d - The alphabet size.
 * @param {} charToI - Function mapping each element in S to an integer.
 */
function atoi(S, d = ASCII_SIZE, charToI = (x) => x) {
  let a = 0;

  for (let i = 0; i < S.length; i++) {
    a = d * a + charToI(S[i]);
  }

  return a;
}

/*
 * Actual mod that returns a positive mod, not remainder that javascript '%' does
 * @param {int} m - number
 * @param {int} n - mod
 */
function mod(m, n) {
  return ((m % n) + n) % n;
}

/*
 * For taking large logs. Not actually necessary in JavaScript as it can represent arbitrarily large numbers however.
 * Function expects all integer arguments.
 */
function largeExpMod(x, n, q) {
  let a = 1;
  for (i = 1; i <= n; i++) {
    a = (a * x) % q;
  }
  return a;
}

/*******************************************************************************
 *
 * Sort
 *
 *******************************************************************************/

function insertionSort(A) {
  for (let j = 1; j < A.length; j++) {
    let key = A[j];
    i = j - 1;

    while (i >= 0 && key < A[i]) {
      A[i + 1] = A[i];
      i--;
    }

    A[i + 1] = key;
  }

  return A;
}

function insertionReverseSort(A) {
  for (let j = A.length - 2; j >= 0; j--) {
    let key = A[j];
    i = j + 1;

    while (i <= A.length - 1 && key < A[i]) {
      A[i - 1] = A[i];
      i++;
    }

    A[i - 1] = key;
  }

  return A;
}

/*
 * Merges two sorted subarrays A[p..q-1] and A[q..r] into sorted subarry A[p..r] where the subarray subscript is inclusive
 */
function merge(A, p, q, r) {
  let L = [...A.slice(p, q), Infinity]; // O(q-p+1) memory and time
  let R = [...A.slice(q, r + 1), Infinity]; // O(r-q+2) memory and time

  let nl = 0;
  let nr = 0;

  for (let i = p; i <= r; i++) {
    // O(r-p+1) time
    if (L[nl] < R[nr]) {
      A[i] = L[nl];
      nl++;
    } else {
      A[i] = R[nr];
      nr++;
    }
  }

  // O(q-p+1) + O(r-q+2) + O(r-p+1) = O(2(r-p+4))
  return A;
}

function mergeSort(A, p, r) {
  if (p === undefined && r === undefined) {
    p = 0;
    r = A.length - 1;
  }
  if (r <= p) {
    return;
  }

  let q = Math.ceil((r + p) / 2);
  mergeSort(A, p, q - 1);
  mergeSort(A, q, r);
  merge(A, p, q, r);

  return A;
}

/*
 * A function binaryAdd(A, B): C
 * Such that A, B, C: Array; A.len = B.len; C.len = A.len + 1; A, B, C only have elements 0 and 1; for a function mapping a binary array to its corresponding number binaryToInteger, binaryToInteger(binaryAdd(A,B)) = binaryToInteger(A) + binaryToInteger(B); binary arrays have the largest digit in the first index;
 */
function binaryAdd(A, B) {
  let C = Array(A.length + 1).fill(0);

  for (let i = A.length - 1; i >= 0; i--) {
    let sum = A[i] + B[i] + C[i + 1];
    C[i + 1] = sum % 2;
    C[i] = sum >= 2 ? 1 : 0; // carry the addition
  }

  return C;
}

/*******************************************************************************
 *
 * Search
 *
 *******************************************************************************/

function naiveStringMatch(T, P) {
  T = [...T];
  P = [...P];
  let matchCount = 0;

  // This loop is inclusive [0, T.length - P.length] as opposed to exclusive [0, T.length - P.length)
  // because you're subtracting by P.length which is removing by an extra parameter [0, P.length] instead of [0, P.length - 1]
  // If you were subtracting by multiple array lengths, you would want to add a "-1" per subtracted array length
  for (let i = 0; i <= T.length - P.length; i++) {
    if (T.slice(i, i + P.length).join("") == P.join("")) {
      matchCount++;
    }
  }

  return matchCount;
}

/*
 * @param {string} T - The text string in which we are checking if Z is a substring.
 * @param {string} Z - The substring we are looking for in T.
 * @param {int} d - The alphabet size.
 * @param {int} q - The modulo value.
 */
function rabinKarpMatch(T, P, d = ASCII_SIZE, q = PRIME, charToI) {
  T = [...T].map(charToI);
  P = [...P].map(charToI);
  let h = largeExpMod(d, P.length - 1, q);
  let t = atoi(T.slice(0, P.length), d) % q;
  let p = atoi(P, d) % q;

  Z(T, P, p, atoi(P, d));

  const matches = [];
  for (let s = 0; s <= T.length - P.length; s++) {
    if (p === t) {
      if (naiveStringMatch(P, T.slice(s, s + P.length))) {
        Z("match at", s);
      }
      matches.push({ p, s });
    }

    if (s < T.length - P.length) {
      t = mod(d * (t - h * T[s]) + T[s + P.length], q);
    }
  }

  Z(matches);
}

function rabinKarpMatchMulti(T, Ps, d = ASCII_SIZE, q = PRIME, charToI) {
  const _T = T;
  T = [...T].map(charToI);
  const Patterns = Ps.reduce((acc, pattern) => {
    const P = [...pattern].map(charToI);
    acc[pattern] = {
      P: P,
      p: atoi(P, d) % q,
      t: atoi(T.slice(0, P.length), d) % q,
      matches: [],
      spuriousHits: [],
      h: largeExpMod(d, P.length - 1, q),
    };
    return acc;
  }, {});

  for (let s = 0; s < T.length; s++) {
    for (const [key, pattern] of Object.entries(Patterns)) {
      const { P, p, t, matches, spuriousHits, h } = pattern;

      if (s <= T.length - P.length) {
        if (p === t) {
          const match = _T.slice(s, s + P.length);
          if (pattern === match) {
            matches.push(match);
          } else {
            spuriousHits.push(match);
          }
        }
      }

      if (s < T.length - P.length) {
        pattern.t = mod(d * (t - h * T[s]) + T[s + P.length], q);
      }
    }
  }

  Z(Patterns);
}

/*
 * For arrays sorted in non-increasing order
 */
function binaryReverseSearch(A, a) {
  let l = 0;
  let r = A.length - 1;
  let m;

  while (l != r) {
    m = Math.floor((l + r) / 2);
    if (A[m] > a) {
      l = m + 1;
    } else {
      r = m;
    }
  }

  if (A[l] === a) {
    return l;
  }
  return null;
}

function binarySearch(A, a) {
  let l = 0;
  let r = A.length - 1;
  let loops = 0;

  while (l != r) {
    loops += 1;
    let m = Math.ceil((r + l) / 2);
    if (a < A[m]) {
      r = m - 1;
    } else {
      l = m;
    }
  }

  if (A[l] === a) {
    return { loops, index: l };
  }

  // while (l <= r) {
  //   loops += 1;
  //   let m = Math.floor((r + l)/2);
  //   if (a < A[m]) {
  //     r = m - 1;
  //   } else if (a > A[m]) {
  //     l = m + 1;
  //   } else {
  //     return {loops, index: m};
  //   }
  // }
  return { loops, index: null };
}

/*******************************************************************************
 *
 * Graphs
 *
 *******************************************************************************/

/*
 * Multigraph to undirected graph:
 * Let E' represent the new edges
 * Let (u,v) = (null,null) represent the current edge
 * For each edge in E:
 *   If the edge is a self-loop let (u,v) = edge or if (u,v) = edge, continue
 *   Otherwise, add v to Adj[u] and u to Adj[v] in the Adj for E'
 *   How to prevent adding another edge if there is already an edge (v,u)?
 *   Let (u,v) = edge
 */

if (!module.parent) {
  const args = process.argv.slice(2);

  switch (args[0]) {
    case "ds":
    case "data-structures":
      break;
    case "search": {
      const A = [1, 2, 3, 4, 5, 8];
      Z(4, binaryReverseSearch([8, 6, 4, 4, 4, 2], 4));
      Z(2, binarySearch([1, 2], 2));
      Z(3, binarySearch(A, 3));
      Z(5, binarySearch(A, 5));
      Z(6, binarySearch(A, 6));
      Z(14, binarySearch([4, 9, 10, 11, 14], 14));
      break;
    }
    case "string":
      // Z(naiveStringMatch("hello", "he"));
      // Z(naiveStringMatch("hello", "llo"));
      // Z(
      //   rabinKarpMatch(
      //     "2359023141526739921".split("").map(Number),
      //     "31415".split("").map(Number),
      //     10,
      //     13,
      //     (x) => x,
      //   ),
      // );
      // Z(rabinKarpMatch([3, 2, 5, 4, 0], [5, 4, 0], 10, 1000003, (x) => x));
      // Z(
      //   rabinKarpMatch(
      //     "Testing",
      //     "ing",
      //     128,
      //     1009,
      //     (c) => c.charCodeAt(0),
      //   ),
      // );
      Z(
        rabinKarpMatchMulti(
          "testing here bruv. you what mate? are another test exp you getting rude to man?",
          ["test", "mate", "are", "getting rude", "another test", "man?"],
          128,
          1009,
          (c) => c.charCodeAt(0),
        ),
      );
      break;
    case "sort": {
      const A = [6, 2, 3, 4, 2, 1, 5];
      // Z(insertionReverseSort(A))
      // Z(insertionSort(A))
      // Z(mergeSort(A));
      break;
    }
    default:
      Z(binaryAdd([1, 0], [1, 1]));
      Z(binaryAdd([1, 1], [1, 1]));
      Z(binaryAdd([1, 1, 1, 1], [1, 1, 1, 1]));
      break;
  }
}

module.exports = {
  atoi,
  binarySearch,
  binaryReverseSearch,
  charToAscii,
  rabinKarpMatch,
  rkm: rabinKarpMatch,
  naiveStringMatch,
  mergeSort,
  insertionSort,
  insertionReverseSort,
};
