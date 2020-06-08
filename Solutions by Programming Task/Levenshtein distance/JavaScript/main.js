const LevenshteinDistance = (a, b) => {
    const rec = (i, j) => {
        if (i === -1) return j + 1;
        if (j === -1) return i + 1;
        if (a[i] === b[j]) {
            return rec(i - 1, j - 1)
        } else {
            return Math.min(
                rec(i, j - 1) + 1,
                rec(i - 1, j) + 1,
                rec(i - 1, j - 1) + 1);
        }
    };
    return rec(a.length - 1, b.length - 1);
}
