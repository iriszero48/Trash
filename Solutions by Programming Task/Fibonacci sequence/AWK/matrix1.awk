func mat_col_impl(mat, rows, elems) {
    split(mat, rows, "\n")
    split(rows[1], elems, " ")
    return length(elems)
}

func mat_col_num(mat) {
    return mat_col_impl(mat)
}

func mat_row_num_impl(mat, rows) {
    split(mat, rows, "\n")
    return length(rows)
}

func mat_row_num(mat) {
    return mat_row_num_impl(mat)
}

func mat_get_impl(mat, row, col, rows, cols) {
    split(mat, rows, "\n")
    split(rows[row], cols, " ")
    return cols[col]
}

func mat_get(mat, row, col) {
    return mat_get_impl(mat, row, col)
}

func mat_mul_impl(a, b, row_num, col_num, i, j, k, mat, r, c) {
    row_num = mat_row_num(a)
    col_num = mat_col_num(b)
    for (i = 1; i <= row_num; ++i) {
        for (j = 1; j <= col_num; ++j) {
            mat[i,j] = 0
            for (k = 1; k <= mat_col_num(a); ++k) {
                mat[i,j] += mat_get(a, i, k) * mat_get(b, k, j)
            }
        }
    }

    r = ""
    for (i = 1; i <= row_num; ++i) {
        c = ""
        for (j = 1; j <= col_num; ++j) {
            c = c mat[i,j]
            if (j != col_num) c = c " " 
        }
        r = r c
        if (i != row_num) r = r "\n"
    }

    return r
}

func mat_mul(a, b) {
    return mat_mul_impl(a, b)
}

func fib_impl(x, fib_mat, res, i) {
    fib_mat = "1 1\n1 0"
    res = fib_mat

    for (i = 0; i < x; ++i) {
        res = mat_mul(res, fib_mat)
    }

    return mat_get(res, 2, 2)
}

func fib(x) {
    return fib_impl(x)
}

BEGIN {
    for (i = 0; i <= 10; ++i) {
        print fib(i)
    }
}
