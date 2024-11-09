subroutine mat_mul(a, b, c)
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none

    real(dp), intent(in) :: a(128 * 128)
    real(dp), intent(in) :: b(128 * 128)
    real(dp), intent(out) :: c(128 * 128)

    real(dp) :: v
    integer :: row, col, i

    do row = 0, 127
        do col = 0, 127
            v = 0
            do i = 0, 127
                v = v + a(row * 128 + i + 1) * b(i * 128 + col + 1)
            end do
            c(row * 128 + col + 1) = v
        end do
    end do
end subroutine mat_mul

subroutine mat_add(m, x)
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none

    real(dp), intent(inout) :: m(128 * 128)
    real(dp), intent(in) :: x

    integer :: i
    do i = 1, 128 * 128
        m(i) = m(i) + x
    end do
end subroutine mat_add

subroutine mat_div(m, x)
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none

    real(dp), intent(inout) :: m(128 * 128)
    real(dp), intent(in) :: x

    integer :: i
    do i = 1, 128 * 128
        m(i) = m(i) / x
    end do
end subroutine mat_div

program float
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none

    integer :: i
    real(dp) :: rv
    real(dp) :: a(128 * 128)
    real(dp) :: b(128 * 128)
    real(dp) :: c(128 * 128)
    real(dp) :: sum_value
    real(dp) :: tp1, tp2
    
    ! a = [(real(i, dp) / size(a), i = 1, size(a))]
    do i = 1, 1000
        call random_number(rv)
        a(i) = rv
    end do
    a(128 * 128) = 1
    b = 1
    
    call cpu_time(tp1)

    do i = 1, 1000
        call mat_add(b, real(i - 1, dp))
        call mat_mul(a, b, c)

        call mat_add(b, real(i, dp))
        call mat_mul(c, b, a)

        call mat_div(a, c(128 * 128))
    end do

    sum_value = 0
    do i = 1, 128 * 128
        sum_value = sum_value + a(i)
    end do

    call cpu_time(tp2)

    print *,(tp2-tp1)*1000,sum_value
  
end program float