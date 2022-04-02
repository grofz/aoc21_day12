  module route
    use node
    implicit none

    integer, parameter :: ROUTE_DONE=1, ROUTE_FAIL=-1, ROUTE_INCOMPLETE=0

    type route_t
      type(node_t), allocatable :: r(:)
    contains
      procedure :: test => route_test
      procedure :: print => route_print
      procedure, private :: route_copy
      generic :: assignment(=) => route_copy
    end type

    type list_t
      type(route_t), allocatable :: m(:)
      integer :: n = 0
    contains
      procedure, private :: list_copy
      generic :: assignment(=) => list_copy
    end type

    interface route_t
       module procedure route_init
    end interface route_t
    interface list_t
       module procedure list_init
    end interface list_t


  contains

    function route_init() result(new)
      type(route_t) :: new
      allocate(new % r(0))
    end function

    function list_init() result(new)
      type(list_t) :: new
      allocate(new % m(0))
      new % n = 0
    end function


    subroutine route_copy(a, b)
      class(route_t), intent(out) :: a
      type(route_t), intent(in) :: b
      integer :: i

      allocate(a % r(size(b%r)))
      do i = 1, size(b%r)
        a % r(i) = b % r(i)
      enddo
    end subroutine route_copy



    subroutine list_copy(a, b)
      class(list_t), intent(out) :: a
      type(list_t), intent(in) :: b
      integer :: i

      allocate(a % m(size(b%m)))
      do i = 1, b % n
        a % m(i) = b % m(i)
      enddo
      a % n = b % n
    end subroutine list_copy



    subroutine route_print(this)
      class(route_t), intent(in) :: this
      integer :: i

      !do i = 1, size(this%r)
      !  call this % r(i) % printngb()
      !enddo

      write(*,'(a)', advance ='no') 'route = { '
      do i = 1, size(this%r)
        write(*,'(a)', advance='no') this % r(i) % id//' '
      enddo
      write(*,'(a)') '}'
    end subroutine route_print



    function putlast(old, added) result(new)
      type(route_t), intent(in) :: old
      type(route_t) :: new
      type(node_t), intent(in) :: added

      integer :: n, i
      if (allocated(old % r)) then
        n = size(old % r)
      else
        n = 0
      endif
!print *, 'n= ', n
      allocate(new % r(n+1))

      do i=1, n
        new % r(i) = old % r(i)
      enddo
      new % r(n+1) = added
    end function



    subroutine list_putlast(list, added)
      type(list_t), intent(inout) :: list
      type(route_t), allocatable :: tmp(:)
      type(route_t), intent(in) :: added
 !
 ! Add entry at the end of the list. Reallocate if necessary
 !
      integer :: i, nmax
      if (allocated(list % m)) then
        nmax = size(list % m)
      else
        nmax = 0
      endif

      if (list % n == nmax) then
        nmax = max(1, 2*nmax)
        allocate(tmp(nmax))

        do i=1, list % n
          tmp(i) = list % m(i)
        enddo
        call move_alloc(tmp, list % m)
      endif
      list % n = list % n + 1
      list % m(list % n) = added
    end subroutine list_putlast



    integer function route_test(this) result(res)
      class(route_t), intent(in) :: this

      integer :: n, i
      character(len=2) :: pattern

      ! empty route is not complete
      res = ROUTE_INCOMPLETE
      n = 0
      if (allocated(this % r)) n = size(this % r)
      if (n == 0) return

      ! valid route must begin at the start, start cannot appear later
      if (.not. this % r(1) % is_beg()) then
        res = ROUTE_FAIL
        return
      endif
      if (n > 1 .and. this % r(n) % is_beg()) then
        res = ROUTE_FAIL
        return
      endif

      ! route is complete if last node is the end
      if (this % r(n) % is_end()) then
        res = ROUTE_DONE
        return
      endif

      ! if last node is a small cave, verify that it is not already present
      if (.not. this % r(n) % is_small()) return
      pattern = this % r(n) % id
      do i = n-1, 2, -1
        if (this % r(i) % id == pattern) then
          res = ROUTE_FAIL
          exit
        endif
      enddo
    end function route_test

  end module route
