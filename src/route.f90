  module route
    use node
    implicit none

    integer, parameter :: ROUTE_DONE=1, ROUTE_FAIL=-1, ROUTE_INCOMPLETE=0, &
                          ROUTE_SMALL=2

    type route_t
      type(node_t), allocatable :: r(:)
      integer :: n = 0
      logical :: visited_small = .false.
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
      new % n = 0
      new % visited_small = .false.
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
      do i = 1, b % n
        a % r(i) = b % r(i)
      enddo
      a % n = b % n
      a % visited_small = b % visited_small
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

      !do i = 1, this % n
      !  call this % r(i) % printngb()
      !enddo

      write(*,'(a)', advance ='no') 'route = { '
      do i = 1, this % n
        write(*,'(a)', advance='no') this % r(i) % id//' '
      enddo
      write(*,'(a)') '}'
    end subroutine route_print



    subroutine putlast(route, added)
      type(route_t), intent(inout) :: route
      type(node_t), intent(in) :: added

      type(node_t), allocatable :: tmp(:)
      integer :: nmax, i
      if (allocated(route % r)) then
        nmax = size(route % r)
      else
        nmax = 0
      endif

      if (route % n == nmax) then
         nmax = max(1, 2*nmax)
         allocate(tmp(nmax))
        do i=1, route % n
          tmp(i) = route % r(i)
        enddo
        call move_alloc(tmp, route % r)
      endif

      route % n = route % n + 1 
      route % r(route % n) = added
    end subroutine putlast



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
      if (allocated(this % r)) n = this % n
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

          ! we can visit once small value
          if (.not. this % visited_small) then
            res = ROUTE_SMALL
          else
            res = ROUTE_FAIL
          endif
          exit
        endif
      enddo
    end function route_test

  end module route
