  module route
    use node
    implicit none
    private
    public route_move, list_move, list_putlast, putlast

    integer, parameter, public  :: ROUTE_DONE=1, ROUTE_FAIL=-1, &
    &   ROUTE_INCOMPLETE=0, ROUTE_SMALL_REVISITED=2

    type, public :: route_t
      private
      type(node_t), allocatable :: r(:)
      integer :: n = 0
      logical :: revisited_small_cave = .false.
    contains
      procedure :: size => route_size
      procedure :: getnode => route_getnode
      procedure :: setvisited => route_setvisited
      procedure :: test => route_test
      procedure :: print => route_print
      procedure, private :: route_copy
      generic :: assignment(=) => route_copy
    end type

    type, public :: list_t
      private
      type(route_t), allocatable :: m(:)
      integer :: n = 0
    contains
      procedure :: size => list_size
      procedure :: getroute => list_getroute
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
      new % revisited_small_cave = .false.
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
      a % revisited_small_cave = b % revisited_small_cave
    end subroutine route_copy

    subroutine route_move(a, b)
      class(route_t), intent(out) :: a
      type(route_t), intent(inout) :: b
      integer :: i

      call move_alloc(b % r, a % r)
      a % n = b % n
      a % revisited_small_cave = b % revisited_small_cave
    end subroutine route_move



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

    subroutine list_move(a,b)
      class(list_t), intent(out) :: a
      type(list_t), intent(inout) :: b
      integer :: i

      call move_alloc(b % m, a % m)
      a % n = b % n
    end subroutine list_move



    subroutine route_print(this)
      class(route_t), intent(in) :: this
      integer :: i

      write(*,'(a)', advance ='no') 'route = { '
      do i = 1, this % n
        write(*,'(a)', advance='no') this % r(i) % getid()//' '
      enddo
      write(*,'(a)') '}'
    end subroutine route_print



    subroutine putlast(route, added_node)
      type(route_t), intent(inout) :: route
      type(node_t), intent(in) :: added_node
 !
 ! Add node at the end of the route. Reallocate if necessary
 !
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
      route % r(route % n) = added_node
    end subroutine putlast



    subroutine list_putlast(list, added_route)
      type(list_t), intent(inout) :: list
      type(route_t), allocatable :: tmp(:)
      type(route_t), intent(in) :: added_route
 !
 ! Add route at the end of the list. Reallocate if necessary
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
      list % m(list % n) = added_route
    end subroutine list_putlast



    integer function route_size(this)
      class(route_t), intent(in) :: this
      route_size = this % n
    end function

    integer function list_size(this)
      class(list_t), intent(in) :: this
      list_size = this % n
    end function



    function route_getnode(this, i)
       class(route_t), intent(in) :: this
       integer, intent(in) :: i
       type(node_t) :: route_getnode
       if (i > this % n) &
           error stop 'route_getnode: index out of bounds'
       route_getnode = this % r(i)
    end function

    function list_getroute(this, i)
       class(list_t), intent(in) :: this
       integer, intent(in) :: i
       type(route_t) :: list_getroute
       if (i > this % n) &
           error stop 'list_getroute: index out of bounds'
       list_getroute = this % m(i)
    end function



    subroutine route_setvisited(this, set)
      class(route_t), intent(inout) :: this
      logical, intent(in) :: set
      this % revisited_small_cave = set
    end subroutine



    integer function route_test(this, revisit_allowed) result(res)
      class(route_t), intent(in) :: this
      logical, intent(in), optional :: revisit_allowed
      ! allow one revisit of small caves (default=true)

!
! Test route for FAIL / DONE / INCOMPLETE / SMALL_REVISITED 
!
      integer :: n, i
      character(len=2) :: pattern
      logical :: revisit_allowed0

      revisit_allowed0 = .true.
      if (present(revisit_allowed)) revisit_allowed0 = revisit_allowed

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
      pattern = this % r(n) % getid()
      do i = n-1, 2, -1
        if (this % r(i) % getid() /= pattern) cycle
        res = ROUTE_FAIL

        ! one small cave can be revisited once
        if (revisit_allowed0 .and. .not. this % revisited_small_cave) &
            res = ROUTE_SMALL_REVISITED

        exit
      enddo
    end function route_test

  end module route
