module code12
  use node !, only : node_t, DAT_KIND, rbtr_t, node_ptr
  use route
  implicit none
  private
  integer(DAT_KIND), allocatable :: mold(:)

  public :: say_hello
contains
  subroutine say_hello
    type(rbtr_t) :: all_nodes
    type(node_ptr) :: new_node, ptr2
    procedure(compare_fun) :: cfun
    type(list_t) :: list, finished
 type(node_ptr) :: ptr
 type(node_t) :: a, b, c
 integer :: ierr
 integer(DAT_KIND), allocatable :: h(:)
 type(route_t) :: route


    all_nodes = rbtr_t(cfun)

    call read_input(all_nodes,'input.txt')
    !call read_input(all_nodes,'test.txt')
    !call read_input(all_nodes,'test2.txt')
    !call read_input(all_nodes,'test3.txt')

    call nodes_print(all_nodes)
    print *
    call print_cave(all_nodes)

 print *, 'EEEEEE'

    ! init list
    list = list_t()
    list = init_list(all_nodes)

    ! search for paths
    finished = list_t()
    do 
      if (list % n == 0) exit
      call update_routes(list, finished)
    enddo

  end subroutine say_hello



  subroutine read_input(all_nodes, file)
    type(rbtr_t) :: all_nodes
    character(len=*), intent(in) :: file

    integer :: fid, i, j
    character(len=200) :: line
    character(len=:), allocatable :: token1, token2
    type(node_ptr) :: ptr1, ptr2, new
    integer(DAT_KIND), allocatable :: h1(:), h2(:)

    open(newunit=fid, file=file, status='old')
    do
      read(fid,'(a)', end=100) line

      i = scan(line, '-')
      token1 = line(1:i-1)
      token2 = trim(line(i+1:))
      print *, '['//token1//'] ['//token2//']'

      if (token1=='start') token1 = NODE_BEG
      if (token2=='start') token2 = NODE_BEG
      if (token1=='end') token1 = NODE_END
      if (token2=='end') token2 = NODE_END
      if (len(token1)==1) token1 = token1//' '
      if (len(token2)==1) token2 = token2//' '

      ! temporary space to communicate with TREE
      ptr1 % p => node_t(token1)
      ptr2 % p => node_t(token2)

      ! If nodes do not exist, allocate space and insert them
      if (.not. all_nodes % isin(transfer(ptr1,mold))) then
        new % p => node_t(token1)
        call all_nodes % add(transfer(new,mold))
      endif
      if (.not. all_nodes % isin(transfer(ptr2,mold))) then
        new % p => node_t(token2)
        call all_nodes % add(transfer(new,mold))
      endif

      ! search pointers to actual positions of current nodes
      call all_nodes % find(transfer(ptr1,mold), h1, i)
      call all_nodes % find(transfer(ptr2,mold), h2, j)
      if (i /=0 .or. j /= 0) &
          error stop 'nodes not found, this is defenive '

      ! reuse temporary space
      deallocate(ptr1 % p, ptr2 % p)
      ptr1 = transfer(all_nodes % read(h1), ptr1)
      ptr2 = transfer(all_nodes % read(h2), ptr2)

      ! add current nodes to each other's list of neighbours
      call ptr1 % p % addngb(ptr2)
      call ptr2 % p % addngb(ptr1)
      ptr1 % p => null()
      ptr2 % p => null()

      cycle
      100 exit
    enddo
  end subroutine read_input


  subroutine print_cave(all_nodes)
    type(rbtr_t), intent(in) :: all_nodes
    integer(DAT_KIND), allocatable :: handle(:)
    integer :: ierr
    type(node_ptr) :: ptr

    call all_nodes % resetcurrent(handle)
    do
      ptr = transfer(all_nodes % nextread(handle, ierr), ptr)
      if (ierr /= 0) exit
      call ptr % p % printngb()
    enddo
  end subroutine print_cave



  function init_list(all_nodes) result(list)
    type(list_t) :: list
    type(rbtr_t), intent(in) :: all_nodes

    type(node_ptr) :: ptr
    type(node_t) :: start
    integer :: i
    integer(DAT_KIND), allocatable :: h(:)
    type(route_t) :: route

    ptr % p => node_t(NODE_BEG)
    call all_nodes % find(transfer(ptr,mold), h, i)
    if (i /=0) &
      error stop 'There is no start'
    deallocate(ptr % p)
    ptr = transfer(all_nodes % read(h), ptr)
    start = ptr % p
    route = route_t()
    call putlast(route, start)
    list = list_t()
    call list_putlast(list, route)
  end function init_list



  subroutine update_routes(list, finished)
    type(list_t), intent(inout) :: list, finished

    type(list_t) :: updated_list
    type(route_t) :: new_route
    integer :: i, n, ierr, itest
    type(node_t) :: last_node
    type(node_t) :: ngb_node
    integer(DAT_KIND), allocatable :: h(:)

    !
    ! For all routes select the last node and iterate over its
    ! neighbours.
    !
    updated_list = list_t()
    do i = 1, list % n
if(mod(i,100)==0) write(*,'(a)', advance='no') '.'
      n = list % m(i) % n
      last_node = list % m(i) % r(n)
      call last_node % resetcurrent(h)
      do
        ngb_node = last_node % nextngb(h, ierr)
        if (ierr /= 0) exit

        !
        ! Add the neighbor to the end of the route and test it 
        !
        new_route = list % m(i)
        call putlast(new_route, ngb_node)
        itest = new_route % test()
! write(*,'(a,i2,l1,a)',advance='no') 'ires ',itest,new_route%visited_small,' for new route '
! call new_route % print()

        ! Invalid route is discarded
        ! Incompleter route will be processed in the next call
        ! Completed route is added as finished
        select case(itest)
        case(ROUTE_FAIL)
          continue
        case(ROUTE_INCOMPLETE)
          call list_putlast(updated_list, new_route)
        case(ROUTE_DONE)
          call list_putlast(finished, new_route)
        case(ROUTE_SMALL)
          new_route % visited_small = .true.
          call list_putlast(updated_list, new_route)
        case default
          error stop 'wrong value of test'
        end select
      enddo
    enddo
    write(*,*)

    print *, 'Finished routes ', finished%n
!   do i=1, finished % n
!     call finished % m(i) % print()
!   enddo
!   write(*,*)
    print *, 'Unfinished  routes ', updated_list%n
!   do i=1,updated_list % n
!     call updated_list % m(i) % print()
!   enddo
    write(*,*)

    list = updated_list

  end subroutine


end module code12
