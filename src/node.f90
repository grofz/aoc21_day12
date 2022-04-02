  module node
    use tree_m
    implicit none
    public nodes_print

    type node_t
  !   private
      character(len=2) :: id
      type(rbtr_t)     :: ngb
    contains
      procedure :: addngb => node_addngb
      procedure :: printngb => node_printngb
      procedure :: is_end, is_beg, is_small
      procedure :: resetcurrent => node_resetcurrent
      procedure :: nextngb => node_nextngb
      procedure, private :: node_copy
      generic :: assignment(=) => node_copy
    end type

    interface node_t
      module procedure node_init
    end interface


    type node_ptr
      type(node_t), pointer :: p
    end type

    integer(DAT_KIND), allocatable :: mold(:)
    private mold
    character(len=2), parameter :: NODE_BEG='[[', NODE_END=']]'

  contains

    function node_init(id) result(new)
      type(node_t), pointer :: new
      character(len=2), intent(in) :: id

      procedure(compare_fun) :: cfun
      allocate(new)
      new % id = id
      new % ngb = rbtr_t(cfun)
    end function node_init



    subroutine node_copy(aout, bin)
      class(node_t), intent(out) :: aout
      type(node_t), intent(in) :: bin
      aout % ngb = bin % ngb
      aout % id  = bin % id
    end subroutine 



    logical function is_end(this)
      class(node_t), intent(in) :: this
      is_end = this % id == NODE_END
    end function



    logical function is_beg(this)
      class(node_t), intent(in) :: this
      is_beg = this % id == NODE_BEG
    end function



    logical function is_small(this)
      class(node_t), intent(in) :: this
      is_small = this % id >= 'aa'
    end function



    subroutine node_addngb(this, ngb)
      class(node_t), intent(inout) :: this
      type(node_ptr), intent(in) :: ngb

      if (this % ngb % isin(transfer(ngb,mold))) then
print *, 'ngb ', ngb%p%id, 'exists in list of ',this%id
      else
        call this % ngb % add(transfer(ngb,mold))
      endif
    end subroutine node_addngb



    subroutine node_printngb(this)
      class(node_t), intent(in) :: this

      write(*,'(a,3(l1),a)',advance='no') this%id//"'s ngb are ", &
          this % is_beg(), this % is_small(), this % is_end(), '  '
      call nodes_print(this%ngb)
    end subroutine node_printngb



    subroutine nodes_print(tree)
      type(rbtr_t), intent(in) :: tree

      integer(DAT_KIND), allocatable :: handle(:)
      type(node_ptr) :: ptr
      integer :: ierr

      call tree % resetcurrent(handle)
      write(*,'(a)',advance='no') '{ '
      do
        ptr = transfer(tree % nextread(handle, ierr), ptr)
        if (ierr /= 0) exit
        write(*,'(a)',advance='no') ptr%p%id//' '
      enddo
      write(*,'(a)') '}'
    end subroutine nodes_print



    subroutine node_resetcurrent(this, handle)
      class(node_t), intent(in) :: this
      integer(DAT_KIND), allocatable, intent(out) :: handle(:)
      call this % ngb % resetcurrent(handle)
    end subroutine



    function node_nextngb(this, handle, ierr) result(node)
      type(node_t) :: node
      class(node_t), intent(in) :: this
      integer(DAT_KIND), intent(inout) :: handle(:)
      integer, intent(out), optional :: ierr

      integer :: ierr0
      type(node_t) :: empty_node !=> node_t('??')
      type(node_ptr) :: ptr

      ptr = transfer(this % ngb % nextread(handle, ierr0), ptr)
      if (ierr0 /= 0) then
        node = empty_node
      else
        node = ptr % p
      endif
      if (present(ierr)) then
        ierr = ierr0
      elseif (ierr0 /= 0) then
        error stop 'nextngb end of list'
      endif
    end function node_nextngb

  end module node



  pure function cfun(a, b) result (ires)
    use tree_m, only : DAT_KIND
    use node, only : node_t, node_ptr
    integer(DAT_KIND), intent(in) :: a(:), b(:)
    integer :: ires
    type(node_ptr) :: ptra, ptrb

    ptra = transfer(a, ptra)
    ptrb = transfer(b, ptrb)

    if (ptra % p % id < ptrb % p % id) then
      ires = -1
    elseif (ptra % p % id > ptrb % p % id) then
      ires = 1
    else
      ires = 0
    endif
  end function cfun
