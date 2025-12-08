!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Saves to disk a dummy pressure field. 
!> This tests the ability to write to a file at custom base frequencies. It's main purpose is to check the previous xios behaviour is not broken when adding supporting for not constant frequency intervals.

module timer_mod
  use, intrinsic :: iso_fortran_env, only : dp=> real64
  use mpi
  implicit none

  type :: timer 
    double precision, private :: start_timestamp = 0 
    real(kind=dp),private :: elapsed_time = 0
    integer, private :: comm = -1

    
    contains
    procedure, public :: start
    procedure, public :: stop
    procedure, public :: elapsed
  end type

  interface timer
    module procedure :: constructor
  end interface

  contains

  function constructor(comm) result(new_timer)
    integer,intent(in) :: comm
    type(timer) :: new_timer

    new_timer%comm=comm
  end function


  subroutine start(this)
    class(timer) :: this

    this%start_timestamp = mpi_wtime()
    
  end subroutine

  function elapsed(this) result(elapsed_time_max)
    class(timer) :: this
    double precision :: elapsed_time_max
    integer :: ierr
    
    call MPI_Allreduce(this%elapsed_time,elapsed_time_max,1,MPI_DOUBLE,MPI_MAX,this%comm,ierr)


  end function


  subroutine stop(this)
    class(timer) :: this

    this%elapsed_time = this%elapsed_time + mpi_wtime() - this%start_timestamp
    
  end subroutine


end module timer_mod

module partition_mod
    use mpi
    implicit none
  
    type :: partition_info 
      integer ::  n_global 
      integer :: n_local 
      integer :: ibegin_global
      integer :: comm
    end type
  
    contains


    subroutine create_partition( n_global , comm ,info )
      type(partition_info), intent(out) :: info 
      integer,intent(in) :: n_global
      integer,intent(in) :: comm
  
      integer :: nRanks 
      integer :: rank
      integer :: ierr
      integer :: remainder
      
      call MPI_Comm_rank(comm , rank, ierr)
      call MPI_Comm_size(comm, nRanks,ierr)
      
      info%comm=comm
      info%n_global=n_global
      info%n_local=n_global/nRanks

      remainder = mod(n_global, nRanks)
      info%ibegin_global=rank * info%n_local

      if (rank < remainder) then 
        info%n_local=info%n_local + 1
        info%ibegin_global=info%ibegin_global + rank
      else 
        info%ibegin_global=info%ibegin_global + remainder
      endif 

    end subroutine
  
    end module
  
  module file_info_mod
    implicit none
    
    integer, parameter :: max_characters = 500

    type file_info

      character(len=max_characters) :: field_base_name, file_name
      integer :: nfields
      integer :: nelements
      character(len=max_characters) :: operation
      character(len=max_characters) :: par_access
      logical :: check
      integer :: log
    contains
        procedure,public :: get_field_name
        procedure,public :: init
        procedure, public :: get_nfields
        procedure, public :: get_file_name
    end type  

    private :: get_field_name
    private :: init
    private :: get_file_name

    contains


    subroutine init(this, file_name_list)
      character(len=*),intent(in) :: file_name_list
      class(file_info),intent(inout) :: this
      integer :: nfields
      integer :: nelements
      integer :: io
      logical :: check
      integer :: log
      character(len=max_characters) :: file_name,field_base_name,operation,par_access
      namelist /info/nelements,nfields,field_base_name,file_name,operation,check,log,par_access
      
      open(newunit=io, file=file_name_list,action="read")
      read(unit=io,nml=info)

      close(io)

      this%field_base_name=field_base_name
      this%file_name=file_name
      this%nfields = nfields
      this%nelements = nelements
      this%operation = operation
      this%check = check
      this%log = log 
      this%par_access = par_access
    end subroutine

    function get_nfields(this) result(nfields)
      class(file_info),intent(in) :: this
      integer :: nfields

      nfields=this%nfields
    end function

    function get_file_name(this) result(file_name)
      class(file_info),intent(in) :: this
      character(len=max_characters) :: file_name
      file_name = this%file_name

    end function

    
    function get_field_name(this,i) result(field_name )
      class(file_info) :: this
      integer, intent(in) :: i
      character(len= max_characters) :: field_name

      write(field_name,"(A8,I5.5)") this%field_base_name, i
      
    end function get_field_name

    
  
  end module file_info_mod

  program resample
    use xios
    use IXML_TREE
    use mpi
    use partition_mod
    use file_info_mod, only : file_info,max_characters
    implicit none
    
  
    integer :: ierr = 0
    integer :: wRank
    integer :: wNranks  
    type(partition_info) :: pinfo 
    type(file_info) :: finfo 

  
    call MPI_INIT(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, wRank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, wNranks,ierr)
    
    call initialise(pinfo,finfo)
    call simulate(pinfo,finfo)
    call finalise()
    
    call MPI_Finalize(ierr)
  
  contains
  
    subroutine initialise(pinfo,finfo)
  
      type(xios_date) :: origin
      type(xios_date) :: start
      type(xios_duration) :: tstep
      TYPE(xios_file) :: file_hdl
      TYPE(xios_field) :: field_hdl
      TYPE(xios_axis) :: axis_hdl
      type(partition_info),intent(out) :: pinfo 
      type(file_info),intent(out) :: finfo
      type(xios_filegroup) :: fgroup
  
      integer :: comm = -1
      integer :: rank = -1
      integer :: npar = 0
      integer :: mpi_error
      integer :: nfields = 1
      character(len = max_characters) :: field_name
      integer :: i
      
      ! Arbitrary datetime setup, required for XIOS but unused
      ! in this example
      origin = xios_date(2022, 2, 2, 12, 0, 0)
      start = xios_date(2022, 12, 13, 12, 0, 0)
      tstep = xios_hour
  
      ! Initialise MPI and XIOS
      call xios_initialize('client', return_comm=comm)
      
  
      call MPI_Comm_rank(comm, rank, mpi_error)
      call MPI_Comm_size(comm, npar, mpi_error)
      
      call finfo%init("config.nml")
      
      call xios_context_initialize('main', comm)
      call xios_set_time_origin(origin)
      call xios_set_start_date(start)
      call xios_set_timestep(tstep)
      
      CALL xios_get_file_handle("axis_output",file_hdl)

      CALL xios_set_file_attr("axis_output",name=finfo%get_file_name() )
      CALL xios_set_file_attr("axis_output",type="one_file")
      CALL xios_set_file_attr("axis_output",output_freq=tstep)
      call xios_set_file_attr("axis_output",par_access=finfo%par_access)
      if (finfo%operation == "read") then 
        call xios_set_file_attr("axis_output",mode="read")
        
        
      endif
      
      call create_partition(finfo%nelements,comm,pinfo) ! distribute levels in the field across clients and save distribution info in pinfo
      call xios_set_axis_attr("pressure_levels1",n_glo=pinfo%n_global,begin=pinfo%ibegin_global,n=pinfo%n_local)
      call xios_set_file_attr("axis_output",output_freq=tstep)


      
      ! Add fields
      do i=1, finfo%get_nfields()
        field_name=finfo%get_field_name(i)
        call xios_add_fieldtofile(file_hdl,field_hdl,field_name)
        call xios_set_field_attr(field_name,grid_ref="model")
        call xios_set_field_attr(field_name,name=field_name)
        call xios_set_field_attr(field_name,operation="instant")
        call xios_set_field_attr(field_name,freq_op=tstep)    
        if (finfo%operation == "read") then  
          call xios_set_field_attr(field_name,read_access=.true.)
        endif 
        
      enddo


      call xios_close_context_definition()
  
      
    end subroutine initialise
  
    subroutine finalise()
  
      integer :: mpi_error
  
      ! Finalise XIOS and MPI
      call xios_context_finalize()
  
      call xios_finalize()
  
    end subroutine finalise
  
    subroutine simulate(pinfo,finfo)
      use, intrinsic :: iso_fortran_env, only : dp=> real64
      use timer_mod , only : timer 

      type(xios_date) :: current
      TYPE(xios_file) :: file_hdl
      type(partition_info),intent(in) :: pinfo
      type(file_info),intent(in) :: finfo 
      type(timer) :: io_timer
      integer :: ts
      logical :: output_start_is_defined = .false.
      logical :: output_stop_is_defined = .false.
      integer :: ilevel
      integer :: ifield
      double precision :: elapsed
      integer :: rank
      
      double precision, dimension (:), allocatable :: inpdata
      double precision , parameter:: max_integer = 1000000

      io_timer = timer(pinfo%comm)
      call MPI_Comm_rank(pinfo%comm,rank,ierr)

      allocate ( inpdata(pinfo%n_local) )
      ts=1
      if (finfo%operation=="write") then 
        call xios_update_calendar(ts)
      endif
      !CALL xios_get_handle("axis_output",file_hdl)
      
      
      do ifield=1,finfo%get_nfields()
        if (finfo%log >=1) write(*,*) "Generating a field"

        
        if (finfo%operation == "write") then 
          if (finfo%log >=1) write(*,*) "Sending a field"
          !write(*,*) rank,": ",pinfo%ibegin_global

          do ilevel=1,pinfo%n_local
            inpdata(ilevel)= mod( ilevel *1d0 + pinfo%ibegin_global + finfo%nelements*(ifield-1) ,max_integer )
          end do
          
          call io_timer%start()
          call xios_send_field(finfo%get_field_name(ifield), inpdata)
          call io_timer%stop()
          
        else if (finfo%operation == "read") then
          
          if (finfo%log >=1) write(*,*) "Receiving a field"
          call io_timer%start()
          call xios_recv_field(finfo%get_field_name(ifield), inpdata)
          call io_timer%stop()
          
          if (finfo%check .eqv. .true. ) then 
            if (finfo%log >=1) write(*,*) "Checking read data"
            do ilevel=1,pinfo%n_local
              !inpdata(ilevel)=
              if ( abs( inpdata(ilevel) - mod(ilevel*1d0 + pinfo%ibegin_global + & 
                finfo%nelements*(ifield-1) , max_integer  ) )> 1e-5 ) then 
                write(*,*) "Error at level" , ilevel,". Expected ", &
                mod(ilevel*1d0 + pinfo%ibegin_global + finfo%nelements*(ifield-1),max_integer)  , ", got " &
                ,inpdata(ilevel), "at rank " , rank
                !call mpi_abort(MPI_COMM_WORLD,-1,ierr)
              endif        
            enddo
          endif 
        endif
      enddo


      deallocate (inpdata)
      
      elapsed = io_timer%elapsed()
      if (rank .eq. 0) then 
         write(*,*) "Time elapsed: ", elapsed
      endif
    end subroutine simulate

  end program resample