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


  contains



  subroutine start(this)
    class(timer) :: this

    this%start_timestamp = mpi_wtime()
    
  end subroutine

  function elapsed(this) result(elapsed_time)
    class(timer),intent(in) :: this
    real(kind=dp) :: elapsed_time

    elapsed_time=this%elapsed_time


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
      integer , dimension(:) , allocatable::  n_global
      integer , dimension(:), allocatable :: n_local
      integer , dimension(:), allocatable :: ibegin_global
      integer :: comm

    end type
  
    contains


    subroutine create_partition( n_global ,n_proc, comm ,info )
      type(partition_info), intent(out) :: info 
      integer,intent(in) :: n_global(:)
      integer,intent(in) :: comm
      integer,intent(in) :: n_proc(:)
      
      integer :: ndims
      integer :: nRanks 
      integer :: rank
      integer :: ierr,d
      integer :: remainder
      integer :: dims
      integer :: nProcs
      logical, dimension(:), allocatable :: periodic
      integer,dimension(:), allocatable :: rank_coords

      
      
      ndims=size(n_proc)
      
      allocate( periodic(1:ndims) )
      allocate( rank_coords(1:ndims) )

      periodic=.false.
      
      call MPI_Comm_size(comm, nRanks,ierr)
      write (*,*) "N. ranks: " , product(n_proc) , ", " , nRanks
      if ( product(n_proc) /= nRanks ) then 
        write (*,*) "Error: Expected " , nRanks , "processors. Got ", product(n_proc) , "instead" 
        call mpi_abort(comm,-1,ierr)
      endif
      

      call MPI_Cart_create ( comm, ndims, n_proc , periodic, .false., info%comm,ierr)
      
      call MPI_Comm_rank(info%comm , rank, ierr)
      call MPI_Comm_size(info%comm, nRanks,ierr)
      call MPI_Cart_coords (info%comm, rank, ndims, rank_coords,ierr)
      
      info%n_global=n_global
      allocate( info%n_local(1:ndims) )
      allocate( info%ibegin_global(1:ndims) )

      do d=1,ndims
        info%n_local(d)=n_global(d)/n_proc(d)

        remainder = mod(n_global(d), n_proc(d) )
        info%ibegin_global(d)=rank_coords(d) * info%n_local(d)

        if ( rank_coords(d) < remainder ) then 
          info%n_local(d)= info%n_local(d) + 1
          info%ibegin_global(d)= info%ibegin_global(d) + rank_coords(d)
        else
          info%ibegin_global(d)=info%ibegin_global(d) + remainder
        endif 
      end do

    end subroutine
  
    end module
  
  module file_info_mod
    implicit none
    
    integer, parameter :: max_characters = 500

    type file_info

      character(len=max_characters) :: field_base_name, file_name
      integer :: nfields
      integer, allocatable :: shape(:)
      integer, allocatable :: n_proc(:)
      integer :: nfiles
      
      character(len=max_characters) :: operation
      character(len=max_characters) :: par_access
      logical :: check
      integer :: log
      integer :: warmup_operations

    contains
        procedure,public :: get_field_name
        procedure,public :: init
        procedure, public :: get_nfields
        procedure, public :: get_file_name
        procedure, public :: get_file_id
    end type  

    private :: get_field_name
    private :: init
    private :: get_file_name
    private :: get_file_id

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
      integer :: n_proc(4)
      integer :: shape(4)
      integer :: nfiles
      integer :: warmup_operations=0

      namelist /info/nelements,nfields,field_base_name,file_name,operation,check,log,par_access,n_proc,shape,nfiles,warmup_operations
      
      open(newunit=io, file=file_name_list,action="read")
      read(unit=io,nml=info)

      close(io)

      this%field_base_name=field_base_name
      this%file_name=file_name
      this%nfields = nfields
      this%operation = operation
      this%check = check
      this%log = log 
      this%par_access = par_access
      this%n_proc=n_proc
      this%shape=shape
      this%nfiles = nfiles
      this%warmup_operations = warmup_operations
    end subroutine

    function get_nfields(this) result(nfields)
      class(file_info),intent(in) :: this
      integer :: nfields

      nfields=this%nfields
    end function

    function get_file_name(this, i) result(file_name)
      class(file_info),intent(in) :: this
      integer, intent(in) :: i
      character(len=max_characters+5) :: file_name
      character(len=5) :: i_str

      write(i_str,'(I5.5)') i 
      file_name = trim(this%file_name) // i_str

    end function

    function get_file_id(this, i) result(file_id)
      class(file_info),intent(in) :: this
      integer, intent(in) :: i
      character(len=max_characters) :: file_id
      character(len=5) :: i_str
      write(i_str,'(I5.5)') i 
      file_id = "axis_output" // i_str


    end function

    
    function get_field_name(this,iFile,iField) result(field_name )
      class(file_info) :: this
      integer, intent(in) :: iFile
      integer, intent(in) :: iField
      character(len= max_characters) :: field_name

      write(field_name,"(A8,I5.5,I5.5)") this%field_base_name, iFile, iField
      
    end function get_field_name

    
  
  end module file_info_mod

  program resample
    use xios
    use IXML_TREE
    use mpi
    use partition_mod
    use file_info_mod, only : file_info,max_characters
    use timer_mod, only : timer
    implicit none
    
  
    integer :: ierr = 0
    integer :: wRank
    integer :: wNranks  
    type(partition_info) :: pinfo 
    type(file_info) :: finfo 
    type(timer) :: timer_initalise
    type(timer) :: timer_finalise
  
    call MPI_INIT(ierr)
    
    call MPI_Comm_rank(MPI_COMM_WORLD, wRank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, wNranks,ierr)

    call timer_initalise%start()
    call initialise(pinfo,finfo)
    call timer_initalise%stop()

    call simulate(pinfo,finfo)
    call timer_finalise%start()
    call finalise()
    call timer_finalise%stop()

    if (wrank .eq. 0) then 
      write(*,*) "Initialise: ", timer_initalise%elapsed()
      write(*,*) "Finalise: ", timer_finalise%elapsed()
      
    endif
      

    
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
      integer :: ifile
  
      integer :: comm = -1
      integer :: rank = -1
      integer :: npar = 0
      integer :: mpi_error
      integer :: nfields = 1
      character(len = max_characters) :: field_name,axis_name
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

      
      call create_partition(finfo%shape,finfo%n_proc,comm,pinfo) ! distribute levels in the field across clients and save distribution info in pinfo
      axis_name="levels"
      call xios_set_axis_attr(axis_name,n_glo=pinfo%n_global(1),begin=pinfo%ibegin_global(1),n=pinfo%n_local(1))
      call xios_set_axis_attr("panel",n_glo=pinfo%n_global(4),begin=pinfo%ibegin_global(4),n=pinfo%n_local(4) )

      call xios_set_domain_attr("horizontal_domain", &
                                ni_glo=pinfo%n_global(2),ibegin=pinfo%ibegin_global(2),ni=pinfo%n_local(2), &
                                nj_glo=pinfo%n_global(3),jbegin=pinfo%ibegin_global(3),nj=pinfo%n_local(3) &
                                )
      
      CALL xios_get_filegroup_handle("output_files",fgroup)

      do ifile=1,finfo%nfiles
        call xios_add_file(fgroup, file_hdl, finfo%get_file_id(iFile))


        CALL xios_set_file_attr(finfo%get_file_id(ifile),name=finfo%get_file_name(ifile) )
        CALL xios_set_file_attr(finfo%get_file_id(ifile),type="one_file")
        CALL xios_set_file_attr(finfo%get_file_id(ifile),output_freq=tstep)
        call xios_set_file_attr(finfo%get_file_id(ifile),par_access=finfo%par_access)
        if (finfo%operation == "read") then 
          call xios_set_file_attr(finfo%get_file_id(ifile),mode="read")
        
        
        endif

        call xios_set_file_attr(finfo%get_file_id(ifile),output_freq=tstep)
      
      
      


      
        ! Add fields
        do i=1, finfo%get_nfields()
          field_name=finfo%get_field_name(ifile,i)
          call xios_add_fieldtofile(file_hdl,field_hdl,field_name)
          call xios_set_field_attr(field_name,grid_ref="model")
          call xios_set_field_attr(field_name,name=field_name)
          call xios_set_field_attr(field_name,operation="instant")
          call xios_set_field_attr(field_name,freq_op=tstep)    
          if (finfo%operation == "read") then  
            call xios_set_field_attr(field_name,read_access=.true.)
          endif 
          
        enddo

      enddo


      call xios_close_context_definition()
  
      
    end subroutine initialise
  
    subroutine finalise()
  
      integer :: mpi_error
  
      ! Finalise XIOS and MPI
      call xios_context_finalize()
  
      call xios_finalize()
  
    end subroutine finalise
    
    subroutine set_data( data, pinfo)
      type(partition_info) , intent(in) :: pinfo
      double precision, dimension(:,:,:,:), intent(inout) :: data

      integer :: i,j,k,t

      do t=1,pinfo%n_local(4)
        do j=1,pinfo%n_local(3)
          do i=1,pinfo%n_local(2)
            do k=1,pinfo%n_local(1)
              
              data(k,i,j,t)= ( pinfo%ibegin_global(1) + k + (pinfo%ibegin_global(2) + i - 1 )*pinfo%n_global(1) + &
             (pinfo%ibegin_global(3) + j - 1)*pinfo%n_global(1)*pinfo%n_global(2) + &
            (pinfo%ibegin_global(4) + t - 1)*pinfo%n_global(1)*pinfo%n_global(2)*pinfo%n_global(3) )* &
                          1d0 / (pinfo%n_global(1)*pinfo%n_global(2) *pinfo%n_global(3)*pinfo%n_global(4) )
            end do 
          end do
        end do 
      end do

    end subroutine

    subroutine check_data( data, pinfo)
      type(partition_info) , intent(in) :: pinfo
      double precision, dimension(:,:,:,:), intent(inout) :: data

      integer :: i,j,k,t
      double precision :: err

      do t=1,pinfo%n_local(4)
        do j=1,pinfo%n_local(3)
          do i=1,pinfo%n_local(2)
            do k=1,pinfo%n_local(1)
              
              err= abs( data(k,i,j,t) - ( pinfo%ibegin_global(1) + k + (pinfo%ibegin_global(2) + i - 1 )*pinfo%n_global(1) + &
             (pinfo%ibegin_global(3) + j - 1)*pinfo%n_global(1)*pinfo%n_global(2) + &
            (pinfo%ibegin_global(4) + t - 1)*pinfo%n_global(1)*pinfo%n_global(2)*pinfo%n_global(3) )* &
                          1d0 / (pinfo%n_global(1)*pinfo%n_global(2) *pinfo%n_global(3)*pinfo%n_global(4) ) )
              
              if (err > 1e-5) then 
                write(*,*) "Error at " , i,j,k,". Error:  ", err
                call mpi_abort(MPI_COMM_WORLD,-1,ierr)
              endif
              
            end do 
          end do
        end do 
      end do

    end subroutine

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
      integer :: rank, ierr
      integer :: ifile
      logical :: is_first_data_point
      
      double precision, dimension (:,:,:,:), allocatable :: inpdata
      double precision , parameter:: max_integer = 1000000

      
      call MPI_Comm_rank(pinfo%comm,rank,ierr)
      
      allocate ( inpdata(pinfo%n_local(1) , pinfo%n_local(2),pinfo%n_local(3),pinfo%n_local(4)) )
      inpdata=0
      ts=1
      
      !call sleep(10)
      !call mpi_barrier(pinfo%comm,ierr )

      if (finfo%operation=="write") then 
        call xios_update_calendar(ts)
      endif
      !CALL xios_get_handle("axis_output",file_hdl)
      
      do ifile=1,finfo%nfiles

        do ifield=1,finfo%get_nfields()
          if (finfo%log >=1 .and. rank==0) write(*,*) "Generating a field"
          
          
          if (finfo%operation == "write") then 
            if (finfo%log >=1 .and. rank == 0) write(*,*) "Sending a field"
            !write(*,*) rank,": ",pinfo%ibegin_global
            
            call set_data( inpdata, pinfo)
            if ( ifield > finfo%warmup_operations ) call io_timer%start()
            call xios_send_field(finfo%get_field_name(ifile,ifield), inpdata)
            if ( ifield > finfo%warmup_operations ) call io_timer%stop()
            
          
            if ( is_first_data_point) is_first_data_point=.false.

          else if (finfo%operation == "read") then
            
            if (finfo%log >=1 .and. rank==0) write(*,*) "Receiving a field"
            if (.not. is_first_data_point) call io_timer%start()
            call io_timer%start()
            call xios_recv_field(finfo%get_field_name(ifile,ifield), inpdata)
            if (.not. is_first_data_point) call io_timer%stop()

            if ( is_first_data_point) is_first_data_point=.false.

            !call sleep(1)
            !call mpi_barrier(pinfo%comm,ierr )
            
            if (finfo%check .eqv. .true. ) then 
              if (finfo%log >=1) write(*,*) "Checking a field"  
              call check_data( inpdata, pinfo)
            endif
            
          endif

          

        enddo
      
      enddo

      deallocate (inpdata)
      
      elapsed = io_timer%elapsed()
      if (rank .eq. 0) then 
         write(*,*) "Time send/recv: ", elapsed
      endif
      
      !call sleep(10)
    end subroutine simulate

  end program resample
