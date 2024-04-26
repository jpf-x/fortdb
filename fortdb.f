module fortDB
!
! Fortran Database (binary) file definition allows easy reading and writing of datasets.
!
!  Initialize automatic file handler with
!
!    call init_fortdb
!
!
!  To write a database file that doesn't already exist:
!
!    type(database) :: database1
!    type(dataset) :: dset1
!    type(dataset) :: dset2
!    integer*4 :: i4(12)
!    real*8 :: r8(4)
!    i4=(/1,2,3,4,5,6,7,8,9,10,11,12/)
!    r8=(/8.0d0,7.0d0,6.0d0,7.0d0/)
!    call init_fortdb
!    call database1%initialize('file.bin')
!    call database1%add('dataset1',i4)
!    call database1%add('dataset2',r8)
!
! index     value/desc
!   1           1 integer*4
!               2 real*4
!               3 integer*8
!               4 real*8
!               5 character
!               
!   2           if index 1 is character, length of character
!   3           array dimension, 0 if scalar
!   4           if index 3 is non-zero, length of dimension 1
!   5           if index 3 is non-zero, length of dimension 2
!   .
!   .
!   .
!  10           if index 3 is non-zero, length of dimension 7
!       

    integer,parameter :: kind4=4
    integer,parameter :: WORD_SIZE=4
    integer,parameter :: DIMENSIONS=7   ! in order to change (increase) this, _1d, _2d, etc functions must be replicated as appropriate to beyond _7d
    integer,parameter :: DATASET_DESCRIPTION_LENGTH=3+DIMENSIONS
    integer,parameter :: DATASET_NAME_LENGTH=32
    integer,parameter :: MAX_PATH=256

    type,public :: dataset

        character(len=DATASET_NAME_LENGTH),public :: name
        integer,private :: description(DATASET_DESCRIPTION_LENGTH)
        integer,private :: word_size
        integer(kind=4),allocatable :: datas_i4(:)
        integer(kind=8),allocatable :: datas_i8(:)
        real(kind=4),allocatable :: datas_r4(:)
        real(kind=8),allocatable :: datas_r8(:)
        character(len=:),allocatable :: datas_c(:)

      contains

        procedure,public :: get_size => get_size_dataset
        procedure,private :: allocator => allocator_dataset
        procedure,public :: get_shape => get_shape_from_description

    end type dataset

    interface list_contains
        procedure :: list_contains_i4
        procedure :: list_contains_i8
        procedure :: list_contains_r4
        procedure :: list_contains_r8
        procedure :: list_contains_str
    end interface list_contains

    interface dataset_from_data
        procedure :: dataset_from_data_0d
        procedure :: dataset_from_data_1d
        procedure :: dataset_from_data_2d
        procedure :: dataset_from_data_3d
        procedure :: dataset_from_data_4d
        procedure :: dataset_from_data_5d
        procedure :: dataset_from_data_6d
        procedure :: dataset_from_data_7d
    end interface dataset_from_data

    interface get_dataset_description_from_data
        procedure :: get_dataset_description_from_data_0d
        procedure :: get_dataset_description_from_data_1d
        procedure :: get_dataset_description_from_data_2d
        procedure :: get_dataset_description_from_data_3d
        procedure :: get_dataset_description_from_data_4d
        procedure :: get_dataset_description_from_data_5d
        procedure :: get_dataset_description_from_data_6d
        procedure :: get_dataset_description_from_data_7d
    end interface get_dataset_description_from_data

    interface set_to_1d
        procedure :: set_to_1d_0d
        procedure :: set_to_1d_nd
    end interface set_to_1d

    type,public :: database

        integer,public :: handle
        integer,public :: number_of_datasets
        character(len=MAX_PATH) :: filename
        character(len=DATASET_NAME_LENGTH),allocatable,public :: dataset_names(:)
        integer,allocatable,private :: dataset_positions(:)
        integer,allocatable,private :: dataset_sizes(:)
        integer,allocatable,private :: dataset_descriptions(:,:)
      contains

        procedure,private :: get_dataset_from_position
        procedure,private :: get_dataset_from_database
        generic,public :: get => get_dataset_from_database, &
     &                           get_dataset_from_position
        procedure,private :: add_dataset_to_database
        procedure,private :: add_dataset_at_position
        procedure,private :: add_dataset_from_data_0d
        procedure,private :: add_dataset_from_data_1d
        procedure,private :: add_dataset_from_data_2d
        procedure,private :: add_dataset_from_data_3d
        procedure,private :: add_dataset_from_data_4d
        procedure,private :: add_dataset_from_data_5d
        procedure,private :: add_dataset_from_data_6d
        procedure,private :: add_dataset_from_data_7d
        generic,public :: add => add_dataset_to_database, &
     &                           add_dataset_from_data_0d, &
     &                           add_dataset_from_data_1d, &
     &                           add_dataset_from_data_2d, &
     &                           add_dataset_from_data_3d, &
     &                           add_dataset_from_data_4d, &
     &                           add_dataset_from_data_5d, &
     &                           add_dataset_from_data_6d, &
     &                           add_dataset_from_data_7d, &
     &                           add_dataset_at_position
        procedure,public :: contains => database_contains
        procedure,private :: remove_dataset_by_number
        procedure,private :: remove_dataset_by_name
        procedure,private :: remove_datasets_by_name
        generic,public :: remove => remove_dataset_by_name,remove_dataset_by_number,remove_datasets_by_name
        procedure,private :: get_dataset_information
        procedure,public :: get_size => get_size_database
        procedure,private :: initialize_database
        procedure,private :: initialize_database_with_filename
        procedure,private :: initialize_database_with_filename_and_unitnumber
        generic,public :: initialize => initialize_database,initialize_database_with_filename,initialize_database_with_filename_and_unitnumber
        procedure,private :: allocator => allocator_database
        procedure,private :: update_number_of_datasets
        procedure,public :: close => close_database
        procedure,public :: delete => delete_database

    end type database

    type,public :: filehandler

        integer,public :: most_recent

      contains
        procedure,public :: initialize => initialize_filehandler
        procedure,public :: new => get_new_handle

    end type filehandler

    integer,parameter :: io_init_handle=1110

    type(filehandler) :: this

contains

    subroutine init_fortdb
        implicit none
        call this%initialize
    end subroutine init_fortdb

    subroutine initialize_filehandler(me)
        implicit none
        class(filehandler) :: me
        me%most_recent=io_init_handle
    end subroutine initialize_filehandler

    function get_new_handle(me) result (handle)
        implicit none
        class(filehandler) :: me
        integer :: handle
        handle=me%most_recent+1
        me%most_recent=me%most_recent+1
    end function get_new_handle

    subroutine initialize_database(me)
        implicit none
        class(database) :: me
        me%number_of_datasets=0
        me%handle=this%new()
    end subroutine initialize_database

    subroutine initialize_database_with_filename(me,filename)
        implicit none
        class(database) :: me
        character(len=*) :: filename
        me%number_of_datasets=0
        me%handle=this%new()
        me%filename=filename
    end subroutine initialize_database_with_filename

    subroutine initialize_database_with_filename_and_unitnumber(me,filename,unitnumber)
        implicit none
        class(database) :: me
        character(len=*) :: filename
        integer :: unitnumber
        me%number_of_datasets=0
        me%handle=unitnumber
        me%filename=filename
    end subroutine initialize_database_with_filename_and_unitnumber

    subroutine close_database(me)
        implicit none
        class(database) :: me
        close(unit=me%handle)
    end subroutine close_database

    subroutine delete_database(me)
        implicit none
        class(database) :: me
        close(unit=me%handle,status='delete')
    end subroutine delete_database

    function get_size_database(me) result (total_size_bytes)
        implicit none
        class(database) :: me
        integer*8 :: total_size_bytes
        integer :: i

        total_size_bytes=WORD_SIZE ! 4-byte number of datasets
        do i=1,me%number_of_datasets
            total_size_bytes=total_size_bytes+DATASET_NAME_LENGTH+ &
     &                       DATASET_DESCRIPTION_LENGTH*WORD_SIZE+ &
     &                       me%dataset_sizes(i)
        enddo
        
    end function get_size_database

    function get_shape_from_description(me) result (s)
        implicit none
        class(dataset) :: me
        integer,allocatable :: s(:)
        integer :: i
        allocate(s(me%description(3)))
        do i=1,me%description(3)
            s(i)=me%description(3+i)
        enddo

    end function get_shape_from_description

    function from_file(filename) result(dbase)
        implicit none
        type(database) :: dbase
        integer :: handle
        integer(kind=4),dimension(DATASET_DESCRIPTION_LENGTH) :: dataset_description
        character(len=*) :: filename
        integer :: i
        handle=this%new()
        dbase%handle=handle
        open(unit=handle,file=filename,status='old',access='stream',form='unformatted',action='read')
        read(handle,pos=1)dbase%number_of_datasets
        call dbase%allocator
        call dbase%get_dataset_information
        dbase%filename=filename
    end function from_file

    subroutine get_dataset_information(me)
        implicit none
        class(database) :: me
        integer :: now_position
        integer(kind=4),dimension(DATASET_DESCRIPTION_LENGTH) :: dataset_description
        integer :: i

        now_position=1+WORD_SIZE
        do i=1,me%number_of_datasets
            me%dataset_positions(i)=now_position
            read(me%handle,pos=now_position)me%dataset_names(i)
            now_position=now_position+DATASET_NAME_LENGTH
            read(me%handle,pos=now_position)dataset_description
            me%dataset_descriptions(:,i)=dataset_description
            now_position=now_position+DATASET_DESCRIPTION_LENGTH*WORD_SIZE
            me%dataset_sizes(i)=data_size_from_description(dataset_description)
            now_position=now_position+me%dataset_sizes(i)
        enddo

    end subroutine get_dataset_information

    function data_size_from_description(description) result (total_size)
        implicit none
        integer(kind=WORD_SIZE) :: ws
        integer :: total_size
        integer(kind=4),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: i
        ! total number of bytes of entry
        select case (description(1))
            case (1)
                ws=4
            case (2)
                ws=4
            case (3)
                ws=8
            case (4)
                ws=8
            case (5)
                ws=description(2)
        end select
        total_size=ws
        do i=1,description(3)
            total_size=total_size*description(3+i)
        enddo
    end function data_size_from_description

    subroutine allocator_database(me)
        implicit none
        class(database) :: me
        integer :: num_dsets
        integer,allocatable :: dataset_positions(:)
        integer,allocatable :: dataset_sizes(:)
        character(len=DATASET_NAME_LENGTH),allocatable :: dataset_names(:)
        integer,allocatable :: dataset_descriptions(:,:)
        integer :: i
        integer :: old_length

        num_dsets=me%number_of_datasets
        if (num_dsets.gt.0) then
            if (.not.allocated(me%dataset_positions)) then
                if (.not.allocated(me%dataset_positions)) allocate(me%dataset_positions(num_dsets))
                if (.not.allocated(me%dataset_names)) allocate(me%dataset_names(num_dsets))
                if (.not.allocated(me%dataset_descriptions)) allocate(me%dataset_descriptions(DATASET_DESCRIPTION_LENGTH,num_dsets))
                if (.not.allocated(me%dataset_sizes)) allocate(me%dataset_sizes(num_dsets))
            elseif (size(me%dataset_positions).ne.num_dsets) then ! resize arrays
                old_length=size(me%dataset_positions)
                allocate(dataset_positions(num_dsets))
                allocate(dataset_names(num_dsets))
                allocate(dataset_descriptions(DATASET_DESCRIPTION_LENGTH,num_dsets))
                allocate(dataset_sizes(num_dsets))
                do i=1,min(old_length,num_dsets)
                    dataset_positions(i)=me%dataset_positions(i)
                    dataset_names(i)=me%dataset_names(i)
                    dataset_descriptions(:,i)=me%dataset_descriptions(:,i)
                    dataset_sizes(i)=me%dataset_sizes(i)
                enddo
                deallocate(me%dataset_positions)
                deallocate(me%dataset_names)
                deallocate(me%dataset_descriptions)
                deallocate(me%dataset_sizes)
                allocate(me%dataset_positions(num_dsets))
                allocate(me%dataset_names(num_dsets))
                allocate(me%dataset_descriptions(DATASET_DESCRIPTION_LENGTH,num_dsets))
                allocate(me%dataset_sizes(num_dsets))
                do i=1,min(old_length,num_dsets)
                    me%dataset_positions(i)=dataset_positions(i)
                    me%dataset_names(i)=dataset_names(i)
                    me%dataset_descriptions(:,i)=dataset_descriptions(:,i)
                    me%dataset_sizes(i)=dataset_sizes(i)
                enddo
                deallocate(dataset_positions)
                deallocate(dataset_names)
                deallocate(dataset_descriptions)
                deallocate(dataset_sizes)

            endif
        endif
    end subroutine allocator_database

    subroutine allocator_dataset(me)
        implicit none
        class(dataset) :: me
        integer :: length
        ! from description, decide what to allocate
        length=get_length_from_description(me%description)
        select case (me%description(1))
            case (1)
                allocate(me%datas_i4(length))
            case (2)
                allocate(me%datas_r4(length))
            case (3)
                allocate(me%datas_i8(length))
            case (4)
                allocate(me%datas_r8(length))
            case (5)
                allocate(character(me%description(2)) :: me%datas_c(length))
        end select
    end subroutine allocator_dataset

    subroutine add_dataset_to_database(me,datset)
        implicit none
        class(database) :: me
        class(dataset) :: datset
        integer :: now_position
        
        me%number_of_datasets=me%number_of_datasets+1

        call me%update_number_of_datasets

        call me%allocator

        if (me%number_of_datasets.eq.1) then
            now_position=WORD_SIZE+1 ! first dataset position just following number of datasets
            me%dataset_sizes(1)=datset%get_size()
            me%dataset_positions(1)=now_position
        else
            now_position=me%dataset_positions(me%number_of_datasets-1)+DATASET_NAME_LENGTH+ &
     &                 DATASET_DESCRIPTION_LENGTH*WORD_SIZE+me%dataset_sizes(me%number_of_datasets-1)
            me%dataset_positions(me%number_of_datasets)=now_position
            me%dataset_sizes(me%number_of_datasets)=datset%get_size()
        endif

        me%dataset_names(me%number_of_datasets)=datset%name
        me%dataset_descriptions(:,me%number_of_datasets)=datset%description
        call me%add_dataset_at_position(datset,now_position)

    end subroutine add_dataset_to_database

    subroutine add_dataset_at_position(me,datset,posit)
        implicit none
        class(database) :: me
        class(dataset) :: datset
        integer,intent(in) :: posit
        integer :: now_position
        open(unit=me%handle,file=me%filename,access='stream',status='unknown',form='unformatted',action='write')
        now_position=posit
        write(me%handle,pos=now_position)datset%name
        now_position=now_position+DATASET_NAME_LENGTH
        write(me%handle,pos=now_position)datset%description
        now_position=now_position+DATASET_DESCRIPTION_LENGTH*WORD_SIZE
        select case (datset%description(1))
            case (1)
                write(me%handle,pos=now_position)datset%datas_i4
            case (2)
                write(me%handle,pos=now_position)datset%datas_r4
            case (3)
                write(me%handle,pos=now_position)datset%datas_i8
            case (4)
                write(me%handle,pos=now_position)datset%datas_r8
            case (5)
                write(me%handle,pos=now_position)datset%datas_c
        end select

        close(me%handle)

    end subroutine add_dataset_at_position

    function database_contains(me,dset_name) result (res)
        implicit none
        class(database) :: me
        character(len=*) :: dset_name
        logical :: res
        integer :: i
        res=.false.
        do i=1,me%number_of_datasets
            if (trim(adjustl(dset_name)).eq.trim(adjustl(me%dataset_names(i)))) then
                res=.true.
                exit
            endif
        enddo

    end function database_contains

    subroutine remove_dataset_by_name(me,dset_name)
        implicit none
        class(database) :: me
        type(dataset) :: dats ! for temporary storage of moved dataset
        character(len=*) :: dset_name

        logical :: ifound
        integer :: i
        integer :: j
        integer :: posir
        integer :: posit
        byte,allocatable :: alldata(:)
        integer*8 :: total_size
        
        ifound=.false.
        do i=1,me%number_of_datasets
            if (trim(adjustl(me%dataset_names(i))).eq.trim(adjustl(dset_name))) then
                ifound=.true.
                exit
            endif
        enddo
        if (ifound) then
            posir=me%dataset_positions(i)
            total_size=me%get_size()
            do j=i+1,me%number_of_datasets
                posit=me%dataset_positions(j)
                dats=me%get_dataset_from_position(posit)
                call me%add_dataset_at_position(dats,posir)
                me%dataset_positions(j-1)=posir
                posir=posir+DATASET_NAME_LENGTH+DATASET_DESCRIPTION_LENGTH*WORD_SIZE+me%dataset_sizes(j)
                me%dataset_sizes(j-1)=me%dataset_sizes(j) ! used in get_size below
                me%dataset_names(j-1)=me%dataset_names(j)
            enddo
            me%number_of_datasets=me%number_of_datasets-1
            call me%allocator
            total_size=me%get_size() ! total size of database in bytes
            allocate(alldata(total_size))

            call me%update_number_of_datasets

            open(unit=me%handle,file=me%filename,form='unformatted',access='stream',status='old',action='read')
            read(unit=me%handle,pos=1)alldata
            call me%delete
            open(unit=me%handle,file=me%filename,form='unformatted',access='stream',status='new',action='write')
            write(me%handle)alldata
            close(me%handle)
            deallocate(alldata)
        endif

    end subroutine remove_dataset_by_name

    subroutine remove_datasets_by_name(me,dset_names)
        implicit none
        class(database) :: me
        type(dataset) :: datset ! for temporary storage of moved dataset
        character(len=DATASET_NAME_LENGTH) :: dset_names(:)
        character(len=DATASET_NAME_LENGTH) :: dset_name

        logical :: ifound
        integer :: i
        integer :: posir
        integer :: posit
        byte,allocatable :: alldata(:)
        integer*8 :: total_size
        integer :: filehandle
        integer*8 :: now_position
        integer :: number_of_datasets

        filehandle=this%new()
        open(unit=filehandle,status='replace',access='stream',form='unformatted',action='readwrite')
        now_position=WORD_SIZE+1
        total_size=WORD_SIZE
        number_of_datasets=0
        do i=1,me%number_of_datasets
            if (list_contains(dset_names,me%dataset_names(i))) then
                cycle
            endif
            number_of_datasets=number_of_datasets+1
            posit=me%dataset_positions(i)
            datset=me%get_dataset_from_position(posit)
            write(filehandle,pos=now_position) datset%name
            now_position=now_position+DATASET_NAME_LENGTH
            write(filehandle) datset%description
            now_position=now_position+DATASET_DESCRIPTION_LENGTH*WORD_SIZE
            select case (datset%description(1))
                case (1)
                    write(filehandle,pos=now_position)datset%datas_i4
                case (2)
                    write(filehandle,pos=now_position)datset%datas_r4
                case (3)
                    write(filehandle,pos=now_position)datset%datas_i8
                case (4)
                    write(filehandle,pos=now_position)datset%datas_r8
                case (5)
                    write(filehandle,pos=now_position)datset%datas_c
            end select
            total_size=total_size+DATASET_NAME_LENGTH+DATASET_DESCRIPTION_LENGTH*WORD_SIZE+datset%get_size()
            now_position=now_position+datset%get_size()
            me%dataset_positions(number_of_datasets)=me%dataset_positions(i)
            me%dataset_names(number_of_datasets)=me%dataset_names(i)
            me%dataset_descriptions(:,number_of_datasets)=me%dataset_descriptions(:,i)
            me%dataset_sizes(number_of_datasets)=me%dataset_sizes(i)
        enddo
        write(filehandle,pos=1)number_of_datasets
        allocate(alldata(total_size))
        read(filehandle,pos=1)alldata
        close(filehandle,status='delete')
        me%number_of_datasets=number_of_datasets        
        call me%allocator
        open(unit=me%handle,file=me%filename,form='unformatted',access='stream',status='replace',action='write')
        write(me%handle,pos=1)alldata
        deallocate(alldata)
        close(me%handle)
        this%most_recent=this%most_recent-1 ! give the file handle back
    end subroutine remove_datasets_by_name

    subroutine remove_dataset_by_number(me,i)
        implicit none
        class(database) :: me
        type(dataset) :: dats ! for temporary storage of moved dataset

        logical :: ifound
        integer :: i
        integer :: j
        integer :: posir
        integer :: posit
        byte,allocatable :: alldata(:)
        integer*8 :: total_size
        
        ifound=.false.
        if (i.le.me%number_of_datasets) ifound=.true.
        if (ifound) then
            posir=me%dataset_positions(i)
            total_size=me%get_size()
            do j=i+1,me%number_of_datasets
                posit=me%dataset_positions(j)
                dats=me%get_dataset_from_position(posit)
                call me%add_dataset_at_position(dats,posir)
                me%dataset_positions(j-1)=posir
                posir=posir+DATASET_NAME_LENGTH+DATASET_DESCRIPTION_LENGTH*WORD_SIZE+me%dataset_sizes(j)
                me%dataset_sizes(j-1)=me%dataset_sizes(j) ! used in get_size below
                me%dataset_names(j-1)=me%dataset_names(j)
            enddo
            me%number_of_datasets=me%number_of_datasets-1
            call me%allocator
            total_size=me%get_size() ! total size of database in bytes
            allocate(alldata(total_size))

            call me%update_number_of_datasets

            open(unit=me%handle,file=me%filename,form='unformatted',access='stream',status='old',action='read')
            read(unit=me%handle,pos=1)alldata
            call me%delete
            open(unit=me%handle,file=me%filename,form='unformatted',access='stream',status='new',action='write')
            write(me%handle)alldata
            close(me%handle)
            deallocate(alldata)
        endif

    end subroutine remove_dataset_by_number

    subroutine update_number_of_datasets(me)
        implicit none
        class(database) :: me
        open(unit=me%handle,file=me%filename,access='stream',status='unknown',form='unformatted',action='write')
        write(me%handle,pos=1)me%number_of_datasets ! update number of datasets
        call me%close
    end subroutine update_number_of_datasets

    function get_dataset_from_position(me,posit) result (dset)
        implicit none
        class(database) :: me
        type(dataset) :: dset
        integer :: posit
        integer :: now_position
        integer :: length
        open(unit=me%handle,file=me%filename,access='stream',status='old',form='unformatted',action='read')
        now_position=posit
        read(me%handle,pos=now_position) dset%name
        now_position=now_position+DATASET_NAME_LENGTH
        read(me%handle,pos=now_position) dset%description
        now_position=now_position+DATASET_DESCRIPTION_LENGTH*WORD_SIZE
        call dset%allocator
        select case (dset%description(1))
            case (1)
                read(me%handle,pos=now_position) dset%datas_i4
            case (2)
                read(me%handle,pos=now_position) dset%datas_r4
            case (3)
                read(me%handle,pos=now_position) dset%datas_i8
            case (4)
                read(me%handle,pos=now_position) dset%datas_r8
            case (5)
                read(me%handle,pos=now_position) dset%datas_c
        end select
        close(me%handle)
    end function get_dataset_from_position

    function dataset_from_data_0d(dset_name,dat) result(dset)
        implicit none
        character(len=*) :: dset_name
        class(*) :: dat
        type(dataset) :: dset
        integer(kind=WORD_SIZE) :: length
        dset%name=dset_name
        dset%name=adjustr(dset%name)
        dset%description=get_dataset_description_from_data(dat)
        length=get_length_from_description(dset%description)
        call set_to_1d(dset,dat)
    end function dataset_from_data_0d

    function dataset_from_data_1d(dset_name,dat) result(dset)
        implicit none
        character(len=*) :: dset_name
        class(*),target :: dat(:)
        type(dataset) :: dset
        integer(kind=WORD_SIZE) :: length
        class(*),pointer :: arr(:) 
        arr(1:size(dat))=>dat
        dset%name=dset_name
        dset%name=adjustr(dset%name)
        dset%description=get_dataset_description_from_data(dat)
        length=get_length_from_description(dset%description)
        call set_to_1d(dset,arr,size(dat))
    end function dataset_from_data_1d

    function dataset_from_data_2d(dset_name,dat) result(dset)
        implicit none
        character(len=*) :: dset_name
        class(*),target :: dat(:,:)
        type(dataset) :: dset
        integer(kind=WORD_SIZE) :: length
        class(*),pointer :: arr(:) 
        arr(1:size(dat))=>dat
        dset%name=dset_name
        dset%name=adjustr(dset%name)
        dset%description=get_dataset_description_from_data(dat)
        length=get_length_from_description(dset%description)
        call set_to_1d(dset,arr,size(dat))

    end function dataset_from_data_2d

    function dataset_from_data_3d(dset_name,dat) result(dset)
        implicit none
        character(len=*) :: dset_name
        class(*),target :: dat(:,:,:)
        type(dataset) :: dset
        integer(kind=WORD_SIZE) :: length
        class(*),pointer :: arr(:) 
        arr(1:size(dat))=>dat
        dset%name=dset_name
        dset%name=adjustr(dset%name)
        dset%description=get_dataset_description_from_data(dat)
        length=get_length_from_description(dset%description)
        call set_to_1d(dset,arr,size(dat))

    end function dataset_from_data_3d

    function dataset_from_data_4d(dset_name,dat) result(dset)
        implicit none
        character(len=*) :: dset_name
        class(*),target :: dat(:,:,:,:)
        type(dataset) :: dset
        integer(kind=WORD_SIZE) :: length
        class(*),pointer :: arr(:) 
        arr(1:size(dat))=>dat
        dset%name=dset_name
        dset%name=adjustr(dset%name)
        dset%description=get_dataset_description_from_data(dat)
        length=get_length_from_description(dset%description)
        call set_to_1d(dset,arr,size(dat))

    end function dataset_from_data_4d

    function dataset_from_data_5d(dset_name,dat) result(dset)
        implicit none
        character(len=*) :: dset_name
        class(*),target :: dat(:,:,:,:,:)
        type(dataset) :: dset
        integer(kind=WORD_SIZE) :: length
        class(*),pointer :: arr(:) 
        arr(1:size(dat))=>dat
        dset%name=dset_name
        dset%name=adjustr(dset%name)
        dset%description=get_dataset_description_from_data(dat)
        length=get_length_from_description(dset%description)
        call set_to_1d(dset,arr,size(dat))

    end function dataset_from_data_5d

    function dataset_from_data_6d(dset_name,dat) result(dset)
        implicit none
        character(len=*) :: dset_name
        class(*),target :: dat(:,:,:,:,:,:)
        type(dataset) :: dset
        integer(kind=WORD_SIZE) :: length
        class(*),pointer :: arr(:) 
        arr(1:size(dat))=>dat
        dset%name=dset_name
        dset%name=adjustr(dset%name)
        dset%description=get_dataset_description_from_data(dat)
        length=get_length_from_description(dset%description)
        call set_to_1d(dset,arr,size(dat))

    end function dataset_from_data_6d

    function dataset_from_data_7d(dset_name,dat) result(dset)
        implicit none
        character(len=*) :: dset_name
        class(*),target :: dat(:,:,:,:,:,:,:)
        type(dataset) :: dset
        integer(kind=WORD_SIZE) :: length
        class(*),pointer :: arr(:) 
        arr(1:size(dat))=>dat
        dset%name=dset_name
        dset%name=adjustr(dset%name)
        dset%description=get_dataset_description_from_data(dat)
        length=get_length_from_description(dset%description)
        call set_to_1d(dset,arr,size(dat))

    end function dataset_from_data_7d

    subroutine set_to_1d_0d(dset,dat)
        class(*) :: dat
        class(dataset) :: dset
        integer :: length
        length=1
        select type (dat)
            type is (integer(kind=4))
                allocate(dset%datas_i4(length))
                dset%datas_i4=dat
            type is (real(kind=4))
                allocate(dset%datas_r4(length))
                dset%datas_r4=dat
            type is (integer(kind=8))
                allocate(dset%datas_i8(length))
                dset%datas_i8=dat
            type is (real(kind=8))
                allocate(dset%datas_r8(length))
                dset%datas_r8=dat
            type is (character(len=*))
                allocate(character(dset%description(2)) :: dset%datas_c(length))
                dset%datas_c=dat
        end select
    end subroutine set_to_1d_0d

    subroutine set_to_1d_nd(dset,dat,length)
        integer,intent(in) :: length
        class(*) :: dat(length)
        class(dataset) :: dset
        select type (dat)
            type is (integer(kind=4))
                allocate(dset%datas_i4(length))
                dset%datas_i4=dat
            type is (real(kind=4))
                allocate(dset%datas_r4(length))
                dset%datas_r4=dat
            type is (integer(kind=8))
                allocate(dset%datas_i8(length))
                dset%datas_i8=dat
            type is (real(kind=8))
                allocate(dset%datas_r8(length))
                dset%datas_r8=dat
            type is (character(len=*))
                allocate(character(dset%description(2)) :: dset%datas_c(length))
                dset%datas_c=dat
        end select
    end subroutine set_to_1d_nd

    subroutine add_dataset_from_data_0d(me,dset_name,dat)
        implicit none
        class(database) :: me
        character(len=*) :: dset_name
        class(*) :: dat
        type(dataset) :: dset
        dset=dataset_from_data(dset_name,dat)
        call me%add(dset)
    end subroutine add_dataset_from_data_0d

    subroutine add_dataset_from_data_1d(me,dset_name,dat)
        implicit none
        class(database) :: me
        character(len=*) :: dset_name
        class(*) :: dat(:)
        type(dataset) :: dset
        dset=dataset_from_data(dset_name,dat)
        call me%add(dset)
    end subroutine add_dataset_from_data_1d

    subroutine add_dataset_from_data_2d(me,dset_name,dat)
        implicit none
        class(database) :: me
        character(len=*) :: dset_name
        class(*) :: dat(:,:)
        type(dataset) :: dset
        dset=dataset_from_data(dset_name,dat)
        call me%add(dset)
    end subroutine add_dataset_from_data_2d

    subroutine add_dataset_from_data_3d(me,dset_name,dat)
        implicit none
        class(database) :: me
        character(len=*) :: dset_name
        class(*) :: dat(:,:,:)
        type(dataset) :: dset
        dset=dataset_from_data(dset_name,dat)
        call me%add(dset)
    end subroutine add_dataset_from_data_3d

    subroutine add_dataset_from_data_4d(me,dset_name,dat)
        implicit none
        class(database) :: me
        character(len=*) :: dset_name
        class(*) :: dat(:,:,:,:)
        type(dataset) :: dset
        dset=dataset_from_data(dset_name,dat)
        call me%add(dset)
    end subroutine add_dataset_from_data_4d

    subroutine add_dataset_from_data_5d(me,dset_name,dat)
        implicit none
        class(database) :: me
        character(len=*) :: dset_name
        class(*) :: dat(:,:,:,:,:)
        type(dataset) :: dset
        dset=dataset_from_data(dset_name,dat)
        call me%add(dset)
    end subroutine add_dataset_from_data_5d

    subroutine add_dataset_from_data_6d(me,dset_name,dat)
        implicit none
        class(database) :: me
        character(len=*) :: dset_name
        class(*) :: dat(:,:,:,:,:,:)
        type(dataset) :: dset
        dset=dataset_from_data(dset_name,dat)
        call me%add(dset)
    end subroutine add_dataset_from_data_6d

    subroutine add_dataset_from_data_7d(me,dset_name,dat)
        implicit none
        class(database) :: me
        character(len=*) :: dset_name
        class(*) :: dat(:,:,:,:,:,:,:)
        type(dataset) :: dset
        dset=dataset_from_data(dset_name,dat)
        call me%add(dset)
    end subroutine add_dataset_from_data_7d

    function get_length_from_description(description) result (s)
        implicit none

        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: s
        integer :: i
        ! return array 1d dimension from description
        select case (description(1))
            case (1)
                s=1
            case (2)
                s=1
            case (3)
                s=1
            case (4)
                s=1
            case (5)
                s=1
        end select

        do i=1,description(3)
            s=s*description(3+i)
        enddo
    end function get_length_from_description

    function get_dataset_description_from_data_0d(dat) result (description)
        implicit none
        class(*) :: dat
        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: i
        description=0
        select type (dat)
            type is (integer(kind=4))
                description(1)=1
            type is (real(kind=4))
                description(1)=2
            type is (integer(kind=8))
                description(1)=3
            type is (real(kind=8))
                description(1)=4
            type is (character(len=*))
                description(1)=5
                description(2)=len(dat)
        end select
    end function get_dataset_description_from_data_0d

    function get_dataset_description_from_data_1d(dat) result (description)
        implicit none
        class(*) :: dat(:)
        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: i
        description=0
        select type (dat)
            type is (integer(kind=4))
                description(1)=1
            type is (real(kind=4))
                description(1)=2
            type is (integer(kind=8))
                description(1)=3
            type is (real(kind=8))
                description(1)=4
            type is (character(len=*))
                description(1)=5
                description(2)=len(dat(1))
                do i=2,size(dat)
                    description(2)=max(description(2),len(dat(i)))
                enddo
        end select
        description(3)=size(shape(dat))
        if (description(3).gt.0) then
            description(4:4+description(3)-1)=shape(dat)
        endif
    end function get_dataset_description_from_data_1d

    function get_dataset_description_from_data_2d(dat) result (description)
        implicit none
        class(*) :: dat(:,:)
        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: i,j
        integer :: shap(2)
        description=0
        select type (dat)
            type is (integer(kind=4))
                description(1)=1
            type is (real(kind=4))
                description(1)=2
            type is (integer(kind=8))
                description(1)=3
            type is (real(kind=8))
                description(1)=4
            type is (character(len=*))
                description(1)=5
                description(2)=len(dat(1,1))
                shap=shape(dat)
                do j=1,shap(2)
                do i=2,size(dat)
                    description(2)=max(description(2),len(dat(i,j)))
                enddo
                enddo
        end select
        description(3)=size(shape(dat))
        if (description(3).gt.0) then
            description(4:4+description(3)-1)=shape(dat)
        endif
    end function get_dataset_description_from_data_2d

    function get_dataset_description_from_data_3d(dat) result (description)
        implicit none
        class(*) :: dat(:,:,:)
        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: i,j,k
        integer :: shap(3)
        
        description=0
        select type (dat)
            type is (integer(kind=4))
                description(1)=1
            type is (real(kind=4))
                description(1)=2
            type is (integer(kind=8))
                description(1)=3
            type is (real(kind=8))
                description(1)=4
            type is (character(len=*))
                description(1)=5
                description(2)=len(dat(1,1,1))
                shap=shape(dat)
                do k=1,shap(3)
                do j=1,shap(2)
                do i=2,shap(1)
                    description(2)=max(description(2),len(dat(i,j,k)))
                enddo
                enddo
                enddo
        end select
        description(3)=size(shape(dat))
        if (description(3).gt.0) then
            description(4:4+description(3)-1)=shape(dat)
        endif
    end function get_dataset_description_from_data_3d

    function get_dataset_description_from_data_4d(dat) result (description)
        implicit none
        class(*) :: dat(:,:,:,:)
        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: i,j,k,l
        integer :: shap(4)
        
        description=0
        select type (dat)
            type is (integer(kind=4))
                description(1)=1
            type is (real(kind=4))
                description(1)=2
            type is (integer(kind=8))
                description(1)=3
            type is (real(kind=8))
                description(1)=4
            type is (character(len=*))
                description(1)=5
                description(2)=len(dat(1,1,1,1))
                shap=shape(dat)
                do l=1,shap(4)
                do k=1,shap(3)
                do j=1,shap(2)
                do i=2,shap(1)
                    description(2)=max(description(2),len(dat(i,j,k,l)))
                enddo
                enddo
                enddo
                enddo
        end select
        description(3)=size(shape(dat))
        if (description(3).gt.0) then
            description(4:4+description(3)-1)=shape(dat)
        endif
    end function get_dataset_description_from_data_4d

    function get_dataset_description_from_data_5d(dat) result (description)
        implicit none
        class(*) :: dat(:,:,:,:,:)
        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: i,j,k,l,m
        integer :: shap(5)
        
        description=0
        select type (dat)
            type is (integer(kind=4))
                description(1)=1
            type is (real(kind=4))
                description(1)=2
            type is (integer(kind=8))
                description(1)=3
            type is (real(kind=8))
                description(1)=4
            type is (character(len=*))
                description(1)=5
                description(2)=len(dat(1,1,1,1,1))
                shap=shape(dat)
                do m=1,shap(5)
                do l=1,shap(4)
                do k=1,shap(3)
                do j=1,shap(2)
                do i=2,shap(1)
                    description(2)=max(description(2),len(dat(i,j,k,l,m)))
                enddo
                enddo
                enddo
                enddo
                enddo
        end select
        description(3)=size(shape(dat))
        if (description(3).gt.0) then
            description(4:4+description(3)-1)=shape(dat)
        endif
    end function get_dataset_description_from_data_5d

    function get_dataset_description_from_data_6d(dat) result (description)
        implicit none
        class(*) :: dat(:,:,:,:,:,:)
        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: i,j,k,l,m,n
        integer :: shap(6)
        
        description=0
        select type (dat)
            type is (integer(kind=4))
                description(1)=1
            type is (real(kind=4))
                description(1)=2
            type is (integer(kind=8))
                description(1)=3
            type is (real(kind=8))
                description(1)=4
            type is (character(len=*))
                description(1)=5
                description(2)=len(dat(1,1,1,1,1,1))
                shap=shape(dat)
                do n=1,shap(6)
                do m=1,shap(5)
                do l=1,shap(4)
                do k=1,shap(3)
                do j=1,shap(2)
                do i=2,shap(1)
                    description(2)=max(description(2),len(dat(i,j,k,l,m,n)))
                enddo
                enddo
                enddo
                enddo
                enddo
                enddo
        end select
        description(3)=size(shape(dat))
        if (description(3).gt.0) then
            description(4:4+description(3)-1)=shape(dat)
        endif
    end function get_dataset_description_from_data_6d

    function get_dataset_description_from_data_7d(dat) result (description)
        implicit none
        class(*) :: dat(:,:,:,:,:,:,:)
        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: i,j,k,l,m,n,o
        integer :: shap(7)
        
        description=0
        select type (dat)
            type is (integer(kind=4))
                description(1)=1
            type is (real(kind=4))
                description(1)=2
            type is (integer(kind=8))
                description(1)=3
            type is (real(kind=8))
                description(1)=4
            type is (character(len=*))
                description(1)=5
                description(2)=len(dat(1,1,1,1,1,1,1))
                shap=shape(dat)
                do o=1,shap(7)
                do n=1,shap(6)
                do m=1,shap(5)
                do l=1,shap(4)
                do k=1,shap(3)
                do j=1,shap(2)
                do i=2,shap(1)
                    description(2)=max(description(2),len(dat(i,j,k,l,m,n,o)))
                enddo
                enddo
                enddo
                enddo
                enddo
                enddo
                enddo
        end select
        description(3)=size(shape(dat))
        if (description(3).gt.0) then
            description(4:4+description(3)-1)=shape(dat)
        endif
    end function get_dataset_description_from_data_7d

    function get_dataset_from_database(me,dset_name) result (dset)
        implicit none
        class(database) :: me
        type(dataset) :: dset
        character(len=*) :: dset_name
        logical :: ifound
        integer :: i
        ifound=.false.
        do i=1,me%number_of_datasets
            if (trim(adjustl(me%dataset_names(i))).eq.trim(dset_name)) then
                ifound=.true.
                exit
            endif
        enddo
        if (ifound) then
            dset=me%get_dataset_from_position(me%dataset_positions(i))
        endif

    end function get_dataset_from_database

    function get_size_dataset(me) result (total_size)
        implicit none
        class(dataset) :: me
        integer :: word_size
        integer :: total_size
        integer :: i

        ! total number of bytes of entry
        select case (me%description(1))
            case (1)
                word_size=4
            case (2)
                word_size=4
            case (3)
                word_size=8
            case (4)
                word_size=8
            case (5)
                word_size=me%description(2)
        end select
        total_size=word_size
        do i=1,me%description(3)
            total_size=total_size*me%description(3+i)
        enddo
    end function get_size_dataset

    function list_contains_i4(list,item) result (icontain)
        implicit none
        integer*4 :: list(:)
        integer*4 :: item
        logical :: icontain
        integer :: i
        icontain=.false.
        do i=1,size(list)
            if (item.eq.list(i)) then
                icontain=.true.
                exit
            endif
        enddo
    end function list_contains_i4

    function list_contains_i8(list,item) result (icontain)
        implicit none
        integer*8 :: list(:)
        integer*8 :: item
        logical :: icontain
        integer :: i
        icontain=.false.
        do i=1,size(list)
            if (item.eq.list(i)) then
                icontain=.true.
                exit
            endif
        enddo
    end function list_contains_i8

    function list_contains_r4(list,item) result (icontain)
        implicit none
        real*4 :: list(:)
        real*4 :: item
        logical :: icontain
        integer :: i
        icontain=.false.
        do i=1,size(list)
            if (item.eq.list(i)) then
                icontain=.true.
                exit
            endif
        enddo
    end function list_contains_r4

    function list_contains_r8(list,item) result (icontain)
        implicit none
        real*8 :: list(:)
        real*8 :: item
        logical :: icontain
        integer :: i
        icontain=.false.
        do i=1,size(list)
            if (item.eq.list(i)) then
                icontain=.true.
                exit
            endif
        enddo
    end function list_contains_r8

    function list_contains_str(list,item) result (icontain)
        implicit none
        character(len=*) :: list(:)
        character(len=*) :: item
        logical :: icontain
        integer :: i
        icontain=.false.
        do i=1,size(list)
            if (trim(adjustl(item)).eq.trim(adjustl(list(i)))) then
                icontain=.true.
                exit
            endif
        enddo
    end function list_contains_str

end module fortDB

