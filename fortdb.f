module fortDB
!
! Fortran Database (binary) file definition allows easy reading and writing of datasets.
!
!  Must initialize file handler with
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
!    dset1
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
    integer,parameter :: DIMENSIONS=7
    integer,parameter :: DATASET_DESCRIPTION_LENGTH=3+DIMENSIONS
    integer,parameter :: DATASET_NAME_LENGTH=8
    integer,parameter :: MAX_PATH=256
    type,public :: dataset

        character(len=DATASET_NAME_LENGTH),public :: name
        integer,private :: description(DATASET_DESCRIPTION_LENGTH)
        integer,private :: word_size
        integer(kind=4),allocatable :: datas_i4(:)
        integer(kind=8),allocatable :: datas_i8(:)
        real(kind=4),allocatable :: datas_r4(:)
        real(kind=8),allocatable :: datas_r8(:)

      contains

        !procedure,public :: get      => get_data_from_dataset
        procedure,public :: get_size => get_dataset_size
        procedure,private :: allocator => allocator_dataset
        procedure,public :: get_shape => get_shape_from_description

    end type dataset


    type,public :: database

        integer,private :: handle
        integer,private :: number_of_datasets
        character(len=MAX_PATH) :: filename
        character(len=DATASET_NAME_LENGTH),allocatable,private :: dataset_names(:)
        integer,allocatable,private :: dataset_positions(:)
        integer,allocatable,private :: dataset_sizes(:)
        integer,allocatable,private :: dataset_descriptions(:,:)
      contains

        procedure,public :: get_dataset_from_position
        procedure,public :: get_dataset_from_database
        generic,public :: get => get_dataset_from_database,get_dataset_from_position      ! get dataset from database
!        procedure,public :: to_file   ! write database to specified file
        procedure,public :: add_dataset_to_database
!        procedure,public :: add_data_to_database
        procedure,public :: add_dataset_at_position
        procedure,public :: add_dataset_from_data
        generic,public :: add => add_dataset_to_database, &
     &                           add_dataset_from_data, &
     &                           add_dataset_at_position
        procedure,public :: get_dataset_information
        procedure,public :: initialize_database
        procedure,public :: initialize_database_with_filename
        generic,public :: initialize => initialize_database,initialize_database_with_filename
        procedure,public :: allocator => allocator_database

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
        integer :: now_position
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

        now_position=1+4
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
            elseif (size(me%dataset_positions).lt.num_dsets) then ! resize arrays
                old_length=size(me%dataset_positions)
                allocate(dataset_positions(num_dsets))
                allocate(dataset_names(num_dsets))
                allocate(dataset_descriptions(DATASET_DESCRIPTION_LENGTH,num_dsets))
                allocate(dataset_sizes(num_dsets))
                do i=1,old_length
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
                do i=1,old_length
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
                allocate(me%datas_i8(length))
            case (3)
                allocate(me%datas_r4(length))
            case (4)
                allocate(me%datas_r8(length))
        end select
    end subroutine allocator_dataset

    subroutine add_dataset_to_database(me,datset)
        implicit none
        class(database) :: me
        class(dataset) :: datset
        integer :: now_position
        integer :: dset_size
        
        me%number_of_datasets=me%number_of_datasets+1
        open(unit=me%handle,file=me%filename,access='stream',form='unformatted',action='write')
        write(me%handle,pos=1)me%number_of_datasets ! update number of datasets
        close(me%handle)
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
        integer :: posit
        integer :: now_position
        open(unit=me%handle,file=me%filename,access='stream',form='unformatted',action='write')
        now_position=posit
        write(me%handle,pos=now_position)datset%name
        now_position=now_position+DATASET_NAME_LENGTH
        write(me%handle,pos=now_position)datset%description
        now_position=now_position+DATASET_DESCRIPTION_LENGTH*WORD_SIZE
        select case (datset%description(1))
            case (1)
                write(me%handle,pos=now_position)datset%datas_i4
            case (2)
                write(me%handle,pos=now_position)datset%datas_i8
            case (3)
                write(me%handle,pos=now_position)datset%datas_r4
            case (4)
                write(me%handle,pos=now_position)datset%datas_r8
        end select

        close(me%handle)

    end subroutine add_dataset_at_position

    function get_dataset_from_position(me,posit) result (dset)
        implicit none
        class(database) :: me
        type(dataset) :: dset
        integer :: posit
        integer :: now_position
        integer :: length
        open(unit=me%handle,file=me%filename,access='stream',form='unformatted',action='read')
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
                read(me%handle,pos=now_position) dset%datas_i8
            case (3)
                read(me%handle,pos=now_position) dset%datas_r4
            case (4)
                read(me%handle,pos=now_position) dset%datas_r8
        end select
        close(me%handle)
    end function get_dataset_from_position

    function dataset_from_data(dset_name,dat) result(dset)
        implicit none
        character(len=DATASET_NAME_LENGTH) :: dset_name
        class(*) :: dat(:)
        type(dataset) :: dset
        integer(kind=WORD_SIZE) :: length
        dset%name=dset_name
        dset%description=get_dataset_description_from_data(dat)
        length=get_1word_length_from_description(dset%description)
        select type (dat)
            type is (integer(kind=4))
                allocate(dset%datas_i4(length))
                dset%datas_i4=dat
            type is (integer(kind=8))
                allocate(dset%datas_i8(length))
                dset%datas_i8=dat
            type is (real(kind=4))
                allocate(dset%datas_r4(length))
                dset%datas_r4=dat
            type is (real(kind=8))
                allocate(dset%datas_r8(length))
                dset%datas_r8=dat
        end select

    end function dataset_from_data

    subroutine add_dataset_from_data(me,dset_name,dat)
        implicit none
        class(database) :: me
        character(len=DATASET_NAME_LENGTH) :: dset_name
        class(*) :: dat(:)
        type(dataset) :: dset
        dset=dataset_from_data(dset_name,dat)
        call me%add(dset)
    end subroutine add_dataset_from_data

    function get_1word_length_from_description(description) result (s)
        implicit none

        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        integer :: s
        integer :: i
        ! return 4-byte word length (dimension) from description
        select case (description(1))
            case (1)
                s=1
            case (2)
                s=2
            case (3)
                s=1
            case (4)
                s=2
            case (5)
                s=description(2)
        end select

        do i=1,description(3)
            s=s*description(3+i)
        enddo
    end function get_1word_length_from_description

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
                s=description(2)
        end select

        do i=1,description(3)
            s=s*description(3+i)
        enddo
    end function get_length_from_description

    function get_dataset_description_from_data(dat) result (description)
        implicit none
        class(*) :: dat(:)
        integer(kind=WORD_SIZE),dimension(DATASET_DESCRIPTION_LENGTH) :: description
        description=0
        select type (dat)
            type is (integer(kind=4))
                description(1)=1
            type is (integer(kind=8))
                description(1)=2
            type is (real(kind=4))
                description(1)=3
            type is (real(kind=8))
                description(1)=4
            type is (character(len=*))
                description(1)=5
                description(2)=size(dat)
        end select
        description(3)=size(shape(dat))
        if (description(3).gt.0) then
            description(4:4+description(3)-1)=shape(dat)
        endif
    end function get_dataset_description_from_data

    function get_dataset_from_database(me,dset_name) result (dset)
        implicit none
        class(database) :: me
        type(dataset) :: dset
        character(len=*) :: dset_name
        logical :: ifound
        integer :: i
        ifound=.false.
        do i=1,me%number_of_datasets
            if (me%dataset_names(i).eq.dset_name) then
                ifound=.true.
                exit
            endif
        enddo
        if (ifound) then
            dset=me%get_dataset_from_position(me%dataset_positions(i))
            
        endif

    end function get_dataset_from_database

    function get_dataset_size(me) result (total_size)
        implicit none
        class(dataset) :: me
        integer :: word_size
        integer :: total_size
        integer :: i

        ! total number of bytes of entry
        if (me%description(1).eq.1) then
            word_size=4
        elseif (me%description(1).eq.2) then
            word_size=4
        elseif (me%description(1).eq.3) then
            word_size=8
        elseif (me%description(1).eq.4) then
            word_size=8
        elseif (me%description(1).eq.5) then
            word_size=me%description(2)
        endif
        total_size=word_size
        do i=1,me%description(3)
            total_size=total_size*me%description(3+i)
        enddo
    end function get_dataset_size



end module fortDB

