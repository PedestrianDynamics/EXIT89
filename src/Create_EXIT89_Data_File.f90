!  Create EXIT89 Data File.f90
!
!  FUNCTIONS:
!        Create EXIT89 Data File      - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Create EXIT89 Data File
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program Create_EXIT89_Data_File

   implicit none

   ! Constants for array dimensions and column indices
   integer, parameter :: NROW = 9000
   integer, parameter :: NCOL = 20
   integer, parameter :: IC_ZONE = 2
   integer, parameter :: IC_BEGIN_FLOOR = 3
   integer, parameter :: IC_END_FLOOR = 4
   integer, parameter :: IC_BASE_NODE = 5
   integer, parameter :: IC_TITLE = 3

   ! Variables
   integer :: numr, numc, ir, ibegin, iend, ibase, ic, needed_columns
   integer :: n_data_column, n_col, num_in_set, ir_first_tag, ir_last_tag
   integer :: i_floor, ir_set, izone, length, i_op
   integer :: ios ! For I/O status
   integer :: data_columns(NCOL)
   real :: rarray(NROW, NCOL)
   character(80) :: carray(NROW, NCOL)
   character(4) :: needed_tag
   character(50) :: czone
   logical :: columns_found, its_arcs, its_nodes, tag_found
   logical :: its_initialization, no_separator_yet
   logical :: file_exists

   ! Operation titles array
   character(29), parameter, dimension(9) :: OP_TITLES = [ &
                                             "UNITS (1-METRIC,2-STD)      =", &
                                             "SIZE (1-AUST,2-SOVIET,3-US) =", &
                                             "SPEED (1-EMER,2-NORMAL)     =", &
                                             "PATH (1-SHORTEST,2-DIRECTED)=", &
                                             "SMOKE (1-CFAST,2-USER/NONE) =", &
                                             "CONTRA (1-IF CONTRA FLOWS)  =", &
                                             "OUTPUT (1-FULL,2-BRIEF)     =", &
                                             "NUM OF STAIRWAYS (00-10)    =", &
                                             "STAIR TRAVEL (1-DOWN, 2-UP) =" &
                                             ]

   ! Body of data file creation
   ! Read in spreadsheet that specifies the arcs and nodes
   ! File names and units
   character(*), parameter :: INPUT_FILE = '../data/EXIT89.csv'
   character(*), parameter :: OUTPUT_FILE = 'exit89.dat'
   integer, parameter :: INPUT_UNIT = 8
   integer, parameter :: OUTPUT_UNIT = 9

   ! Check input file existence
   inquire (file=INPUT_FILE, exist=file_exists)
   if (.not. file_exists) then
      write (*, '(A)') 'Error: Input file '//trim(INPUT_FILE)//' not found.'
      stop
   end if

   ! Check if output file already exists
   inquire (file=OUTPUT_FILE, exist=file_exists)
   if (file_exists) then
      write (*, '(A)') 'Warning: '//trim(OUTPUT_FILE)//' already exists and will be overwritten.'
   end if

   ! Open input file with error handling
   open (unit=INPUT_UNIT, file=INPUT_FILE, form='formatted', iostat=ios, status='old')
   if (ios /= 0) then
      write (*, '(A, A, I0)') 'Error opening input file: ', INPUT_FILE, ' IOSTAT = ', ios
      stop
   end if

   ! Read CSV data
   call readcsv(INPUT_UNIT, rarray, carray, NROW, NCOL, 1, numr, numc)
   if (numr <= 0 .or. numc <= 0) then
      write (*, '(A)') 'Error: No data read from input file.'
      close (INPUT_UNIT)
      stop
   end if
   open (unit=OUTPUT_UNIT, file=OUTPUT_FILE, form='formatted')

   tag_found = .false.
   no_separator_yet = .true.
   ir = 0
10 ir = ir + 1
   ! each set of nodes or arcs starts with a keyword "Arcset" or "Nodeset"
   if (carray(ir, 1) .eq. 'Arcset') then
      its_initialization = .false.
      its_arcs = .true.
      its_nodes = .false.
      czone = carray(ir, ic_zone)
      ibegin = rarray(ir, ic_begin_floor)        ! first floor in set
      iend = rarray(ir, ic_end_floor)                ! last floor in set
      ibase = rarray(ir, ic_base_node)                ! base node number for set; others relative to this #
      columns_found = .false.                                ! true if data columns have been defined for this set
      needed_columns = 5                                        ! number of data columns for EXIT89 arcs
      needed_tag = 'Arc'
      num_in_set = 0
   else if (carray(ir, 1) .eq. 'Nodeset') then
      its_initialization = .false.
      its_nodes = .true.
      its_arcs = .false.
      czone = carray(ir, ic_zone)
      ibegin = rarray(ir, ic_begin_floor)        ! first floor in set
      iend = rarray(ir, ic_end_floor)                ! last floor in set
      ibase = rarray(ir, ic_base_node)                ! base node number for set; others relative to this #
      columns_found = .false.                                ! true if data columns have been defined for this set
      needed_columns = 8                                        ! number of data columns for EXIT89 nodes
      needed_tag = 'Node'
      num_in_set = 0
   else if (carray(ir, 1) .eq. 'Initial') then
      its_initialization = .true.
      its_nodes = .false.
      its_arcs = .false.
      needed_columns = 9

      ! for both arcs and nodes, data columns are defined with C1, C2, ...
   else if (carray(ir, 2) .eq. 'Columns') then
      do ic = 1, ncol
         data_columns(ic) = 0
         if (carray(ir, ic) (1:1) .eq. 'C') then
            n_col = ichar(carray(ir, ic) (2:2)) - ichar("1") + 1
            if (n_col .le. ncol) data_columns(n_col) = ic
         end if
      end do
      do ic = 1, needed_columns
         if (data_columns(ic) .eq. 0) stop 'Missing data column for arc or node set'
      end do
      columns_found = .true.
      tag_found = .false.
      !write (*,'(a4,a,a12,a,8i3)') needed_tag,' = ',czone(1:length(czone)),' Columns = ',(data_columns(ic),ic=1,needed_columns)

      ! Initialization information is fixed format with titles at the beginning
   elseif (its_initialization) then
      if (carray(ir, 2) .eq. 'Title') write (OUTPUT_UNIT, 120) carray(ir, ic_title) (2:80)
      if (columns_found) then
         if (carray(ir, 2) .eq. 'Options') then
            do i_op = 1, 7
               write (OUTPUT_UNIT, 130) Op_titles(i_op), int(rarray(ir, data_columns(i_op)))
            end do
            write (OUTPUT_UNIT, 140) Op_titles(8), int(rarray(ir, data_columns(8)))
            write (OUTPUT_UNIT, 130) Op_titles(9), int(rarray(ir, data_columns(9)))
         else if (carray(ir, 2) .eq. 'Delay') then
            write (OUTPUT_UNIT, 150) 'RANDOM DELAY (1-Y,2-N) =', int(rarray(ir, data_columns(1))), &
               ', PROB =', int(rarray(ir, data_columns(2)))
            write (OUTPUT_UNIT, 160) '    DISTRIBUTION (1-UNIFORM, 2-LOGNORMAL) =', int(rarray(ir, data_columns(3)))
            write (OUTPUT_UNIT, 170) '    MIN OR STDEV   = ', carray(ir, data_columns(4)), &
               ', MAX OR MEAN    =', carray(ir, data_columns(5))
         end if

      end if

      ! once we know we are looking for arcs or nodes and columns are defined, we can find the full set of arcs or nodes
      ! and replicate them for each floor
   else if (its_arcs .eqv. .true. .or. its_nodes .eqv. .true.) then
      if (columns_found) then
         if (carray(ir, 2) .eq. needed_tag .and. tag_found .eqv. .false.) then
            ir_first_tag = ir
            tag_found = .true.
         else if (carray(ir, 2) .eq. 'Endset') then
            if (tag_found .eqv. .false.) stop 'No beginning arc or node before Endset'
            ir_last_tag = ir - 1
            ! we found everything we need ...
            ! set floors, columns, first row and last row in set, let's write stuff
            ! write (9,*) 'Beginning of ',needed_tag,' Zone ',czone(1:length(czone))
            do i_floor = ibegin, iend
               do ir_set = ir_first_tag, ir_last_tag
                  if (its_arcs) then
                     write (OUTPUT_UNIT, 100) int(rarray(ir_set, data_columns(1))) - ibase + i_floor*100, &
                        (rarray(ir_set, data_columns(ic)), ic=2, 4), &
                        int(rarray(ir_set, data_columns(5))) - ibase + i_floor*100
                  else if (its_nodes) then
                     if (no_separator_yet) write (9, 120) '99999   0.0   0.0   0.0  000'
                     no_separator_yet = .false.
                     write (OUTPUT_UNIT, 110) int(rarray(ir_set, data_columns(1))) - ibase + i_floor*100, &
                        int(rarray(ir_set, data_columns(2))), &
                        rarray(ir_set, data_columns(3)), &
                        (int(rarray(ir_set, data_columns(ic))), ic=4, 7), &
                        rarray(ir_set, data_columns(8))
                  end if
               end do
            end do
         end if
      end if
   end if
   if (ir .lt. nrow) go to 10
   write (OUTPUT_UNIT, 120) '9999 9999'
   close (unit=OUTPUT_UNIT)
100 format(i5, f6.1, f6.1, f6.1, i5)
110 format(i5, i5, f6.1, i5, i5, i5, i5, f6.1)
120 format(a)
130 format(a29, i1)
140 format(a29, i2.2)
150 format(a24, i1, a8, i3)
160 format(a43, i1)
170 format(a20, a4, a18, a4)
   write (*, '(A)') 'The program has finished successfully.'
end program Create_EXIT89_Data_File

