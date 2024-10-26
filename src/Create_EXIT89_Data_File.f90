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

   ! Variables

   integer, parameter :: nrow = 9000, ncol = 20, &
                         ic_zone = 2, ic_begin_floor = 3, ic_end_floor = 4, ic_base_node = 5, &
                         ic_title = 3

   integer numr, numc, ir, ibegin, iend, ibase, data_columns(ncol), ic, needed_columns, &
      n_data_column, n_col, num_in_set, ir_first_tag, ir_last_tag, i_floor, ir_set, izone, &
      length, i_op
   real rarray(nrow, ncol)
   character carray(nrow, ncol)*80, data_file*128, needed_tag*4, czone*50
   character*29, dimension(9) :: Op_titles = (/"UNITS (1-METRIC,2-STD)      =", &
                                               "SIZE (1-AUST,2-SOVIET,3-US) =", &
                                               "SPEED (1-EMER,2-NORMAL)     =", &
                                               "PATH (1-SHORTEST,2-DIRECTED)=", &
                                               "SMOKE (1-CFAST,2-USER/NONE) =", &
                                               "CONTRA (1-IF CONTRA FLOWS)  =", &
                                               "OUTPUT (1-FULL,2-BRIEF)     =", &
                                               "NUM OF STAIRWAYS (00-10)    =", &
                                               "STAIR TRAVEL (1-DOWN, 2-UP) ="/)
   logical columns_found, its_arcs, its_nodes, tag_found, its_initialization, no_separator_yet

   ! Body of data file creation
   ! Read in spreadsheet that specifies the arcs and nodes
   data_file = 'EXIT89.csv'
   open (unit=8, file=data_file, form='formatted')
   call readcsv(8, rarray, carray, nrow, ncol, 1, numr, numc)
   write (*, *) 'Data file read, rows and columns=', numr, numc
   close (unit=8)
   open (unit=9, file='exit89.dat', form='formatted')

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
      if (carray(ir, 2) .eq. 'Title') write (9, 120) carray(ir, ic_title) (2:80)
      if (columns_found) then
         if (carray(ir, 2) .eq. 'Options') then
            do i_op = 1, 7
               write (9, 130) Op_titles(i_op), int(rarray(ir, data_columns(i_op)))
            end do
            write (9, 140) Op_titles(8), int(rarray(ir, data_columns(8)))
            write (9, 130) Op_titles(9), int(rarray(ir, data_columns(9)))
         else if (carray(ir, 2) .eq. 'Delay') then
            write (9, 150) 'RANDOM DELAY (1-Y,2-N) =', int(rarray(ir, data_columns(1))), &
               ', PROB =', int(rarray(ir, data_columns(2)))
            write (9, 160) '    DISTRIBUTION (1-UNIFORM, 2-LOGNORMAL) =', int(rarray(ir, data_columns(3)))
            write (9, 170) '    MIN OR STDEV   = ', carray(ir, data_columns(4)), &
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
                     write (9, 100) int(rarray(ir_set, data_columns(1))) - ibase + i_floor*100, &
                        (rarray(ir_set, data_columns(ic)), ic=2, 4), &
                        int(rarray(ir_set, data_columns(5))) - ibase + i_floor*100
                  else if (its_nodes) then
                     if (no_separator_yet) write (9, 120) '99999   0.0   0.0   0.0  000'
                     no_separator_yet = .false.
                     write (9, 110) int(rarray(ir_set, data_columns(1))) - ibase + i_floor*100, &
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
   write (9, 120) '9999 9999'
   close (unit=9)
100 format(i5, f6.1, f6.1, f6.1, i5)
110 format(i5, i5, f6.1, i5, i5, i5, i5, f6.1)
120 format(a)
130 format(a29, i1)
140 format(a29, i2.2)
150 format(a24, i1, a8, i3)
160 format(a43, i1)
170 format(a20, a4, a18, a4)
end program Create_EXIT89_Data_File

