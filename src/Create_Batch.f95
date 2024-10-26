program Create_Scripts
    implicit none
    
    ! Constants
    integer, parameter      :: MAX_FILES = 2000
    integer, parameter      :: FILENAME_LEN = 8
    integer, parameter      :: WIN_UNIT = 10
    integer, parameter      :: UNIX_UNIT = 11
    character(*), parameter :: WIN_FILE = 'runall.bat'
    character(*), parameter :: UNIX_FILE = 'runall.sh'
    character(*), parameter :: TEMP_DAT = 'dphiltmp.dat'
    character(*), parameter :: TEMP_OUT = 'dphiltmp.out'
    character(*), parameter :: PREFIX = 'exit'
    
    ! Variables
    integer               :: i, ios
    character(FILENAME_LEN) :: file_name
    logical               :: file_exists
    
    ! Create Windows batch file
    call create_windows_script()
    
    ! Create Unix shell script
    call create_unix_script()
    
    write(*, '(A)') 'Successfully created script files:'
    write(*, '(A)') '  - ' // WIN_FILE // ' for Windows'
    write(*, '(A)') '  - ' // UNIX_FILE // ' for Linux/macOS'
    write(*, '(A)') ''
    write(*, '(A)') 'Note: For Unix systems, you may need to run:'
    write(*, '(A)') '      chmod +x ' // UNIX_FILE

contains

    subroutine create_windows_script()
        ! Open batch file for Windows
        open(unit=WIN_UNIT, file=WIN_FILE, form='formatted', &
             iostat=ios, status='replace', action='write')
        if (ios /= 0) then
            write(*, '(A)') 'Error: Could not create Windows batch file'
            return
        end if
        
        ! Write simplified batch file content
        write(WIN_UNIT, '(A)') '@echo off'
        write(WIN_UNIT, '(A)') 'echo Starting processing...'
        write(WIN_UNIT, '(A)') ''
        
        do i = 1, MAX_FILES
            write(file_name, '(A,I4.4)') PREFIX, i
            write(WIN_UNIT, '(A)') 'copy ' // trim(file_name) // '.dat ' // TEMP_DAT
            write(WIN_UNIT, '(A)') 'exit89'
            write(WIN_UNIT, '(A)') 'copy ' // TEMP_OUT // ' ' // trim(file_name) // '.out'
            write(WIN_UNIT, '(A)') 'del ' // TEMP_DAT // ' ' // TEMP_OUT
            write(WIN_UNIT, '(A)') ''
        end do
        
        write(WIN_UNIT, '(A)') 'echo Processing completed'
        close(WIN_UNIT)
    end subroutine create_windows_script
    
    subroutine create_unix_script()
        ! Open shell script for Unix
        open(unit=UNIX_UNIT, file=UNIX_FILE, form='formatted', &
             iostat=ios, status='replace', action='write')
        if (ios /= 0) then
            write(*, '(A)') 'Error: Could not create Unix shell script'
            return
        end if
        
        ! Write shell script headers
        write(UNIX_UNIT, '(A)') '#!/bin/bash'
        write(UNIX_UNIT, '(A)') ''
        write(UNIX_UNIT, '(A)') '# Define exit89 function (replace with actual implementation)'
        write(UNIX_UNIT, '(A)') 'exit89() {'
        write(UNIX_UNIT, '(A)') '    # Add your exit89 equivalent command here'
        write(UNIX_UNIT, '(A)') '    command_status=$?'
        write(UNIX_UNIT, '(A)') '    if [ $command_status -ne 0 ]; then'
        write(UNIX_UNIT, '(A)') '        echo "Error in processing"'
        write(UNIX_UNIT, '(A)') '        exit $command_status'
        write(UNIX_UNIT, '(A)') '    fi'
        write(UNIX_UNIT, '(A)') '}'
        write(UNIX_UNIT, '(A)') ''
        write(UNIX_UNIT, '(A)') 'echo "Starting processing..."'
        write(UNIX_UNIT, '(A)') ''
        
        ! Write commands for each file
        do i = 1, MAX_FILES
            write(file_name, '(A,I4.4)') PREFIX, i
            write(UNIX_UNIT, '(A)') 'cp ' // trim(file_name) // '.dat ' // TEMP_DAT
            write(UNIX_UNIT, '(A)') 'exit89'
            write(UNIX_UNIT, '(A)') 'cp ' // TEMP_OUT // ' ' // trim(file_name) // '.out'
            write(UNIX_UNIT, '(A)') 'rm ' // TEMP_DAT // ' ' // TEMP_OUT
            write(UNIX_UNIT, '(A)') ''
        end do
        
        write(UNIX_UNIT, '(A)') 'echo "Processing completed"'
        write(UNIX_UNIT, '(A)') 'exit 0'
        
        close(UNIX_UNIT)
    end subroutine create_unix_script
    
end program Create_Scripts
