!  Pack_String.f90
!
!  FUNCTIONS:
!	pack_string      - Subroutine to eliminate multiple blanks from a string
!

	subroutine pack_string(string)

	implicit none

	! Variables

	character string*(*), tmp*(512)
	integer ic,i_space,len_string

	! Body of pack_string
	
	if (len(string).gt.512) stop 'String too long in Pack_String'
	len_string = len(string)
	ic = 0
10	ic = ic + 1
	if (string(ic:ic+1).eq.'  ') then
		tmp=string
		string=tmp(1:ic) // tmp(ic+2:len_string)
		ic = ic - 1
	end if
	if (string(ic+1:len_string).ne.' '.and. ic.lt.len_string-1) go to 10
	end subroutine pack_string