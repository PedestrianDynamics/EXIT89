program Create_Batch_File
	implicit none
   
	! Place variable declarations here
	integer i
	character*8 file_name
	! Place executable statements here
	open (unit=9,file='runall.bat',form='formatted')
	do i=1, 2000
		write (file_name,fmt='(a4,i4.4)') 'exit',i
		write (9,fmt=100) file_name
		write (9,fmt=110) file_name
		write (9,fmt=120)
	end do
	close (unit=9)
100 format ('copy ',a,'.dat dphiltmp.dat',/,'exit89')
110 format ('copy dphiltmp.out ',a,'.out')
120 format ('del dphiltmp.dat dphiltmp.out')

end program Create_Batch_File
