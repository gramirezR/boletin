These IDL, FORTRAN 90, Python, C++ and Matlab routines are available 
to aid in reading the RSS ASCAT scatterometer bytemap daily 
and time-averaged data files.  The read routines have been
tested and work correctly within our PC environment.
We do not guarantee that they work perfectly in a different
environment with different compilers.  If portability is a problem,
we suggest using the Python code.

The routines work on ASCAT version 02.1 data(released April 2016)
The ASCAT V02.1 data use the C-2015 geophysical model function (GMF).

There is an ascat_v02.1_daily_verify.txt file and associated
image (ascat_v02.1_daily_verify.png) to help you make sure
you are still reading the data correctly after you've 
adapted one of these programs to your needs.  The 
ascat_v02.1_averaged_verify.txt file can be used to check 
the 3-day, weekly and monthly data.

3-day files are an average of 3 days ending on file date
weekly files are an average of 7 days ending on Saturday of file date
monthly files are an average of all days in the calendar month

The FORTRAN subroutines get_ascat_daily.f and get_ascat_averaged.f
each contain a description of the data file at the top of the subroutine.  
The 3-day, weekly and monthly binary files can all be read with the
get_ascat_averaged.f routine by supplying the correct filename with 
path. These routines have been tested with Compaq Fortran90. Data files 
must be unzipped prior to using these routines. Use a file unzipper of your choice.


The IDL program get_ascat_daily.pro requires a full path file name
and returns five 1440x720x2 real arrays.  The time-averaged
data files are read using get_ascat_averaged.pro.  This routine
returns four 1440x720 real arrays.  A description is provided 
within the routine.  These routines have been tested with IDL 8.5
Data files do not need to be unzipped when using the /compress keyword
in the routine.


We also provide Matlab read routines, get_ascat_daily.m and
get_ascat_averaged.m    These routines function like the IDL 
routines above and have been tested with Matlab 7.1  Data files 
must be unzipped before using the Matlab routines.  Use a file unzipper of your choice.


The Python code consists of ascat_daily.py and ascat_averaged.py.  
Both require the use of bytemaps.py.  The example_usage_v02.1.py code provides a main
program that you can use to test the data or adapt to your needs.
Description of file contents is provided at the top of each ascat reading routine.
Data files do not need to be unzipped.


The C++ code consists of ascat_daily.h and ascat_daily.cpp as well as
the averaged files.  To use, one needs the dataset.h and the dataset.cpp also.
The program ascat_example_usage.cpp provides a main program that you can 
use to test the data or to adapt to your own needs.  Description of data 
contents is located in the comments at the top of each file.


Once you have further developed one of these skeleton programs
to suit your processing needs, you may wish to use the verify file 
to confirm that the data files are still being read correctly.
Please check that you have downloaded the correct verify file.

If you have any questions regarding these programs 
or the RSS binary data files, contact RSS support:
http://www.remss.com/support
