! change horizontal resolution stepwise from 250m to 4km outside the bullseye
! Draft version coded by hparish in Nov. 2018.
! Verified and tested by hparish in Dec. 2018.
! Questions: please contact h.parish@uci.edu

subroutine bullseye_dx_change (my_lat,my_lon)

use vars

implicit none

#ifdef BULLSEYECRMDX

! locally defined parameters:
real, parameter :: DX_MIN        = 250.0
real, parameter :: DX_MAX        = 4000.0

real(r8)      :: dlat, dlon, my_lat, my_lon, my_lon_shifted
real(r8)      :: fhorizontal, fnep, fsep, fnamb, dx_old
integer       :: i,j,k

!dlon = 15.0;
!dlat = 20.0;

dlon = 60.0;
dlat = 80.0;

fnep = (30.0 - ((my_lon-230.0)**2.0/dlon + (my_lat-26.0)**2.0/dlat))/30.0
fnep = min(fnep,1.0)
fnep = max(fnep,0.0)
fsep = (30.0 - ((my_lon-280.0)**2.0/dlon + (my_lat+18.0)**2.0/dlat))/30.0
fsep = min(fsep,1.0)
fsep = max(fsep,0.0)

my_lon_shifted = my_lon
if (my_lon_shifted .gt. 180.0) then
  my_lon_shifted = my_lon_shifted - 360.0
endif
fnamb = (30.0 - ((my_lon_shifted**2.0)/dlon + (my_lat+18.0)**2.0/dlat))/30.0
fnamb = min(fnamb,1.0)
fnamb = max(fnamb,0.0)

fhorizontal = fnep + fsep + fnamb

dx_old = dx
dx     = fhorizontal*DX_MIN + (1.0-fhorizontal)*DX_MAX     ! smoothly transition between 250m and 4km grid spacing.
dy     = dx

! adjust vertical velocity to preserve continuity
! w(:,:,:) = w(:,:,:) * dx_old / dx    !HP comments this out for debugging.

#endif

end subroutine bullseye_dx_change 
