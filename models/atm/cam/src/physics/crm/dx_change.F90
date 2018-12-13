! change horizontal resolution depending on the presence of deep convection
! between DX_MIN and DX_MAX
! coded by Marat Khairoutdinov, April 2018
! modified by HP and MSP during porting to SPCAM5. May 2018.

subroutine dx_change()

use vars
!use setparm_mod, only : setparm
implicit none

#ifdef ADAPTIVECRMDX

! locally defined parameters:
real, parameter :: wconv_min     = 1.  ! minimum vertical velocity to identify deep convection
real, parameter :: z_min         = 2000. ! minimum height to check for deep convection
real, parameter :: tau_toshallow = 1800.  ! relaxation time for returning from deep to shallow regime, set for L125 sims
real, parameter :: tau_todeep    = 3.*CRM_DT ! relaxation time to go to deep regime
real, parameter :: DX_MIN        = 250.0
real, parameter :: DX_MAX        = 4000.0

real     flag    ! on-off (0 or 1) flag for presence of deep convection
integer  i,j,k
real     dx_old

! check if there are any points above sigma > sigma_min with mass-flux exceeding 

flag = 0.
do k=1,nzm
  if ( z(k).gt.z_min ) then 
    do j=1,ny
     do i=1,nx
       if ( w(i,j,k)*dx.gt.wconv_min*DX_MIN .and. &
          qcl(i,j,k)+qci(i,j,k).gt.0.1e-3 .and. tabs(i,j,k).lt.273. ) then
             flag = 1.
             goto 111
       end if
     end do
    end do
  end if
end do

111 continue

dx_old = dx
dx     = dx - dt * ( (dx - DX_MIN) / tau_toshallow + flag * (dx - DX_MAX) / tau_todeep )
dy     = dx

!write (0,*) "changing dx. dx,dt,DX_MIN,DX_MAX,flag,tau_toshallow,tau_todeep = ", dx,dt,DX_MIN,DX_MAX,flag,tau_toshallow,tau_todeep 

! adjust vertical velocity to preserve continuity
w(:,:,:) = w(:,:,:) * dx_old / dx

#endif

end subroutine dx_change
