PROGRAM verlet
  IMPLICIT NONE
  
  ! Declare variables
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(p=13, r=300)
  INTEGER :: k, nsteps
  REAL(KIND=wp) :: tau, mass
  REAL(KIND=wp) :: x, y, z
  REAL(KIND=wp) :: vx, vy, vz
  REAL(KIND=wp) :: fx, fy, fz
  REAL(KIND=wp) :: fx_new, fy_new, fz_new
  
  ! Initialize parameters
  nsteps = 10
  tau = 0.2_wp
  mass = 1.0_wp
  
  ! Constant force components
  fx = 0.0_wp
  fy = 0.1_wp
  fz = 0.0_wp
  
  ! Initial conditions: particle at rest at origin
  x = 0.0_wp
  y = 0.0_wp
  z = 0.0_wp
  vx = 0.0_wp
  vy = 0.0_wp
  vz = 0.0_wp
  
  ! Print initial position
  PRINT *, "Step 0:", x, y, z
  
  ! Velocity Verlet algorithm
  DO k = 1, nsteps
    ! Step 1: Calculate new positions
    x = x + tau * vx + (tau**2 / (2.0_wp * mass)) * fx
    y = y + tau * vy + (tau**2 / (2.0_wp * mass)) * fy
    z = z + tau * vz + (tau**2 / (2.0_wp * mass)) * fz
    
    ! Step 2: Evaluate forces at new position
    ! (forces are constant in this case)
    fx_new = fx
    fy_new = fy
    fz_new = fz
    
    ! Step 3: Calculate new velocities
    vx = vx + (tau / (2.0_wp * mass)) * (fx + fx_new)
    vy = vy + (tau / (2.0_wp * mass)) * (fy + fy_new)
    vz = vz + (tau / (2.0_wp * mass)) * (fz + fz_new)
    
    ! Print current position
    PRINT *, "Step", k, ":", x, y, z
  ENDDO
  
END PROGRAM verlet
