PROGRAM verlet
  IMPLICIT NONE
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300)
  
  ! Variables
  REAL (KIND=wp) :: x, y, z                ! positions
  REAL (KIND=wp) :: vx, vy, vz            ! velocities
  REAL (KIND=wp) :: fx, fy, fz            ! forces
  REAL (KIND=wp) :: mass, tau, t_final
  REAL (KIND=wp) :: x_old, y_old, z_old   ! previous iteration positions
  REAL (KIND=wp) :: diff                   ! difference in positions
  INTEGER :: nsteps, i, iter
  LOGICAL :: converged
  
  ! Constants
  mass = 1.0_wp
  fx = 0.0_wp
  fy = 0.1_wp
  fz = 0.0_wp
  t_final = 120.0_wp  ! 2 minutes in seconds
  
  ! Initial conditions
  x_old = 0.0_wp
  y_old = 0.0_wp
  z_old = 0.0_wp
  
  ! Start with tau = 0.2 and halve it
  tau = 0.2_wp
  converged = .FALSE.
  iter = 0
  
  DO WHILE (.NOT. converged .AND. iter < 20)
    iter = iter + 1
    nsteps = INT(t_final / tau)
    
    ! Reset initial conditions
    x = 0.0_wp
    y = 0.0_wp
    z = 0.0_wp
    vx = 0.0_wp
    vy = 0.0_wp
    vz = 0.0_wp
    
    ! Velocity Verlet loop
    DO i = 1, nsteps
      ! Update positions
      x = x + tau * vx + (tau**2 / (2.0_wp * mass)) * fx
      y = y + tau * vy + (tau**2 / (2.0_wp * mass)) * fy
      z = z + tau * vz + (tau**2 / (2.0_wp * mass)) * fz
      
      ! Forces remain constant (no need to recalculate)
      
      ! Update velocities
      vx = vx + (tau / (2.0_wp * mass)) * (fx + fx)
      vy = vy + (tau / (2.0_wp * mass)) * (fy + fy)
      vz = vz + (tau / (2.0_wp * mass)) * (fz + fz)
    ENDDO
    
    ! Check convergence (1 cm = 0.01 m)
    IF (iter > 1) THEN
      diff = SQRT((x - x_old)**2 + (y - y_old)**2 + (z - z_old)**2)
      PRINT *, "Iteration:", iter, "tau:", tau, "diff:", diff, "m"
      
      IF (diff < 0.01_wp) THEN
        converged = .TRUE.
        PRINT *, "Converged! Final tau:", tau
        PRINT *, "Final position: x=", x, "y=", y, "z=", z
      ENDIF
    ELSE
      PRINT *, "Iteration:", iter, "tau:", tau
      PRINT *, "Position: x=", x, "y=", y, "z=", z
    ENDIF
    
    ! Store current positions
    x_old = x
    y_old = y
    z_old = z
    
    ! Halve the timestep
    tau = tau / 2.0_wp
  ENDDO
  
END PROGRAM verlet
