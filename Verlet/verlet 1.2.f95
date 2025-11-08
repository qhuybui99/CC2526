PROGRAM verlet_1.2
  IMPLICIT NONE
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300)
  
  ! Variables
  REAL (KIND=wp) :: x, y, z                ! positions
  REAL (KIND=wp) :: x_prev, y_prev, z_prev ! positions at previous step
  REAL (KIND=wp) :: vx, vy, vz            ! velocities
  REAL (KIND=wp) :: fx, fy, fz            ! forces
  REAL (KIND=wp) :: mass, tau, t_final
  REAL (KIND=wp) :: T_initial, T_final     ! kinetic energy
  REAL (KIND=wp) :: V_initial, V_final     ! potential energy
  REAL (KIND=wp) :: E_initial, E_final     ! total energy
  REAL (KIND=wp) :: delta_E                ! energy difference
  REAL (KIND=wp) :: delta_V                ! change in potential energy
  REAL (KIND=wp) :: kinetic_energy         ! function declaration
  INTEGER :: nsteps, i, iter
  
  ! Constants
  mass = 1.0_wp
  fx = 0.0_wp
  fy = 0.1_wp
  fz = 0.0_wp
  t_final = 120.0_wp  ! 2 minutes in seconds
  
  PRINT *, "Energy conservation test for various time steps"
  PRINT *, "Simulation time: 2 minutes"
  PRINT *, ""
  
  ! Test different time steps
  tau = 0.2_wp
  
  DO iter = 1, 6
    nsteps = INT(t_final / tau)
    
    ! Initial conditions
    x = 0.0_wp
    y = 0.0_wp
    z = 0.0_wp
    vx = 0.0_wp
    vy = 0.0_wp
    vz = 0.0_wp
    
    ! Store initial position
    x_prev = x
    y_prev = y
    z_prev = z
    
    ! Initial energy (set potential energy reference to zero)
    T_initial = kinetic_energy(mass, vx, vy, vz)
    V_initial = 0.0_wp
    E_initial = T_initial + V_initial
    
    ! Velocity Verlet loop
    DO i = 1, nsteps
      ! Store previous position
      x_prev = x
      y_prev = y
      z_prev = z
      
      ! Step 1: Update positions
      x = x + tau * vx + (tau**2 / (2.0_wp * mass)) * fx
      y = y + tau * vy + (tau**2 / (2.0_wp * mass)) * fy
      z = z + tau * vz + (tau**2 / (2.0_wp * mass)) * fz
      
      ! Step 2: Calculate change in potential energy
      ! delta_V = -f . delta_r (work done by force)
      delta_V = -(fx * (x - x_prev) + fy * (y - y_prev) + fz * (z - z_prev))
      V_initial = V_initial + delta_V
      
      ! Step 3: Update velocities (forces constant, so f_k+1 = f_k)
      vx = vx + (tau / (2.0_wp * mass)) * (fx + fx)
      vy = vy + (tau / (2.0_wp * mass)) * (fy + fy)
      vz = vz + (tau / (2.0_wp * mass)) * (fz + fz)
    ENDDO
    
    ! Final energy
    T_final = kinetic_energy(mass, vx, vy, vz)
    V_final = V_initial
    E_final = T_final + V_final
    
    ! Energy difference
    delta_E = E_final - E_initial
    
    ! Print results
    PRINT *, "Time step tau =", tau, "s (", nsteps, "steps)"
    PRINT *, "  Initial energy:", E_initial, "J"
    PRINT *, "  Final energy:  ", E_final, "J"
    PRINT *, "  Energy difference:", delta_E, "J"
    PRINT *, "  Final position: y =", y, "m"
    PRINT *, ""
    
    ! Halve the timestep for next test
    tau = tau / 2.0_wp
  ENDDO
  
END PROGRAM verlet_1.2

FUNCTION kinetic_energy(m, vx, vy, vz)
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300)
  REAL (KIND=wp) :: kinetic_energy
  REAL (KIND=wp), INTENT(IN) :: m, vx, vy, vz
  
  ! T = (1/2) * m * v^2
  kinetic_energy = 0.5_wp * m * (vx**2 + vy**2 + vz**2)
  
END FUNCTION kinetic_energy
