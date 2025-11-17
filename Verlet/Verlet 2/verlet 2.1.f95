PROGRAM verlet_2.1
  USE kinds, ONLY: wp => dp
  USE lj_module
  IMPLICIT NONE
  
  ! Variables
  INTEGER :: n, nk, k, a, iter
  REAL (KIND=wp) :: tau, tau_init, sigma, epsilon
  REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: m
  REAL (KIND=wp), DIMENSION(:,:), ALLOCATABLE :: x, x_init, v, v_init, f, fnext
  REAL (KIND=wp), DIMENSION(:,:), ALLOCATABLE :: x_prev
  REAL (KIND=wp) :: time, total_time
  REAL (KIND=wp) :: max_diff, tolerance
  LOGICAL :: converged
  INTEGER :: max_iterations
  
  ! Convergence parameters
  tolerance = 1.0e-8_wp  ! Convergence criterion (Bohr)
  max_iterations = 20     ! Maximum number of refinements
  
  ! Read input file
  OPEN (UNIT=10, FILE="input.dat", STATUS="old", ACTION="read")
  
  ! Read number of steps and initial time step
  READ (UNIT=10, FMT=*) nk, tau_init
  
  ! Read LJ parameters
  READ (UNIT=10, FMT=*) sigma, epsilon
  
  ! Read number of atoms
  READ (UNIT=10, FMT=*) n
  
  ! Allocate arrays
  ALLOCATE ( m(n) )
  ALLOCATE ( x(n,3), x_init(n,3), v(n,3), v_init(n,3), f(n,3), fnext(n,3) )
  ALLOCATE ( x_prev(n,3) )
  
  ! Read masses, positions, and velocities
  DO a = 1, n
    READ (UNIT=10, FMT=*) m(a), x_init(a,1), x_init(a,2), x_init(a,3), &
                                 v_init(a,1), v_init(a,2), v_init(a,3)
  ENDDO
  
  CLOSE (10)
  
  ! Calculate total simulation time
  total_time = REAL(nk, wp) * tau_init
  
  ! Print header
  PRINT *, "==========================================================="
  PRINT *, "  CONVERGENCE STUDY: Velocity Verlet with LJ Potential"
  PRINT *, "==========================================================="
  PRINT *, "Number of atoms:", n
  PRINT *, "Total simulation time (a.u.):", total_time
  PRINT *, "Initial time step (a.u.):", tau_init
  PRINT *, "Convergence tolerance (Bohr):", tolerance
  PRINT *, "Sigma (Bohr):", sigma
  PRINT *, "Epsilon (Hartree):", epsilon
  PRINT *, ""
  PRINT *, "Iter  Time_Step    Num_Steps   Max_Diff      Converged"
  PRINT *, "-----------------------------------------------------------"
  
  ! Initialize for convergence loop
  converged = .FALSE.
  tau = tau_init
  x_prev = 0.0_wp
  
  ! Convergence loop: halve timestep at each iteration
  DO iter = 1, max_iterations
    
    ! Calculate number of steps for this timestep
    nk = NINT(total_time / tau)
    
    ! Reset to initial conditions
    x = x_init
    v = v_init
    time = 0.0_wp
    
    ! Compute initial forces
    CALL compute_forces(x, f, n, sigma, epsilon)
    
    ! Velocity Verlet algorithm
    DO k = 1, nk
      ! Step 1: Update positions
      DO a = 1, n
        x(a,:) = x(a,:) + tau * v(a,:) + (tau**2 / (2.0_wp * m(a))) * f(a,:)
      ENDDO
      
      ! Step 2: Evaluate forces at new positions
      CALL compute_forces(x, fnext, n, sigma, epsilon)
      
      ! Step 3: Update velocities
      DO a = 1, n
        v(a,:) = v(a,:) + (tau / (2.0_wp * m(a))) * (f(a,:) + fnext(a,:))
      ENDDO
      
      ! Step 4: Update forces for next iteration
      f = fnext
      
      ! Update time
      time = time + tau
    ENDDO
    
    ! Check convergence (only after first iteration)
    IF (iter > 1) THEN
      max_diff = 0.0_wp
      DO a = 1, n
        DO k = 1, 3
          max_diff = MAX(max_diff, ABS(x(a,k) - x_prev(a,k)))
        ENDDO
      ENDDO
      
      ! Check if converged
      IF (max_diff < tolerance) THEN
        converged = .TRUE.
      ENDIF
      
      ! Print results for this iteration
      PRINT '(I4,2X,E12.5,2X,I8,2X,E12.5,2X,L1)', &
            iter, tau, nk, max_diff, converged
      
      ! Exit if converged
      IF (converged) THEN
        PRINT *, ""
        PRINT *, "*** CONVERGENCE ACHIEVED ***"
        PRINT *, "Converged time step (a.u.):", tau
        PRINT *, "Number of steps required:", nk
        PRINT *, "Maximum position difference (Bohr):", max_diff
        EXIT
      ENDIF
      
    ELSE
      ! First iteration: no comparison yet
      PRINT '(I4,2X,E12.5,2X,I8,2X,A12,2X,A1)', &
            iter, tau, nk, "N/A", "-"
    ENDIF
    
    ! Save current final positions for next comparison
    x_prev = x
    
    ! Halve the timestep for next iteration
    tau = tau / 2.0_wp
    
  ENDDO
  
  ! Check if convergence was not achieved
  IF (.NOT. converged) THEN
    PRINT *, ""
    PRINT *, "*** WARNING: Convergence not achieved within", max_iterations, "iterations ***"
    PRINT *, "Consider increasing max_iterations or relaxing tolerance"
  ENDIF
  
  PRINT *, ""
  PRINT *, "==========================================================="
  PRINT *, "Final positions (Bohr):"
  PRINT *, "-----------------------------------------------------------"
  DO a = 1, n
    PRINT '(A,I3,A,3F14.8)', "Atom ", a, ": ", x(a,1), x(a,2), x(a,3)
  ENDDO
  PRINT *, "==========================================================="
  
  ! Deallocate arrays
  DEALLOCATE ( m, x, x_init, v, v_init, f, fnext, x_prev )
  
END PROGRAM verlet_2.1
