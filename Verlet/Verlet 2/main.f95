PROGRAM main
  USE kinds, ONLY: wp => dp
  USE lj_module
  IMPLICIT NONE
  
  ! Variables
  INTEGER :: n, nk, k, a
  REAL (KIND=wp) :: tau, sigma, epsilon
  REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: m
  REAL (KIND=wp), DIMENSION(:,:), ALLOCATABLE :: x, v, f, fnext
  REAL (KIND=wp) :: time
  REAL (KIND=wp) :: bohr_to_angstrom
  
  ! Conversion factor: 1 Bohr = 0.529177 Angstrom
  bohr_to_angstrom = 0.529177_wp
  
  ! Read input file
  OPEN (UNIT=10, FILE="input.dat", STATUS="old", ACTION="read")
  
  ! Read number of steps and time step
  READ (UNIT=10, FMT=*) nk, tau
  
  ! Read LJ parameters
  READ (UNIT=10, FMT=*) sigma, epsilon
  
  ! Read number of atoms
  READ (UNIT=10, FMT=*) n
  
  ! Allocate arrays
  ALLOCATE ( m(n) )
  ALLOCATE ( x(n,3), v(n,3), f(n,3), fnext(n,3) )
  
  ! Read masses, positions, and velocities
  DO a = 1, n
    READ (UNIT=10, FMT=*) m(a), x(a,1), x(a,2), x(a,3), v(a,1), v(a,2), v(a,3)
  ENDDO
  
  CLOSE (10)
  
  ! Print initial configuration
  PRINT *, "Velocity Verlet simulation with Lennard-Jones potential"
  PRINT *, "Number of atoms:", n
  PRINT *, "Number of steps:", nk
  PRINT *, "Time step (a.u.):", tau
  PRINT *, "Sigma (Bohr):", sigma
  PRINT *, "Epsilon (Hartree):", epsilon
  PRINT *, ""
  
  ! Open output trajectory file in XYZ format
  OPEN (UNIT=20, FILE="trajectory.xyz", STATUS="replace", ACTION="write")
  
  ! Compute initial forces
  CALL compute_forces(x, f, n, sigma, epsilon)
  
  ! Initialize time
  time = 0.0_wp
  
  ! Write initial configuration
  WRITE (UNIT=20, FMT=*) n
  WRITE (UNIT=20, FMT=*) time
  DO a = 1, n
    WRITE (UNIT=20, FMT='(A,3F16.8)') "Ne", x(a,1)*bohr_to_angstrom, &
                                            x(a,2)*bohr_to_angstrom, &
                                            x(a,3)*bohr_to_angstrom
  ENDDO
  
  ! Velocity Verlet algorithm
  DO k = 1, nk
    ! Step 1: Update positions using array syntax
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
    
    ! Write current configuration to trajectory file
    WRITE (UNIT=20, FMT=*) n
    WRITE (UNIT=20, FMT=*) time
    DO a = 1, n
      WRITE (UNIT=20, FMT='(A,3F16.8)') "Ne", x(a,1)*bohr_to_angstrom, &
                                              x(a,2)*bohr_to_angstrom, &
                                              x(a,3)*bohr_to_angstrom
    ENDDO
    
    ! Optional: Print progress every 100 steps
    IF (MOD(k, 100) == 0) THEN
      PRINT *, "Step", k, "of", nk, "| Time:", time, "a.u."
    ENDIF
  ENDDO
  
  CLOSE (20)
  
  PRINT *, ""
  PRINT *, "Simulation complete!"
  PRINT *, "Trajectory written to: trajectory.xyz"
  PRINT *, "Visualize with: vmd trajectory.xyz"
  
  ! Deallocate arrays
  DEALLOCATE ( m, x, v, f, fnext )
  
END PROGRAM main
