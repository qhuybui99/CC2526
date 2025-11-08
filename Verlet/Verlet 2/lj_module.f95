MODULE lj_module
  USE kinds, ONLY: wp => dp
  IMPLICIT NONE
  
  PUBLIC :: compute_forces, kinetic_energy
  
  CONTAINS
  
  ! Subroutine to compute forces on all particles
  SUBROUTINE compute_forces(x, f, n, sigma, epsilon)
    INTEGER, INTENT(IN) :: n
    REAL (KIND=wp), DIMENSION(:,:), INTENT(IN) :: x
    REAL (KIND=wp), DIMENSION(:,:), INTENT(OUT) :: f
    REAL (KIND=wp), INTENT(IN) :: sigma, epsilon
    
    INTEGER :: a, b
    REAL (KIND=wp) :: xab, yab, zab, rab, rab2
    REAL (KIND=wp) :: vlj_prime, fx, fy, fz
    
    ! Initialize forces to zero
    f = 0.0_wp
    
    ! Loop over all particle pairs
    DO a = 1, n
      DO b = 1, n
        IF (a /= b) THEN
          ! Compute distance components
          xab = x(a,1) - x(b,1)
          yab = x(a,2) - x(b,2)
          zab = x(a,3) - x(b,3)
          
          ! Compute distance
          rab2 = xab**2 + yab**2 + zab**2
          rab = SQRT(rab2)
          
          ! Compute derivative of LJ potential
          vlj_prime = lj_derivative(rab, sigma, epsilon)
          
          ! Compute force components (negative gradient)
          ! f^(a,x) = - sum_{b /= a} (x_ab/r_ab) * V'_LJ(r_ab)
          fx = -(xab / rab) * vlj_prime
          fy = -(yab / rab) * vlj_prime
          fz = -(zab / rab) * vlj_prime
          
          ! Accumulate forces
          f(a,1) = f(a,1) + fx
          f(a,2) = f(a,2) + fy
          f(a,3) = f(a,3) + fz
        ENDIF
      ENDDO
    ENDDO
    
  END SUBROUTINE compute_forces
  
  ! Function to compute the derivative of Lennard-Jones potential
  FUNCTION lj_derivative(r, sigma, epsilon)
    REAL (KIND=wp) :: lj_derivative
    REAL (KIND=wp), INTENT(IN) :: r, sigma, epsilon
    
    REAL (KIND=wp) :: sr6, sr12
    
    ! Compute (sigma/r)^6 and (sigma/r)^12
    sr6 = (sigma / r)**6
    sr12 = sr6**2
    
    ! V'_LJ(r) = 4*epsilon * [-12*sigma^12/r^13 + 6*sigma^6/r^7]
    lj_derivative = 4.0_wp * epsilon * (-12.0_wp * sr12 / r + 6.0_wp * sr6 / r)
    
  END FUNCTION lj_derivative
  
  ! Function to compute kinetic energy
  FUNCTION kinetic_energy(v, m, n)
    REAL (KIND=wp) :: kinetic_energy
    REAL (KIND=wp), DIMENSION(:,:), INTENT(IN) :: v
    REAL (KIND=wp), DIMENSION(:), INTENT(IN) :: m
    INTEGER, INTENT(IN) :: n
    
    INTEGER :: a
    REAL (KIND=wp) :: ke
    
    ke = 0.0_wp
    
    DO a = 1, n
      ke = ke + 0.5_wp * m(a) * (v(a,1)**2 + v(a,2)**2 + v(a,3)**2)
    ENDDO
    
    kinetic_energy = ke
    
  END FUNCTION kinetic_energy
  
END MODULE lj_module
