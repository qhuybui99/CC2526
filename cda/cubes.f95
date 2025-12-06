MODULE cubes
  USE kinds, ONLY: wp => dp
  IMPLICIT NONE

  TYPE, PUBLIC :: cube
    CHARACTER (LEN=72) :: str1
    CHARACTER (LEN=72) :: str2
    REAL (KIND=wp) :: x_min, y_min, z_min, dx, dy, dz
    INTEGER :: n_x, n_y, n_z, N_atoms
    INTEGER, DIMENSION(:), POINTER :: zahl
    REAL (KIND=wp), DIMENSION(:), POINTER :: chrg, x, y, z
    REAL (KIND=wp), DIMENSION(:), POINTER :: array
    REAL (KIND=wp) :: dummy1, dummy2
  END TYPE cube

  PUBLIC :: cube_get, &
            cube_put, &
            cube_add, &
            cube_sub, &
            cube_unwrap, &
            cube_int, &
            cube_cdz, &
            cube_del

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE cube_add
  END INTERFACE
  INTERFACE OPERATOR(-)
    MODULE PROCEDURE cube_sub
  END INTERFACE
  
  CONTAINS

  ! get input from target .cube file 
  SUBROUTINE cube_get (mycube, infile)
    CHARACTER(LEN=*), INTENT(IN) :: infile
    TYPE (cube), INTENT(OUT) :: mycube
    INTEGER :: i
    
    OPEN (UNIT=11, FILE=infile, STATUS="old", ACTION="read")           
                                                                      
    READ (UNIT=11, FMT=*) mycube%str1
    READ (UNIT=11, FMT=*) mycube%str2
    READ (UNIT=11, FMT=*) mycube%N_atoms, mycube%x_min, mycube%y_min, mycube%z_min
    READ (UNIT=11, FMT=*) mycube%n_x, mycube%dx, mycube%dummy1, mycube%dummy2
    READ (UNIT=11, FMT=*) mycube%n_y, mycube%dummy1, mycube%dy, mycube%dummy2
    READ (UNIT=11, FMT=*) mycube%n_z, mycube%dummy1, mycube%dummy2, mycube%dz

    ! allocate to each pointer variable the presumed size (amount of atoms)
    ALLOCATE (mycube%zahl(mycube%N_atoms), mycube%chrg(mycube%N_atoms))
    ALLOCATE (mycube%x(mycube%N_atoms), mycube%y(mycube%N_atoms), mycube%z(mycube%N_atoms))
    ALLOCATE (mycube%array(mycube%n_x*mycube%n_y*mycube%n_z))          

    ! loop over the lines containing Z, a charge, and x/y/z coords
    DO i = 1, mycube%N_atoms
      READ(UNIT=11, FMT=*) mycube%zahl(i), mycube%chrg(i), mycube%x(i), mycube%y(i), mycube%z(i)
    END DO
                                     
    READ (UNIT=11, FMT=*) mycube%array

    CLOSE (11)                                                         
    
  END SUBROUTINE cube_get

  ! write cube data to output file
  SUBROUTINE cube_put (mycube, outfile)
    CHARACTER(LEN=*), INTENT(IN) :: outfile
    TYPE (cube), INTENT(IN) :: mycube
    INTEGER :: i, i_x, i_y, i_z, idx
    
    OPEN (UNIT=12, FILE=outfile, STATUS="replace", ACTION="write")
    
    ! Write header
    WRITE(12, '(A)') TRIM(mycube%str1)
    WRITE(12, '(A)') TRIM(mycube%str2)
    WRITE(12, '(I5, 3F12.6)') mycube%N_atoms, mycube%x_min, mycube%y_min, mycube%z_min
    WRITE(12, '(I5, 3F12.6)') mycube%n_x, mycube%dx, 0.0_wp, 0.0_wp
    WRITE(12, '(I5, 3F12.6)') mycube%n_y, 0.0_wp, mycube%dy, 0.0_wp
    WRITE(12, '(I5, 3F12.6)') mycube%n_z, 0.0_wp, 0.0_wp, mycube%dz
    
    ! Write atom information
    DO i = 1, mycube%N_atoms
      WRITE(12, '(I5, 4F12.6)') mycube%zahl(i), mycube%chrg(i), &
                                mycube%x(i), mycube%y(i), mycube%z(i)
    END DO
    
    ! Write density array
    idx = 0
    DO i_x = 1, mycube%n_x
      DO i_y = 1, mycube%n_y
        DO i_z = 1, mycube%n_z
          idx = idx + 1
          WRITE(12, '(E13.5)', ADVANCE='NO') mycube%array(idx)
          ! New line every 6 values (standard cube format)
          IF (MOD(i_z, 6) == 0) WRITE(12, *)
        END DO
        IF (MOD(mycube%n_z, 6) /= 0) WRITE(12, *)
      END DO
    END DO
    
    CLOSE(12)
  END SUBROUTINE cube_put

  ! operation of adding together two cubes
  FUNCTION cube_add (mycube1, mycube2)
    TYPE(cube) :: cube_add
    TYPE(cube), INTENT(IN) :: mycube1, mycube2

    ! since the two cubes being added need to be "compatible", we can just take the non-array part from either one
    cube_add%N_atoms = mycube1%N_atoms
    cube_add%x_min = mycube1%x_min
    cube_add%y_min = mycube1%y_min 
    cube_add%z_min = mycube1%z_min
    cube_add%n_x = mycube1%n_x
    cube_add%n_y = mycube1%n_y
    cube_add%n_z = mycube1%n_z
    cube_add%dx = mycube1%dx
    cube_add%dy = mycube1%dy 
    cube_add%dz = mycube1%dz

    ! allocate to each pointer variable the presumed size (amount of atoms)
    ALLOCATE (cube_add%zahl(cube_add%N_atoms), cube_add%chrg(cube_add%N_atoms))
    ALLOCATE (cube_add%x(cube_add%N_atoms), cube_add%y(cube_add%N_atoms), cube_add%z(cube_add%N_atoms))
    ALLOCATE (cube_add%array(cube_add%n_x*cube_add%n_y*cube_add%n_z))

    cube_add%zahl = mycube1%zahl
    cube_add%chrg = mycube1%chrg
    cube_add%x = mycube1%x
    cube_add%y = mycube1%y
    cube_add%z = mycube1%z
    cube_add%str1 = mycube1%str1
    cube_add%str2 = mycube1%str2

    ! the addition is just the addition of their array components
    cube_add%array = mycube1%array + mycube2%array   
  END FUNCTION cube_add

  ! operation of subtracting between two cubes
  FUNCTION cube_sub (mycube1, mycube2)
    TYPE(cube) :: cube_sub
    TYPE(cube), INTENT(IN) :: mycube1, mycube2

    ! since the two cubes being subtracted need to be "compatible", we can just take the non-array part from either one
    cube_sub%N_atoms = mycube1%N_atoms
    cube_sub%x_min = mycube1%x_min
    cube_sub%y_min = mycube1%y_min 
    cube_sub%z_min = mycube1%z_min
    cube_sub%n_x = mycube1%n_x
    cube_sub%n_y = mycube1%n_y
    cube_sub%n_z = mycube1%n_z
    cube_sub%dx = mycube1%dx
    cube_sub%dy = mycube1%dy 
    cube_sub%dz = mycube1%dz

    ! allocate to each pointer variable the presumed size (amount of atoms)
    ALLOCATE (cube_sub%zahl(cube_sub%N_atoms), cube_sub%chrg(cube_sub%N_atoms))
    ALLOCATE (cube_sub%x(cube_sub%N_atoms), cube_sub%y(cube_sub%N_atoms), cube_sub%z(cube_sub%N_atoms))
    ALLOCATE (cube_sub%array(cube_sub%n_x*cube_sub%n_y*cube_sub%n_z))

    cube_sub%zahl = mycube1%zahl
    cube_sub%chrg = mycube1%chrg
    cube_sub%x = mycube1%x
    cube_sub%y = mycube1%y
    cube_sub%z = mycube1%z
    cube_sub%str1 = mycube1%str1
    cube_sub%str2 = mycube1%str2

    ! the subtraction is just the subtraction of their array components
    cube_sub%array = mycube1%array - mycube2%array
  END FUNCTION cube_sub

  ! "unwrap" the cube from the single onedimensional array for all corodinates into a 3D array
  SUBROUTINE cube_unwrap (mycube, array3d)

    REAL (KIND=wp) :: cube_int
    TYPE (cube), INTENT(IN) :: mycube
    INTEGER :: i, i_x, i_y, i_z
    REAL (KIND = wp), DIMENSION(:,:,:), ALLOCATABLE :: array3d
    ALLOCATE(array3d(mycube%n_x, mycube%n_y, mycube%n_z))

    i = 1

    DO i_x=1, mycube%n_x
      DO i_y=1, mycube%n_y
        DO i_z=1, mycube%n_z
          array3d(i_x,i_y,i_z) = mycube%array(i)

          i = i + 1 
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE cube_unwrap 

  ! repeatedly integrate over x and y
  FUNCTION cube_int (mycube)
    REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: cube_int
    TYPE (cube), INTENT(IN) :: mycube
    INTEGER :: i_x, i_y, i_z
    REAL (KIND = wp), DIMENSION(:,:,:), ALLOCATABLE :: array3d

    ALLOCATE(cube_int(mycube%n_z))

    CALL cube_unwrap(mycube, array3d)

    DO i_z = 1, mycube%n_z
      cube_int(i_z) = 0.0_wp
        DO i_x = 1, mycube%n_x
          DO i_y = 1, mycube%n_y
            cube_int(i_z) = cube_int(i_z) + (array3d(i_x, i_y, i_z) * mycube%dx * mycube%dy)
          ENDDO
        ENDDO
    ENDDO
    
  END FUNCTION cube_int

  ! "destroy" the cube and deallocate the memory
  SUBROUTINE cube_del (mycube)
    TYPE (cube), INTENT(INOUT) :: mycube  
  
    IF (ASSOCIATED(mycube%zahl)) DEALLOCATE(mycube%zahl)
    IF (ASSOCIATED(mycube%chrg)) DEALLOCATE(mycube%chrg)
    IF (ASSOCIATED(mycube%x)) DEALLOCATE(mycube%x)
    IF (ASSOCIATED(mycube%y)) DEALLOCATE(mycube%y)
    IF (ASSOCIATED(mycube%z)) DEALLOCATE(mycube%z)
    IF (ASSOCIATED(mycube%array)) DEALLOCATE(mycube%array)
  END SUBROUTINE cube_del

  ! give the final charge displacement in z
  SUBROUTINE cube_cdz (mycube, cdz)
    TYPE (cube), INTENT(IN) :: mycube
    REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: integral, cdz
    INTEGER :: i, j

    integral = cube_int(mycube)
    ALLOCATE(cdz(mycube%n_z))

    DO i = 1, mycube%n_z
        cdz(i) = 0.0_wp
        DO j = 1, i
                cdz(i) = cdz(i) + (integral(j) * mycube%dz) 
        ENDDO
    ENDDO

  END SUBROUTINE cube_cdz

END MODULE cubes