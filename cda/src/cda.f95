PROGRAM cda
  USE kinds, ONLY: wp => dp
  USE cubes
  IMPLICIT NONE

  TYPE (cube) :: rho_a, rho_b, rho_ab, rho_ref, drho
  REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: cdz
  INTEGER :: i

  PRINT *, "============================================"
  PRINT *, "  Charge Displacement Analysis (CDA)"
  PRINT *, "============================================"
  PRINT *, ""
  PRINT *, "Reading cube files..."

  ! using the cube_get subroutine, pull the data from the cube files
  CALL cube_get(rho_a, "../test/CuCO+/a.cube")
  PRINT *, "  [OK] Read a.cube"
  
  CALL cube_get(rho_b, "../test/CuCO+/b.cube")
  PRINT *, "  [OK] Read b.cube"
  
  CALL cube_get(rho_ab, "../test/CuCO+/ab.cube")
  PRINT *, "  [OK] Read ab.cube"
  PRINT *, ""

  PRINT *, "Computing densities..."
  ! calculate charge displacement according to formulas
  rho_ref = rho_a + rho_b 
  PRINT *, "  [OK] Calculated reference density (rho_ref)"
  
  drho = rho_ab - rho_ref
  PRINT *, "  [OK] Calculated charge redistribution (drho)"
  PRINT *, ""

  PRINT *, "Computing CD function..."
  CALL cube_cdz(drho, cdz) 
  PRINT *, "  [OK] Calculated CD function along z-axis"
  PRINT *, ""

  PRINT *, "Writing output files..."
  
  ! Write cube files to output directory
  CALL cube_put(rho_a, "../output/a.cube")
  PRINT *, "  [OK] Written ../output/a.cube"
  
  CALL cube_put(rho_b, "../output/b.cube")
  PRINT *, "  [OK] Written ../output/b.cube"
  
  CALL cube_put(rho_ab, "../output/ab.cube")
  PRINT *, "  [OK] Written ../output/ab.cube"
  
  CALL cube_put(rho_ref, "../output/ref.cube")
  PRINT *, "  [OK] Written ../output/ref.cube"
  
  CALL cube_put(drho, "../output/drho.cube")
  PRINT *, "  [OK] Written ../output/drho.cube"
  
  ! Write CD function data
  OPEN(UNIT=20, FILE="../output/cd_function.dat", STATUS="replace")
  DO i = 1, SIZE(cdz)
    WRITE(20, '(F12.6, 2X, E16.8)') drho%z_min + (i-1)*drho%dz, cdz(i)
  END DO
  CLOSE(20)
  PRINT *, "  [OK] Written ../output/cd_function.dat"
  PRINT *, ""
  
  PRINT *, "============================================"
  PRINT *, "  CD Analysis completed successfully!"
  PRINT *, "============================================"
  PRINT *, ""
  PRINT *, "Output files in ../output/:"
  PRINT *, "  - a.cube           : Fragment A density"
  PRINT *, "  - b.cube           : Fragment B density"
  PRINT *, "  - ab.cube          : Molecular density"
  PRINT *, "  - ref.cube         : Reference density (A+B)"
  PRINT *, "  - drho.cube        : Charge redistribution"
  PRINT *, "  - cd_function.dat  : CD function vs z"
  PRINT *, ""

  ! delete the cubes from memory in case you want to start over
  CALL cube_del(rho_a)
  CALL cube_del(rho_b)
  CALL cube_del(rho_ab)
  CALL cube_del(rho_ref)
  CALL cube_del(drho)

END PROGRAM cda