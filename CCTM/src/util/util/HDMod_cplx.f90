!*************************************************************************
!* Hyperdual Number Module (HDMod_cplx) of Fortran Codes 
!*----------------------------------------------------------------

Module HDMod_cplx

  use HDMod
  implicit none
  
  public
  
      integer, parameter                   :: SP_cplx = KIND(1.0)      ! Single Precision
      integer, parameter                   :: DP_cplx = KIND(1.d0)     ! Double Precision
      integer, parameter                   :: PRhyd_cplx = DP_cplx             ! Set the precision
      real(PRhyd_cplx), parameter, private    :: pi = ACOS(-1.0_PRhyd)  
      complex(PRhyd_cplx), parameter, private :: pi_c = (pi, 0.0_PRhyd)
  
      !--- DEFINITION OF HYPERDUAL TYPE
      TYPE hyperdual_cplx
          complex(PRhyd) :: x
          complex(PRhyd) :: dx1
          complex(PRhyd) :: dx2
          complex(PRhyd) :: dx1x2
      END TYPE
  
      !================================================================!
      !                Overloading hdual functions                     !
      !================================================================!
        
      !----- Constructor
      interface hdual_cplx
        module procedure hdual_cplx_from_dble
      end interface
  
      interface assignment(=) 
        module procedure hdual_cplx_assign_hdual_cplx, hdual_cplx_assign_cplx
      end interface
      
      interface operator(+)
        module procedure hdual_cplx_plus_hdual_cplx, hdual_cplx_plus_dble, dble_plus_hdual_cplx, &
          hdual_cplx_plus_hdual
      end interface
        
      interface operator(-)
        module procedure hdual_cplx_minus_hdual_cplx, hdual_cplx_minus_dble, dble_minus_hdual_cplx, &
          minus_hdual_cplx, hdual_cplx_minus_hdual
      end interface
      
      interface operator(*)
        module procedure hdual_cplx_mul_hdual_cplx, hdual_cplx_mul_dble, dble_mul_hdual_cplx, &
          hdual_mul_hdual_cplx, hdual_cplx_mul_hdual
      end interface  
  
      interface operator(/) 
        module procedure hdual_cplx_div_hdual_cplx, hdual_cplx_div_dble, dble_div_hdual_cplx
      end interface 

      interface operator(**)
        module procedure hdual_cplx_pow_dble
      end interface
      
      interface ABS
        module procedure hdual_cplx_abs
      end interface

      interface COS
        module procedure hdual_cplx_cos
      end interface

      interface SIN
        module procedure hdual_cplx_sin
      end interface
  
      interface CMPLX
        module procedure hdual_cplx_from_hdual, hdual_cplx_tens_from_hdual_tens
      end interface
      
      interface REAL
        module procedure real_from_hdual_cplx
      end interface 
      
      interface AIMAG
        module procedure aimag_from_hdual_cplx
      end interface AIMAG
  
      interface IMAG 
        module procedure imag_from_hdual_cplx
      end interface IMAG
  
      interface CONJG
        module procedure conjg_from_hdual_cplx
      end interface
  
  
      CONTAINS
  
  
      function hdual_cplx_from_dble(x11,x12,x21,x22) result(q)
            
        implicit none
        complex(PRhyd_cplx), intent(in)  :: x11, x12, x21, x22
        TYPE(hyperdual_cplx)       :: q
        
        q%x = x11
        q%dx1 = x12
        q%dx2 = x21
        q%dx1x2 = x22
              
      end function hdual_cplx_from_dble
  
  
      subroutine hdual_cplx_assign_hdual_cplx(qcplx_left, qcplx_right)
  
        implicit none
        TYPE(hyperdual_cplx), intent(out)  :: qcplx_left
        TYPE(hyperdual_cplx), intent(in)   :: qcplx_right
  
        qcplx_left%x = qcplx_right%x
        qcplx_left%dx1 = qcplx_right%dx1
        qcplx_left%dx2 = qcplx_right%dx2
        qcplx_left%dx1x2 = qcplx_right%dx1x2
  
      end subroutine hdual_cplx_assign_hdual_cplx


      subroutine hdual_cplx_assign_cplx(qcplx_left, cplx_right)

        implicit none
        complex(PRhyd_cplx), intent(in)    :: cplx_right
        TYPE(hyperdual_cplx), intent(out) :: qcplx_left

        qcplx_left%x = cplx_right
        qcplx_left%dx1 = 0.0d0
        qcplx_left%dx2 = 0.0d0
        qcplx_left%dx1x2 = 0.0d0

      end subroutine hdual_cplx_assign_cplx
      
  
      function hdual_cplx_plus_hdual_cplx(qleft, qright) result(res)
          
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qleft, qright
        TYPE(hyperdual_cplx) :: res
              
        res%x = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2
                  
      end function hdual_cplx_plus_hdual_cplx
      
  
      function hdual_cplx_plus_dble(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual_cplx), intent(in) :: qleft
          real(PRhyd_cplx), intent(in)        :: iright
          TYPE(hyperdual_cplx)             :: res
                
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                    
      end function hdual_cplx_plus_dble	
  
  
      function dble_plus_hdual_cplx(ileft, qright) result(res)
        
        implicit none      
        TYPE(hyperdual_cplx), intent(in) :: qright
        real(PRhyd_cplx), intent(in)        :: ileft
        TYPE(hyperdual_cplx)             :: res
                
          res%x  = qright%x + ileft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2 
      
      end function dble_plus_hdual_cplx


      function hdual_cplx_plus_hdual(qcplx, qright) result(res)

        implicit none
        TYPE(hyperdual_cplx), intent(in)  :: qcplx
        TYPE(hyperdual), intent(in)       :: qright
        TYPE(hyperdual_cplx)              :: res

        res%x = qcplx%x + qright%x 
        res%dx1 = qcplx%dx1 + qright%dx1
        res%dx2 = qcplx%dx2 + qright%dx2
        res%dx1x2 = qcplx%dx1x2 + qright%dx1x2

      end function hdual_cplx_plus_hdual
       

      
      function hdual_cplx_minus_hdual_cplx(qleft, qright) result(res)
          
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qleft, qright
        TYPE(hyperdual_cplx) :: res
              
        res%x = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2
                  
      end function hdual_cplx_minus_hdual_cplx
  
  
      function hdual_cplx_minus_dble(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual_cplx), intent(in) :: qleft
          real(PRhyd_cplx), intent(in)        :: iright
          TYPE(hyperdual_cplx)             :: res
                
          res%x  = qleft%x - iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                    
      end function hdual_cplx_minus_dble	
  
  
      function dble_minus_hdual_cplx(ileft, qright) result(res)
  
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qright
        real(PRhyd_cplx), intent(in)        :: ileft
        TYPE(hyperdual_cplx)             :: res
  
        res%x = ileft - qright%x
        res%dx1 = - qright%dx1
        res%dx2 = - qright%dx2
        res%dx1x2 = - qright%dx1x2
       
      end function dble_minus_hdual_cplx
  
  
      function minus_hdual_cplx(qright) result(res)
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qright
        TYPE(hyperdual_cplx)             :: res
      
        res%x     = - qright%x 
        res%dx1   = - qright%dx1
        res%dx2   = - qright%dx2
        res%dx1x2 = - qright%dx1x2
      
      end function minus_hdual_cplx
      

      function hdual_cplx_minus_hdual(qcplx, qright) result(res)

        implicit none
        TYPE(hyperdual_cplx), intent(in)  :: qcplx
        TYPE(hyperdual), intent(in)       :: qright
        TYPE(hyperdual_cplx)              :: res

        res%x = qcplx%x - qright%x 
        res%dx1 = qcplx%dx1 - qright%dx1
        res%dx2 = qcplx%dx2 - qright%dx2
        res%dx1x2 = qcplx%dx1x2 - qright%dx1x2

      end function hdual_cplx_minus_hdual

  
      function hdual_cplx_mul_hdual_cplx(qleft, qright) result(res)
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qleft, qright
        TYPE(hyperdual_cplx)             :: res
          
        res%x = qleft%x * qright%x
        res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
        res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
        res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
      
      end function hdual_cplx_mul_hdual_cplx
  
  
      function hdual_cplx_mul_dble(qleft, iright) result(res)
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qleft
        real(PRhyd_cplx), intent(in)        :: iright
        TYPE(hyperdual_cplx)             :: res
      
        res%x = qleft%x * iright
        res%dx1 = qleft%dx1 * iright
        res%dx2 = qleft%dx2 * iright
        res%dx1x2 = qleft%dx1x2 * iright
      
      end function hdual_cplx_mul_dble
  
  
      function dble_mul_hdual_cplx(ileft, qright) result(res)
  
        implicit none
        TYPE(hyperdual_cplx), intent(in)  :: qright
        real(PRhyd_cplx), intent(in)         :: ileft
        TYPE(hyperdual_cplx)              :: res
  
          res%x = qright%x * ileft
          res%dx1 = qright%dx1 * ileft
          res%dx2 = qright%dx2 * ileft
          res%dx1x2 = qright%dx1x2 * ileft
  
      end function dble_mul_hdual_cplx


      function hdual_mul_hdual_cplx(qleft, qcplx) result(res)

        ! Hyperdual multiplied by hyperdual complex
        ! FOR subroutine BHCOAT in twoway_rrtmg_aero_optical.F

        implicit none
        TYPE(hyperdual), intent(in)       :: qleft
        TYPE(hyperdual_cplx), intent(in)  :: qcplx
        TYPE(hyperdual_cplx)              :: res

        res%x = qleft%x * qcplx%x
        res%dx1 = qleft%x * qcplx%dx1 + qleft%dx1 * qcplx%x
        res%dx2 = qleft%x * qcplx%dx2 + qleft%dx2 * qcplx%x
        res%dx1x2 = qleft%x * qcplx%dx1x2 + qleft%dx1 * qcplx%dx2 + qleft%dx2 * qcplx%dx1 + qleft%dx1x2 * qcplx%x


      end function hdual_mul_hdual_cplx


      function hdual_cplx_mul_hdual(qcplx, qright) result(res)

        ! Hyperdual multiplied by hyperdual complex
        ! FOR subroutine BHCOAT in twoway_rrtmg_aero_optical.F

        implicit none
        TYPE(hyperdual), intent(in)       :: qright
        TYPE(hyperdual_cplx), intent(in)  :: qcplx
        TYPE(hyperdual_cplx)              :: res

        res%x = qright%x * qcplx%x
        res%dx1 = qright%x * qcplx%dx1 + qright%dx1 * qcplx%x
        res%dx2 = qright%x * qcplx%dx2 + qright%dx2 * qcplx%x
        res%dx1x2 = qright%x * qcplx%dx1x2 + qright%dx1 * qcplx%dx2 + qright%dx2 * qcplx%dx1 + qright%dx1x2 * qcplx%x


      end function hdual_cplx_mul_hdual
      ! DIVISION !
  
  
      function hdual_cplx_div_hdual_cplx(qleft, qright) result(res)
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qleft, qright
  
        TYPE(hyperdual_cplx)             :: inv
        TYPE(hyperdual_cplx)             :: res
  
        inv = hdual_cplx_pow_dble(qright, -1.0d0)
        res = qleft * inv
  
      end function hdual_cplx_div_hdual_cplx
  
  
      function hdual_cplx_div_dble(qleft, iright) result(res)
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qleft
        real(PRhyd_cplx), intent(in)        :: iright
        TYPE(hyperdual_cplx)             :: res
        
        res%x = qleft%x / iright
        res%dx1 = qleft%dx1 / iright
        res%dx2 = qleft%dx2 / iright
        res%dx1x2 = qleft%dx1x2 / iright
  
      end function hdual_cplx_div_dble
  
  
      function dble_div_hdual_cplx(ileft, qright) result(res)
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qright
        real(PRhyd_cplx), intent(in)        :: ileft
        TYPE(hyperdual_cplx)             :: inv
        TYPE(hyperdual_cplx)             :: res
        
        inv = hdual_cplx_pow_dble(qright, -1.0d0)
        res = ileft * inv
        
      end function dble_div_hdual_cplx
  
  
      function hdual_cplx_pow_dble(qleft, iright) result(res)
         
          implicit none
          TYPE(hyperdual_cplx), intent(in)    :: qleft
          real(PRhyd_cplx), intent(in)           :: iright
          complex(PRhyd_cplx)                    :: xval
          complex(PRhyd_cplx)                    :: tol
          complex(PRhyd_cplx)                    :: deriv
          TYPE(hyperdual_cplx)                :: res
  
          xval = qleft%x 
          tol = 1.0e-15
          if (abs(real(xval)) < real(tol)) then
            
            if (real(xval) >= 0) then 
              xval = tol
            endif 
  
            if (real(xval) < 0) then
              xval = -tol
            endif
  
          endif 
          deriv = iright * xval**(iright - 1.0d0)
          
          res%x = qleft%x**iright
          res%dx1 = qleft%dx1 * deriv
          res%dx2 = qleft%dx2 * deriv
          res%dx1x2 = qleft%dx1x2 * deriv + iright * (iright - 1.0d0) * qleft%dx1 * qleft%dx2 * xval**(iright - 2.0d0)
        
        end function hdual_cplx_pow_dble
  
      
      function hdual_cplx_abs(qcplx) result(res)

        ! using abs on hyperdual complex
        implicit none
        TYPE(hyperdual_cplx), intent(in)   :: qcplx
        TYPE(hyperdual)               :: res

        ! Like applying abs on complex(8), the result 
        ! should be a hyperdual number
        res%x = abs(qcplx%x)
        res%dx1 = abs(qcplx%dx1)
        res%dx2 = abs(qcplx%dx2)
        res%dx1x2 = abs(qcplx%dx1x2)
      
      end function hdual_cplx_abs


      function hdual_cplx_cos(qcplx) result(res)
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qcplx
        complex(PRhyd_cplx)                 :: funval, deriv
        TYPE(hyperdual_cplx)             :: res

        funval = cos(qcplx%x)
        deriv  = -sin(qcplx%x)

        res%x = funval
        res%dx1 = deriv * qcplx%dx1
        res%dx2 = deriv * qcplx%dx2
        res%dx1x2 = -funval * qcplx%dx1 * qcplx%dx2 + deriv * qcplx%dx1x2

      end function hdual_cplx_cos


      function hdual_cplx_sin(qcplx) result(res)
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qcplx
        complex(PRhyd_cplx)                 :: funval, deriv
        TYPE(hyperdual_cplx)             :: res

        funval = sin(qcplx%x)
        deriv  = cos(qcplx%x)

        res%x = funval
        res%dx1 = deriv * qcplx%dx1
        res%dx2 = deriv * qcplx%dx2
        res%dx1x2 = -funval * qcplx%dx1 * qcplx%dx2 + deriv * qcplx%dx1x2

      end function hdual_cplx_sin
        

      function hdual_cplx_from_hdual(qreal, qimag) result(qcplx)
  
        ! Complexify hyperdual numbers using CMPLX
  
        implicit none
        TYPE(hyperdual), intent(in) :: qreal
        TYPE(hyperdual), intent(in) :: qimag
        TYPE(hyperdual_cplx)        :: qcplx
  
        qcplx%x = CMPLX(qreal%x, qimag%x, KIND=PRhyd_cplx) 
        qcplx%dx1 = CMPLX(qreal%dx1, qimag%dx1, KIND=PRhyd_cplx)
        qcplx%dx2 = CMPLX(qreal%dx2, qimag%dx2, KIND=PRhyd_cplx)
        qcplx%dx1x2 = CMPLX(qreal%dx1x2, qimag%dx1x2, KIND=PRhyd_cplx)
  
      end function hdual_cplx_from_hdual
  
  
      function hdual_cplx_tens_from_hdual_tens(qreal, qimag) result(qcplx)
  
        ! Complexify hyperdual numbers using CMPLX
  
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qreal
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qimag
        TYPE(hyperdual_cplx), dimension(size(qreal, 1), size(qreal, 2), size(qreal, 3)) :: qcplx
  
        qcplx%x = CMPLX(qreal%x, qimag%x, KIND=PRhyd_cplx) 
        qcplx%dx1 = CMPLX(qreal%dx1, qimag%dx1, KIND=PRhyd_cplx)
        qcplx%dx2 = CMPLX(qreal%dx2, qimag%dx2, KIND=PRhyd_cplx)
        qcplx%dx1x2 = CMPLX(qreal%dx1x2, qimag%dx1x2, KIND=PRhyd_cplx)
  
      end function hdual_cplx_tens_from_hdual_tens
      
      
      function real_from_hdual_cplx(qcplx) result(qreal)
         
        ! Extract the real component from hyperdual_cplx
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qcplx
        TYPE(hyperdual)                  :: qreal
        
        qreal%x = real(qcplx%x)
        qreal%dx1 = real(qcplx%dx1)
        qreal%dx2 = real(qcplx%dx2)
        qreal%dx1x2 = real(qcplx%dx1x2)
        
      end function real_from_hdual_cplx
      
      
      function aimag_from_hdual_cplx(qcplx) result(qimag)
         
        ! Extract the real component from hyperdual_cplx
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qcplx
        TYPE(hyperdual)                  :: qimag
        
        qimag%x = aimag(qcplx%x)
        qimag%dx1 = aimag(qcplx%dx1)
        qimag%dx2 = aimag(qcplx%dx2)
        qimag%dx1x2 = aimag(qcplx%dx1x2)
        
      end function aimag_From_hdual_cplx
  
  
      function imag_from_hdual_cplx(qcplx) result(qimag)
         
        ! Extract the real component from hyperdual_cplx
        
        implicit none
        TYPE(hyperdual_cplx), intent(in) :: qcplx
        TYPE(hyperdual)                  :: qimag
        
        qimag%x = imag(qcplx%x)
        qimag%dx1 = imag(qcplx%dx1)
        qimag%dx2 = imag(qcplx%dx2)
        qimag%dx1x2 = imag(qcplx%dx1x2)
        
      end function imag_From_hdual_cplx
      
  
      function conjg_from_hdual_cplx(qcplx) result(qconjg)
        TYPE(hyperdual_cplx), intent(in)  :: qcplx
        TYPE(hyperdual_cplx)              :: qconjg
  
        qconjg%x = conjg(qcplx%x)
        qconjg%dx1 = conjg(qcplx%dx1)
        qconjg%dx2 = conjg(qcplx%dx2)
        qconjg%dx1x2 = conjg(qcplx%dx1x2)
  
      end function conjg_from_hdual_cplx


  
  
  end module HDMod_cplx