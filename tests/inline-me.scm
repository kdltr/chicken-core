(module
 inline-me
 (foreign-foo)
 (import scheme (chicken base))
 (import (only (chicken foreign) foreign-lambda*))

 (define foreign-foo (foreign-lambda* int ((int x)) "C_return ( x + 1 );"))

)
