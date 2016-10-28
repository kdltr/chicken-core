;;; environment settings for egg compilation


(foreign-declare #<<EOF
#ifndef STATICBUILD
# define STATIC_CHICKEN 0
#else
# define STATIC_CHICKEN 1
#endif
#ifndef DEBUGBUILD
# define DEBUG_CHICKEN 0
#else
# define DEBUG_CHICKEN 1
#endif
EOF
)

(define staticbuild (foreign-value "STATIC_CHICKEN" bool))
(define debugbuild (foreign-value "DEBUG_CHICKEN" bool))
(define cross-chicken (feature? #:cross-chicken))
(define binary-version (foreign-value "C_BINARY_VERSION" int))

(define default-cc (foreign-value "C_TARGET_CC" c-string))
(define default-cxx (foreign-value "C_TARGET_CXX" c-string))
(define default-cflags (foreign-value "C_TARGET_CFLAGS" c-string))
(define default-ldflags (foreign-value "C_TARGET_LDFLAGS" c-string))
(define default-libs (foreign-value "C_TARGET_MORE_LIBS" c-string))
(define default-slibs (foreign-value "C_TARGET_MORE_STATIC_LIBS" c-string))
(define default-libdir (foreign-value "C_TARGET_LIB_HOME" c-string))
(define default-slibdir (foreign-value "C_TARGET_STATIC_LIB_HOME" c-string))
(define default-incdir (foreign-value "C_TARGET_INCLUDE_HOME" c-string))

(define default-platform
  (if (eq? (software-type) 'windows)
      (foreign-value "C_WINDOWS_SHELL" bool)
      'unix))

(define default-prefix (foreign-value "C_INSTALL_PREFIX" c-string))
(define default-bindir (foreign-value "C_INSTALL_BIN_HOME" c-string))

(define default-csc 
  (string-append default-bindir "/" (foreign-value "C_CSC_PROGRAM" c-string)))

(define default-csi 
  (string-append default-bindir "/" (foreign-value "C_CSI_PROGRAM" c-string)))

(define default-builder 
  (make-pathname default-bindir (foreign-value "C_CHICKEN_DO_PROGRAM" c-string)))

(define host-repo (foreign-value "C_INSTALL_EGG_HOME" c-string))
(define host-bindir (foreign-value "C_INSTALL_BIN_HOME" c-string))
(define host-incdir (foreign-value "C_INSTALL_INCLUDE_HOME" c-string))
(define host-sharedir (foreign-value "C_INSTALL_SHARE_HOME" c-string))

(define target-repo
  (string-append default-libdir "/chicken/" (number->string binary-version)))

(define target-bindir (foreign-value "C_TARGET_BIN_HOME" c-string))
(define target-incdir (foreign-value "C_TARGET_INCLUDE_HOME" c-string))
(define target-sharedir (foreign-value "C_TARGET_SHARE_HOME" c-string))

(define +egg-info-extension+ ".egg-info") 

(define (destination-repository mode)
  (or (get-environment-variable "CHICKEN_REPOSITORY")
      (case mode
        ((target) target-repo)
        ((host) host-repo))))
