;;;; irregex.import.scm - import library for "regex" module (irregex API)
;
; Copyright (c) 2008-2013, The Chicken Team
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.


(##sys#register-primitive-module
 'irregex
 '(irregex 
   irregex-apply-match
   irregex-dfa 
   irregex-dfa/extract
   irregex-dfa/search
   irregex-extract
   irregex-flags
   irregex-fold
   irregex-fold/chunked
   irregex-lengths
   irregex-match
   irregex-match?
   irregex-match-data?
   irregex-match-end-chunk
   irregex-match-end-index
   irregex-match-names
   irregex-match-num-submatches
   irregex-match-start-chunk
   irregex-match-start-index
   irregex-match-string
   irregex-match-subchunk
   irregex-match-substring
   irregex-match-valid-index?
   irregex-match/chunked
   irregex-names
   irregex-new-matches
   irregex-nfa
   irregex-num-submatches
   irregex-opt
   irregex-quote
   irregex-replace
   irregex-replace/all
   irregex-reset-matches!
   irregex-search
   irregex-search/chunked
   irregex-search/matches
   irregex-split
   irregex?
   make-irregex-chunker
   maybe-string->sre
   sre->irregex
   sre->string
   string->irregex 
   string->sre
   ))
