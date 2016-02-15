;; both arms of if branches are dropped

(let ((a "yep")) (if (string? a) 'ok 'no))
(let ((a 'nope)) (if (string? a) 'ok 'no))
