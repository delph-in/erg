(in-package :lkb)

(defun bool-value-true (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type '+))))
  
(defun bool-value-false (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type '-))))

(defun make-well-typed (mrs)
  (loop
      for ep in (mrs:psoa-liszt mrs)
      do
        (loop
            for role in (mrs:rel-flist ep)
            for variable = (mrs:fvpair-value role)
            when (mrs:var-p variable)
            do
              (loop
                  with unifications
                  for property in (mrs:var-extra variable)
                  for path = (create-path-from-feature-list
                              (list (mrs::extrapair-feature property)))
                  for type = (make-u-value
                              :type (mrs::extrapair-value property))
                  for unification = (make-unification :lhs path :rhs type)
                  do (pprint (push unification unifications))
                  finally
                    (let* ((*standard-output* (make-string-output-stream))
                           (dag (process-unifications unifications t t)))
                      (when dag
                        (setf (mrs:var-type variable)
                          (dag-type dag))))))))

                    