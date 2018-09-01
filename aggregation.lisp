I found my ppcre parse trees were a bottleneck when matching many terms.
I think this code might be of value to ppcre developers.
|#

(defun aggregate (terms &key case-insensitive)
  (let ((table (make-hash-table :test #'equal))
        (normaliser (if case-insensitive #'string-downcase #'identity))
        (sortfunc (if case-insensitive #'string-lessp #'string<)))
    (loop for term in (sort terms sortfunc)
          as norm = (funcall normaliser term)
          as key = (or (loop for end from (length norm) downto 0
                             as key = (subseq norm 0 end)
                             if (gethash key table) return key)
                       norm) do
          ;;(format t "term ~s key ~s~%" term key)
          (setf (gethash key table) (merge 'list
                                           (list (subseq norm (length key)))
                                           (gethash key table) #'string-lessp)))
    (sort (loop for suffixes being the hash-values in table using (hash-key prefix)
                collect (list prefix suffixes))
          sortfunc
          :key #'first)))
          
 ;; (aggregate '("worker" "work" "worked" "read" "release" "redeem" "redeemer" "red"
 "ready" "rodent" "road hog" "road kill" "road" "Road Block" "road" "reader")
 :case-insensitive t)
 
 ;; >> (("read" ("" "er" "y")) ("red" ("" "eem" "eemer")) ("release" ("")) ("road" ("" "" " block" " hog" " kill")) ("rodent" ("")) ("work" ("" "ed" "er")))
