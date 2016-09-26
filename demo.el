(package-demo-define-demo my-package-demo-demo
  (M-x apropos :speed 5)
  (pause 1)
  (kbd "RET"))

(my-package-demo-demo)

(package-demo--run-demo
 '((M-x apropos :speed 5)
   (pause 1)
   (kbd "RET")))

(package-demo-define-demo demo-magit-commit
  (M-x magit-status
       :callback
       ((pause 1)
        (kbd "c")
        (pause 1)
        (kbd "c")
        (pause 1 :callback
               ((typewriter "Initial commit")
                (kbd "C-c C-c"))))))


(package-demo-do
 'M-x
 'magit-status
 :callback
 '((pause 1)
   (kbd "c")
   (pause 1)
   (kbd "c")
   (pause 1 :callback
          ((typewriter "Initial commit")
           (pause 1 :callback ((kbd "C-c C-c")))))))

(package-demo-do 'pause 1)


;;; syntax like this would be nice:

(run ["C-x" 0.5 "b"] ; vectors introduce keystrokes
     1 ; bare numbers always pause
     "typewriter text\n" ; bare strings insert with a typerwriter effect
     1
     (:typerwriter-chars-per-second 20 "this text is inserted much faster\n")
     3
     ["M-x"]
     "save-buffer"
     ["RET"])
