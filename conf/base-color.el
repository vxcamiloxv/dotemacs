(provide 'base-color)

;; These colors are stolen from Tango.
(defvar naquadah-colors
  '((((class color) (min-colors 65535))
     (aluminium-1 . "#eeeeec")
     (aluminium-2 . "#d3d7cf")
     (aluminium-3 . "#babdb6")
     (aluminium-4 . "#888a85")
     (aluminium-5 . "#555753")
     (aluminium-6 . "#2e3436")
     (butter-1 . "#fce94f")
     (butter-2 . "#edd400")
     (butter-3 . "#c4a000")
     (orange-1 . "#fcaf3e")
     (orange-2 . "#f57900")
     (orange-3 . "#ce5c00")
     (chocolate-1 . "#e9b96e")
     (chocolate-2 . "#c17d11")
     (chocolate-3 . "#9f5902")
     (chameleon-1 . "#8ae234")
     (chameleon-2 . "#73d216")
     (chameleon-3 . "#4e9a06")
     (sky-blue-1 . "#729fcf")
     (sky-blue-2 . "#3465a4")
     (sky-blue-3 . "#204a87")
     (plum-1 . "#ad7fa8")
     (plum-2 . "#75507b")
     (plum-3 . "#5c3566")
     (scarlet-red-1 . "#ef2929")
     (scarlet-red-2 . "#cc0000")
     (scarlet-red-3 . "#a40000")
     (background . "#262B2C")
     (black . "#0c191C")
     (gradient-1 . "#729fcf")  ;; sky-blue-1
     (gradient-2 . "#8ae234")  ;; chameleon-1
     (gradient-3 . "#fce94f")  ;; butter-1
     (gradient-4 . "#ad7fa8")  ;; plum-1
     (gradient-5 . "#e9b96e")  ;; chocolate-1
     (gradient-6 . "#fcaf3e")  ;; orange-1
     (gradient-7 . "#3465a4")  ;; sky-blue-2
     (gradient-8 . "#73d216")  ;; chameleon-2
     (gradient-9 . "#f57900")  ;; orange-2
     (gradient-10 . "#75507b") ;; plum-2
     (gradient-11 . "#c17d11") ;; chocolate-2
     )
    (((class color) (min-colors 256))
     (aluminium-1 . "color-255")
     (aluminium-2 . "color-253")
     (aluminium-3 . "color-251")
     (aluminium-4 . "color-245")
     (aluminium-5 . "color-240")
     (aluminium-6 . "color-235")
     (butter-1 . "color-221")
     (butter-2 . "color-220")
     (butter-3 . "color-178")
     (orange-1 . "color-214")
     (orange-2 . "color-208")
     (orange-3 . "color-130")
     (chocolate-1 . "color-180")
     (chocolate-2 . "color-172")
     (chocolate-3 . "color-94")
     (chameleon-1 . "color-82")
     (chameleon-2 . "color-76")
     (chameleon-3 . "color-34")
     (sky-blue-1 . "color-117")
     (sky-blue-2 . "color-63")
     (sky-blue-3 . "color-24")
     (plum-1 . "color-176")
     (plum-2 . "color-96")
     (plum-3 . "color-54")
     (scarlet-red-1 . "color-196")
     (scarlet-red-2 . "color-160")
     (scarlet-red-3 . "color-124")
     (background . "color-234")
     (black . "color-16")
     (gradient-1 . "color-117")    ;; sky-blue-1
     (gradient-2 . "color-82")     ;; chameleon-1
     (gradient-3 . "color-221")    ;; butter-1
     (gradient-4 . "color-176")    ;; plum-1
     (gradient-5 . "color-180")    ;; chocolate-1
     (gradient-6 . "color-214")    ;; orange-1
     (gradient-7 . "color-63")     ;; sky-blue-2
     (gradient-8 . "color-76")     ;; chameleon-2
     (gradient-9 . "color-208")    ;; orange-2
     (gradient-10 . "color-96")    ;; plum-2
     (gradient-11 . "color-172")   ;; chocolate-2
     )
    (((class color) (min-colors 88))
     (aluminium-1 . "color-87")
     (aluminium-2 . "color-86")
     (aluminium-3 . "color-85")
     (aluminium-4 . "color-84")
     (aluminium-5 . "color-82")
     (aluminium-6 . "color-80")
     (butter-1 . "color-77")
     (butter-2 . "color-76")
     (butter-3 . "color-72")
     (orange-1 . "color-72")
     (orange-2 . "color-68")
     (orange-3 . "color-68")
     (chocolate-1 . "color-73")
     (chocolate-2 . "color-68")
     (chocolate-3 . "color-52")
     (chameleon-1 . "color-60")
     (chameleon-2 . "color-44")
     (chameleon-3 . "color-40")
     (sky-blue-1 . "color-43")
     (sky-blue-2 . "color-22")
     (sky-blue-3 . "color-22")
     (plum-1 . "color-54")
     (plum-2 . "color-37")
     (plum-3 . "color-33")
     (scarlet-red-1 . "color-64")
     (scarlet-red-2 . "color-64")
     (scarlet-red-3 . "color-48")
     (background . "color-80")
     (black . "color-16")
     (gradient-1 . "color-43")    ;; sky-blue-1
     (gradient-2 . "color-60")    ;; chameleon-1
     (gradient-3 . "color-77")    ;; butter-1
     (gradient-4 . "color-54")    ;; plum-1
     (gradient-5 . "color-73")    ;; chocolate-1
     (gradient-6 . "color-72")    ;; orange-1
     (gradient-7 . "color-22")    ;; sky-blue-2
     (gradient-8 . "color-44")    ;; chameleon-2
     (gradient-9 . "color-68")    ;; orange-2
     (gradient-10 . "color-37")   ;; plum-2
     (gradient-11 . "color-68")   ;; chocolate-2
     )
    (t
     (aluminium-1 . "white")
     (aluminium-2 . "white")
     (aluminium-3 . "white")
     (aluminium-4 . "white")
     (aluminium-5 . "white")
     (aluminium-6 . "white")
     (butter-1 . "yellow")
     (butter-2 . "yellow")
     (butter-3 . "yellow")
     (orange-1 . "yellow")
     (orange-2 . "yellow")
     (orange-3 . "yellow")
     (chocolate-1 . "yellow")
     (chocolate-2 . "yellow")
     (chocolate-3 . "yellow")
     (chameleon-1 . "green")
     (chameleon-2 . "green")
     (chameleon-3 . "green")
     (sky-blue-1 . "blue")
     (sky-blue-2 . "blue")
     (sky-blue-3 . "blue")
     (plum-1 . "magenta")
     (plum-2 . "magenta")
     (plum-3 . "magenta")
     (scarlet-red-1 . "red")
     (scarlet-red-2 . "red")
     (scarlet-red-3 . "red")
     (background . "black")
     (black . "black")
     (gradient-1 . "blue")    ;; sky-blue-1
     (gradient-2 . "green")     ;; chameleon-1
     (gradient-3 . "yellow")    ;; butter-1
     (gradient-4 . "yellow")    ;; plum-1
     (gradient-5 . "yellow")    ;; chocolate-1
     (gradient-6 . "yellow")    ;; orange-1
     (gradient-7 . "blue")     ;; sky-blue-2
     (gradient-8 . "green")     ;; chameleon-2
     (gradient-9 . "red")    ;; orange-2
     (gradient-10 . "yellow")    ;; plum-2
     (gradient-11 . "yellow")   ;; chocolate-2
     ))
  "The color values for each color name for a given
      condition.  The format is: ((condition) (key . value) (key
      . value) ...)")
      
(defun naquadah-get-colors (name)
  (cdr
   (assoc
    name
    (car naquadah-colors))))      
