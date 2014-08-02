(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
       (1+ (current-column))))))
             
(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))
        
    (load-library "hideshow")
    (global-set-key (kbd "C-+") 'toggle-hiding)
    (global-set-key (kbd "C-\\") 'toggle-selective-display)
    
    (hs-minor-mode 1)
    
(provide 'conf-hideshow)    
