;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "auto-package-update" "auto-package-update.el"
;;;;;;  (21696 22242 297097 206000))
;;; Generated autoloads from auto-package-update.el

(autoload 'auto-package-update-now "auto-package-update" "\
Update installed Emacs packages.

\(fn)" t nil)

(autoload 'auto-package-update-maybe "auto-package-update" "\
Update installed Emacs packages if needed.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "autosmiley" "autosmiley.el" (22172 17431 757621
;;;;;;  641000))
;;; Generated autoloads from autosmiley.el

(autoload 'autosmiley-mode "autosmiley" "\
Minor mode for automatically replacing smileys in text with
cute little graphical smileys.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "dired-single" "dired-single.el" (21696 22242
;;;;;;  297097 206000))
;;; Generated autoloads from dired-single.el

(autoload 'dired-single-buffer "dired-single" "\
Visit selected directory in current buffer.

Visits the selected directory in the current buffer, replacing the
   current contents with the contents of the new directory.  This doesn't
   prevent you from having more than one dired buffer.  The main difference
   is that a given dired buffer will not spawn off a new buffer every time
   a new directory is visited.

If the variable `dired-single-use-magic-buffer' is non-nil, and the current
   buffer's name is the same as that specified by the variable
`dired-single-magic-buffer-name', then the new directory's buffer will retain
   that same name (i.e. not only will dired only use a single buffer, but
its name will not change every time a new directory is entered).

Optional argument DEFAULT-DIRNAME specifies the directory to visit; if not
specified, the directory or file on the current line is used (assuming it's
a dired buffer).  If the current line represents a file, the file is visited
in another window.

\(fn &optional DEFAULT-DIRNAME)" t nil)

(autoload 'dired-single-buffer-mouse "dired-single" "\
Mouse-initiated version of `dired-single-buffer' (which see).

Argument CLICK is the mouse-click event.

\(fn CLICK)" t nil)

(autoload 'dired-single-magic-buffer "dired-single" "\
Switch to buffer whose name is the value of `dired-single-magic-buffer-name'.

If no such buffer exists, launch dired in a new buffer and rename that buffer
to the value of `dired-single-magic-buffer-name'.  If the current buffer is the
magic buffer, it will prompt for a new directory to visit.

Optional argument DEFAULT-DIRNAME specifies the directory to visit (defaults to
the currently displayed directory).

\(fn &optional DEFAULT-DIRNAME)" t nil)

(autoload 'dired-single-toggle-buffer-name "dired-single" "\
Toggle between the 'magic' buffer name and the 'real' dired buffer name.

Will also seek to uniquify the 'real' buffer name.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "fixme-mode" "fixme-mode.el" (21696 22242 297097
;;;;;;  206000))
;;; Generated autoloads from fixme-mode.el
(defvar fixme-mode nil)

;;;***

;;;### (autoloads nil "frame-cmds" "frame-cmds.el" (21696 22242 297097
;;;;;;  206000))
;;; Generated autoloads from frame-cmds.el

(autoload 'save-frame-config "frame-cmds" "\
Save current frame configuration.
You can restore it with \\[jump-to-frame-config-register].

\(fn)" t nil)

(autoload 'jump-to-frame-config-register "frame-cmds" "\
Restore frame configuration saved in `frame-config-register'.

\(fn)" t nil)

(autoload 'iconify-everything "frame-cmds" "\
Iconify all frames of session at once.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'.

\(fn)" t nil)

(autoload 'hide-everything "frame-cmds" "\
Hide all frames of session at once.
Iconify minibuffer frame; make all others invisible.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'.

\(fn)" t nil)

(autoload 'show-hide "frame-cmds" "\
1 frame visible: `show-hide-show-function'; else: `hide-everything'.
This acts as a toggle between showing all frames and showing only an
iconified minibuffer frame.

\(fn)" t nil)

(autoload 'show-buffer-menu "frame-cmds" "\
Call `buffer-menu' after making all frames visible.
Useful after using `hide-everything' because of a Windows bug that
doesn't let you display frames that have been made visible after
being made invisible.

\(fn)" t nil)

(autoload 'mouse-show-hide-mark-unmark "frame-cmds" "\
In minibuffer: `show-hide'.  In dired: mark/unmark; else: buffer menu.

\(fn EVENT)" t nil)

(autoload 'iconify/map-frame "frame-cmds" "\
Iconify selected frame if now mapped.  Map it if now iconified.
With non-nil prefix arg ICONIFY-ALL, iconify all visible frames.

\(fn &optional ICONIFY-ALL)" t nil)

(autoload 'mouse-iconify/map-frame "frame-cmds" "\
Iconify frame clicked on, if now mapped.  Map it if now iconified.

\(fn EVENT)" t nil)

(autoload 'delete-window "frame-cmds" "\
Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too.

\(fn &optional WINDOW)" t nil)

(autoload 'delete-windows-for "frame-cmds" "\
`delete-window' or prompt for buffer and delete its windows.
With no prefix arg, delete the selected window.
With a prefix arg, prompt for a buffer and delete all windows, on any
  frame, that show that buffer.

\(fn &optional BUFFER)" t nil)

(autoload 'delete-windows-on "frame-cmds" "\
Delete windows showing BUFFER.
Optional arg BUFFER defaults to the current buffer.

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, delete all windows showing BUFFER in any frame.
  If t, delete only windows showing BUFFER in the selected frame.
  If `visible', delete all windows showing BUFFER in any visible frame.
  If a frame, delete only windows showing BUFFER in that frame.

Interactively, FRAME depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME is nil (all frames).
  With prefix arg >= 0, FRAME is t (this frame only).
  With prefix arg < 0,  FRAME is `visible' (all visible frames).

\(fn &optional BUFFER FRAME)" t nil)

(defalias 'remove-window 'delete-window)

(autoload 'remove-windows-on "frame-cmds" "\
Remove all windows showing BUFFER.  This calls `remove-window'
on each window showing BUFFER.

\(fn BUFFER)" t nil)

(autoload 'mouse-remove-window "frame-cmds" "\
Remove the window you click on.  (This calls `remove-window'.)
This command must be bound to a mouse click.

\(fn EVENT)" t nil)

(autoload 'delete/iconify-window "frame-cmds" "\
Delete or iconify WINDOW (default: `selected-window').
If WINDOW is the only one in its frame (`one-window-p'), then optional
arg FRAME-P determines the behavior regarding the frame, as follows:
  If FRAME-P is nil, then the frame is deleted (with the window).
  If FRAME-P is t, then the frame is iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to WINDOW as its only arg.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': the frame is iconified if
             WINDOW is dedicated, otherwise the frame is deleted.

Interactively, FRAME-P depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is t.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted.

\(fn &optional WINDOW FRAME-P)" t nil)

(autoload 'delete/iconify-windows-on "frame-cmds" "\
For each window showing BUFFER: delete it or iconify its frame.
\(This calls `delete/iconify-window' on each window showing BUFFER.)

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, treat all windows showing BUFFER in any frame.
  If t, treat only windows showing BUFFER in the selected frame.
  If `visible', treat all windows showing BUFFER in any visible frame.
  If a frame, treat only windows showing BUFFER in that frame.

Optional third arg FRAME-P controls what to do with one-window frames.
  If FRAME-P is nil, then one-window frames showing BUFFER are deleted.
  If FRAME-P is t, then one-window frames are iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to each window showing buffer in a frame by itself.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': One-window frames are
             iconified if window is dedicated, else they are deleted.

Interactively, FRAME is nil, and FRAME-P depends on the prefix arg:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is t.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted.

\(fn BUFFER &optional FRAME FRAME-P)" t nil)

(autoload 'rename-frame "frame-cmds" "\
Rename a frame named OLD-NAME to NEW-NAME.
Prefix arg ALL-NAMED non-nil means rename all frames named FRAME to NEWNAME.

OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.

NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'.

\(fn &optional OLD-NAME NEW-NAME ALL-NAMED)" t nil)

(autoload 'rename-non-minibuffer-frame "frame-cmds" "\
Unless OLD-NAME names the minibuffer frame, use `rename-frame'
to rename a frame named OLD-NAME to NEW-NAME.

Prefix arg ALL-NAMED non-nil => Rename all frames named FRAME to NEWNAME.
OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.
NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'.

\(fn &optional OLD-NAME NEW-NAME ALL-NAMED)" t nil)

(autoload 'show-frame "frame-cmds" "\
Make FRAME visible and raise it, without selecting it.
FRAME may be a frame or its name.

\(fn FRAME)" t nil)

(autoload 'hide-frame "frame-cmds" "\
Make FRAME invisible.  Like `make-frame-invisible', but reads frame name.
Non-nil PREFIX makes it invisible even if all other frames are invisible.

\(fn FRAME &optional PREFIX)" t nil)

(autoload 'show-a-frame-on "frame-cmds" "\
Make visible and raise a frame showing BUFFER, if there is one.
Neither the frame nor the BUFFER are selected.
BUFFER may be a buffer or its name (a string).

\(fn BUFFER)" t nil)

(autoload 'show-*Help*-buffer "frame-cmds" "\
Raise a frame showing buffer *Help*, without selecting it.

\(fn)" t nil)

(autoload 'delete-1-window-frames-on "frame-cmds" "\
Delete all visible 1-window frames showing BUFFER.

\(fn BUFFER)" t nil)

(autoload 'delete-other-frames "frame-cmds" "\
Delete all frames except FRAME (default: selected frame).
Interactively, use a prefix arg (`\\[universal-argument]') to be prompted for FRAME.

\(fn &optional FRAME)" t nil)

(autoload 'maximize-frame-horizontally "frame-cmds" "\
Maximize selected frame horizontally.

\(fn &optional FRAME)" t nil)

(autoload 'maximize-frame-vertically "frame-cmds" "\
Maximize selected frame vertically.

\(fn &optional FRAME)" t nil)

(autoload 'maximize-frame "frame-cmds" "\
Maximize selected frame horizontally, vertically, or both.
With no prefix arg, maximize both directions.
With a non-negative prefix arg, maximize vertically.
With a negative prefix arg, maximize horizontally.

In Lisp code:
 DIRECTION is the direction: `horizontal', `vertical', or `both'.
 FRAME is the frame to maximize.

\(fn &optional DIRECTION FRAME)" t nil)

(defalias 'restore-frame-horizontally 'toggle-max-frame-horizontally)

(autoload 'toggle-max-frame-horizontally "frame-cmds" "\
Toggle maximization of FRAME horizontally.
If used once, this restores the frame.  If repeated, it maximizes.
This affects the `left' and `width' frame parameters.

FRAME defaults to the selected frame.

\(fn &optional FRAME)" t nil)

(defalias 'restore-frame-horizontally 'toggle-max-frame-horizontally)

(autoload 'toggle-max-frame-vertically "frame-cmds" "\
Toggle maximization of FRAME vertically.
If used once, this restores the frame.  If repeated, it maximizes.
This affects the `top' and `height' frame parameters.

FRAME defaults to the selected frame.

\(fn &optional FRAME)" t nil)

(defalias 'restore-frame 'toggle-max-frame)

(autoload 'toggle-max-frame "frame-cmds" "\
Toggle maximization of FRAME horizontally, vertically, or both.
Reverses or (if restored) repeats the effect of the Emacs maximize
commands.  Does not restore from maximization effected outside Emacs.

With no prefix arg, toggle both directions.
With a non-negative prefix arg, toggle only vertically.
With a negative prefix arg, toggle horizontally.

When toggling both, each is toggled from its last maximize or restore
state.  This means that using this after `maximize-horizontal',
`maximize-vertical', `toggle-max-horizontal', or `toggle-max-vertical'
does not necessarily just reverse the effect of that command.

In Lisp code:
 DIRECTION is the direction: `horizontal', `vertical', or `both'.
 FRAME is the frame to change.  It defaults to the selected frame.

\(fn &optional DIRECTION FRAME)" t nil)

(autoload 'tile-frames-horizontally "frame-cmds" "\
Tile frames horizontally.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled.

\(fn &optional FRAMES)" t nil)

(autoload 'tile-frames-vertically "frame-cmds" "\
Tile frames vertically.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled.

\(fn &optional FRAMES)" t nil)

(autoload 'enlarge-frame "frame-cmds" "\
Increase the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'enlarge-frame-horizontally "frame-cmds" "\
Increase the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'shrink-frame "frame-cmds" "\
Decrease the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'shrink-frame-horizontally "frame-cmds" "\
Decrease the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'move-frame-down "frame-cmds" "\
Move FRAME (default: selected-frame) down by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'move-frame-up "frame-cmds" "\
Move FRAME (default: selected-frame) up by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'move-frame-right "frame-cmds" "\
Move FRAME (default: selected-frame) toward the right by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'move-frame-left "frame-cmds" "\
Move FRAME (default: selected-frame) toward the left by INCREMENT.
INCREMENT is in units of ten pixels.
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'enlarge-font "frame-cmds" "\
Increase size of font in FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'set-frame-alist-parameter-from-frame "frame-cmds" "\
Set PARAMETER of frame alist ALIST to its current value in FRAME.
FRAME defaults to the selected frame.  ALIST is a variable (symbol)
whose value is an alist of frame parameters.

\(fn ALIST PARAMETER &optional FRAME)" t nil)

(autoload 'set-all-frame-alist-parameters-from-frame "frame-cmds" "\
Set frame parameters of ALIST to their current values in FRAME.
Unless optional argument REALLY-ALL-P (prefix arg) is non-nil, the
frame parameters in list `frame-parameters-to-exclude' are
excluded: they are not copied from FRAME to ALIST.
ALIST is a variable (symbol) whose value is an alist of frame parameters.
FRAME defaults to the selected frame.

\(fn ALIST &optional FRAME REALLY-ALL-P)" t nil)

(autoload 'tell-customize-var-has-changed "frame-cmds" "\
Tell Customize to recognize that VARIABLE has been set (changed).
VARIABLE is a symbol that names a user option.

\(fn VARIABLE)" t nil)

(autoload 'other-window-or-frame "frame-cmds" "\
`other-frame', if `one-window-p'; otherwise, `other-window'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "indent-hint" "indent-hint.el" (21696 22242
;;;;;;  297097 206000))
;;; Generated autoloads from indent-hint.el

(autoload 'indent-hint-lisp "indent-hint" "\


\(fn)" t nil)

(autoload 'indent-hint-fixed "indent-hint" "\


\(fn &optional IMG)" t nil)

;;;***

;;;### (autoloads nil "longlines" "longlines.el" (21696 22242 297097
;;;;;;  206000))
;;; Generated autoloads from longlines.el

(autoload 'longlines-mode "longlines" "\
Toggle Long Lines mode.
In Long Lines mode, long lines are wrapped if they extend beyond
`fill-column'.  The soft newlines used for line wrapping will not
show up when the text is yanked or saved to disk.

If the variable `longlines-auto-wrap' is non-nil, lines are automatically
wrapped whenever the buffer is changed.  You can always call
`fill-paragraph' to fill individual paragraphs.

If the variable `longlines-show-hard-newlines' is non-nil, hard newlines
are indicated with a symbol.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "menu-bar+" "menu-bar+.el" (21696 22242 297097
;;;;;;  206000))
;;; Generated autoloads from menu-bar+.el

(autoload 'describe-menubar "menu-bar+" "\
Explain the menu bar, in general terms.

\(fn)" t nil)

(autoload 'menu-bar-create-directory "menu-bar+" "\
Create a subdirectory of `default-directory' called DIRECTORY.

\(fn DIRECTORY)" t nil)

(autoload 'kill-this-buffer "menu-bar+" "\
Delete the current buffer and delete all of its windows.

\(fn)" t nil)

(autoload 'menu-bar-next-tag-other-window "menu-bar+" "\
Find the next definition of the tag already specified.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "move-dup" "move-dup.el" (21696 22242 297097
;;;;;;  206000))
;;; Generated autoloads from move-dup.el

(autoload 'md/move-region "move-dup" "\
Interactive function to move the current selection N lines.

If the selection is not a rectangle, this function will expand
the selection to a rectangle via the function
`md/ensure-rectangle' and move it accordingly.  If the prefix N
is positive, this function moves the rectangle forward N lines;
otherwise backward.

\(fn &optional N)" t nil)

(autoload 'md/move-line "move-dup" "\
Interactive function to move the current line N line.

If the prefix N is positive, this function moves the current line
forward N lines; otherwise backward.

\(fn &optional N)" t nil)

(autoload 'md/move-lines-up "move-dup" "\
Interactive function to move the current line or selection up.

If the prefix N is positive, this function moves the current line
or selection up N lines; otherwise down.

\(fn &optional N)" t nil)

(autoload 'md/move-lines-down "move-dup" "\
Interactive function to move the current line or selection down.

If the prefix N is positive, this function moves the current line
or selection down N lines; otherwise up.

\(fn &optional N)" t nil)

(autoload 'md/duplicate-up "move-dup" "\
Interactive function to duplicate the current line or selection upward.

If the prefix N is positive, this function makes N duplicates of
the current line or selection and place them above the current
line or selection.

\(fn &optional N)" t nil)

(autoload 'md/duplicate-down "move-dup" "\
Interactive function to duplicate the current line or selection downward.

If the prefix N is positive, this function makes N duplicates of
the current line or selection and place them below the current
line or selection.

\(fn &optional N)" t nil)

(autoload 'move-dup-mode "move-dup" "\
Minor mode for Eclipse-like moving and duplicating lines or
rectangles with default key bindings.

The default key bindings are:

\([M-up] . md/move-lines-up)
\([M-down] . md/move-lines-down)
\([C-M-up] . md/duplicate-up)
\([C-M-down] . md/duplicate-down)

\(fn &optional ARG)" t nil)

(defvar global-move-dup-mode nil "\
Non-nil if Global-Move-Dup mode is enabled.
See the command `global-move-dup-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-move-dup-mode'.")

(custom-autoload 'global-move-dup-mode "move-dup" nil)

(autoload 'global-move-dup-mode "move-dup" "\
Toggle Move-Dup mode in all buffers.
With prefix ARG, enable Global-Move-Dup mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Move-Dup mode is enabled in all buffers where
`move-dup-on' would do it.
See `move-dup-mode' for more information on Move-Dup mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "moz" "moz.el" (21696 22242 297097 206000))
;;; Generated autoloads from moz.el

(autoload 'moz-minor-mode "moz" "\
MozRepl minor mode for interaction with Firefox.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this minor mode is enabled, some commands become available
to send current code area (as understood by c-mark-function) or
region or buffer to an inferior MozRepl process (which will be
started as needed).

The following keys are bound in this minor mode:

\\{moz-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'inferior-moz-mode "moz" "\
Major mode for interacting with Firefox via MozRepl.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mwe-color-box" "mwe-color-box.el" (21696 22242
;;;;;;  297097 206000))
;;; Generated autoloads from mwe-color-box.el

(autoload 'mwe:color-box-region "mwe-color-box" "\
Create nested color boxes for region BEG to END.
If positive number, RMARGIN sets right margin of color boxes to column RMARGIN.
If Non-nil, REG-TOK-FN sets the tokenizer.  If nil, uses `mwe:tokenize-region'.

Calls `mwe:color-box-color' with argument DEPTH to pick color.

\(fn BEG END &optional RMARGIN REG-TOK-FN)" t nil)

(autoload 'mwe:color-box-buffer "mwe-color-box" "\
Create nested color boxes for buffer.
See also `mwe:color-box-region'.

\(fn &optional BUF)" t nil)

(autoload 'mwe:color-box-region/miscbill "mwe-color-box" "\


\(fn BEG END &optional RMARGIN)" t nil)

;;;***

;;;### (autoloads nil "single-dired" "single-dired.el" (21696 22242
;;;;;;  297097 206000))
;;; Generated autoloads from single-dired.el

(autoload 'dired-mouse-find-alternate-file "single-dired" "\
In dired, visit the file or directory you click on instead of the dired buffer.

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads nil "sr-speedbar" "sr-speedbar.el" (21696 22242
;;;;;;  297097 206000))
;;; Generated autoloads from sr-speedbar.el

(autoload 'sr-speedbar-toggle "sr-speedbar" "\
Toggle sr-speedbar window.
Toggle visibility of sr-speedbar by resizing
the `sr-speedbar-window' to a minimal width
or the last width when visible.
Use this function to create or toggle visibility
of a speedbar-window.  It will be created if necessary.

\(fn)" t nil)

(autoload 'sr-speedbar-open "sr-speedbar" "\
Create `sr-speedbar' window.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "undo-tree" "undo-tree.el" (21696 22242 297097
;;;;;;  206000))
;;; Generated autoloads from undo-tree.el

(autoload 'undo-tree-mode "undo-tree" "\
Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-undo-tree-mode nil "\
Non-nil if Global-Undo-Tree mode is enabled.
See the command `global-undo-tree-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.")

(custom-autoload 'global-undo-tree-mode "undo-tree" nil)

(autoload 'global-undo-tree-mode "undo-tree" "\
Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global-Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "windata" "windata.el" (21696 22242 297097
;;;;;;  206000))
;;; Generated autoloads from windata.el

(autoload 'windata-name-winconf "windata" "\
Save window configuration with NAME.
After save the window configuration you can restore it by NAME using
`windata-restore-named-winconf'.

\(fn NAME)" t nil)

(autoload 'windata-restore-named-winconf "windata" "\
Restore saved window configuration.

\(fn NAME)" t nil)

(autoload 'windata-display-buffer "windata" "\
Display buffer more precisely.
FRAME-P is non-nil and not window, the displayed buffer affect
the whole frame, that is to say, if DIR is right or left, the
displayed buffer will show on the right or left in the frame. If
it is nil, the buf will share space with current window.

DIR can be one of member of (right left top bottom).

SIZE is the displayed windowed size in width(if DIR is left or
right) or height(DIR is top or bottom). It can be a decimal which
will stand for percentage of window(frame) width(heigth)

DELETE-P is non-nil, the other window will be deleted before
display the BUF.

\(fn BUF FRAME-P DIR SIZE &optional DELETE-P)" nil nil)

;;;***

;;;### (autoloads nil "zoom-frm" "zoom-frm.el" (21696 22242 297097
;;;;;;  206000))
;;; Generated autoloads from zoom-frm.el

(let ((loads (get 'zoom 'custom-loads))) (if (member '"zoom-frm" loads) nil (put 'zoom 'custom-loads (cons '"zoom-frm" loads))))

(defvar frame-zoom-font-difference 1 "\
*Number of points to change the frame font size when zooming
using commands `zoom-frm-in' and `zoom-frm-out'.
The absolute value of this must be less than the current font size,
since the new font size cannot be less than 1 point.")

(custom-autoload 'frame-zoom-font-difference "zoom-frm" t)

(autoload 'zoom-frm-in "zoom-frm" "\
Zoom FRAME in by `frame-zoom-font-difference', making text larger.
If `frame-zoom-font-difference' is negative, make text smaller.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, then make text smaller.
This is equal but opposite to `zoom-frm-out'.

\(fn &optional FRAME FLIP)" t nil)

(autoload 'zoom-frm-out "zoom-frm" "\
Zoom FRAME out by `frame-zoom-font-difference'.
If `frame-zoom-font-difference' is negative, make text larger.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, then make text larger.
This is equal but opposite to `zoom-frm-in'.

\(fn &optional FRAME FLIP)" t nil)

(autoload 'zoom-frm-unzoom "zoom-frm" "\
Cancel zoom of FRAME.

\(fn &optional FRAME)" t nil)

(autoload 'toggle-zoom-frame "zoom-frm" "\
Alternately zoom/unzoom FRAME by `frame-zoom-font-difference'.

\(fn &optional FRAME)" t nil)

;;;***

;;;### (autoloads nil nil ("context-menu.el" "frame-fns.el" "fsdired.el"
;;;;;;  "hide-mode-line.el" "ide-skel.el" "org-archive-subtree-hierarchical.el"
;;;;;;  "org-license.el" "org-loaddefs.el" "ox-gfm.el" "scroll-bell-fix.el"
;;;;;;  "smooth-scrolling.el" "squeeze-view.el") (22177 45020 518433
;;;;;;  939000))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
